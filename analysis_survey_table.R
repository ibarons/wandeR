################################################################################
## @Title: analysis_survey_table.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 26.07.2020
## @Last updated: 11.03.2021
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes:
##
## Low count need validation and further cross-check
##
## Adjust `.admins` argument to take a regex to specify admin levels. Further wrap
## it in a purrr::pmap to apply the specified .admins and generate a list fo tables
## for each given admin. This will make the function more efficient and neat, rather
## than specifying each admin in a in if statement as it is now
##
## ----------------------------------------------------------------------------
#' @name SurveyTbl
#' @title Tabulate Survey data, including SE
#'
#' @description Produce tables with survey data for numeric and categorical data.
#' All including weighted and unweighted outputs and SE.
#'
#' @usage
#' SurveyTbl(db, ..., .num = NULL, .admins = NULL, .extra = NULL, .median = FALSE,
#'  .q_method = "linear", .error = NULL, .low.count = FALSE, .name = "a_table")
#' 
#' @param db  object of class `survey.design` containing the variable to analyze
#' @param ... variables to analyze (do not enter as character "")
#' @param .num optional to enter a numeric variable (e.g. population) to perform
#' the calculationw with. Instead of counts and percentages, will return sum and 
#' average for the given numeric variable. (see detail when input is a binary 
#' numeric)
#' @param .admins a character vector to indicate the requered admin level
#' disaggregated. Take `c("aoc", "admin1", "admin3"). If NULL take only National
#' @param .extra to add an extra group for disaggregation, aside of .admins.
#' @param .median logical if median need to be calculated
#' @param .q_method passed to `srvyr::survey_median()`. Choose between "linear" 
#' or "constant"
#' @param .error measure to indicate the error of the estimate: c("se", "ci", 
#' "var", "cv"). `NULL` if no error measure desired. Passed to `srvyr::survey_sum()`
#' @param .low.count Logical if a check for low expected count cell should be done.
#' Base on SPSS check for contingency tables. Only available when at least two 
#' vars passed to `...`
#' @param .name string indicating the title of the table, which will appear
#' in the first column
#'
#' @details 
#' The function has Governorate, AoC and Sub-district dis-aggregation level (each
#'  of them must be named as [admin1(Pcode|Name), AoC and admin3(Pcode|Name)] by
#'  default. With the `.extra` argument an additional dis-aggregation  level can
#'  be provided. 
#' 
#' If the numeric variable `.num` is binary 1/0, then it will be equivalent as 
#' to count and percentages, and column names will be adjusted accordingly to 
#' `coun` and `perc`, instead of `sum` and `avg` (This not sure it works...)
#'
#' It computes column percentages for the latest variable inputted in the ...
#' Thus the order of variables is not trivial: dependent variable should be the 
#' last with independents before to provide column % for the dependent
#' 
#' `.error` argument strings stands for: "se" for Standard Error; "ci" for confidence
#'  interval (lower and upper); "var" for variance; "cv" for coefficient of variation.
#'  More details in `?survey_count(vartype = c(...))`
#'  
#'  `low.count` check if the percentages of cell with expected count < 5 is greater
#'  than 20%: returns TRUE if so. Results are based on non-empty rows and columns
#'  in each innermost subtable. Formula for expected count is:
#'  
#'  $$e_ij = i * j / n_ij$$
#' 
#' @return A tibble with the relevant statistics for the given type of variable
#' for each `.admins` defined for disaggreagation.
#'
#' @examples
#'
#' db <- data.frame(
#'   pop = sample(50:10000, 100),
#'   sex = sample(c("male", "female"), 100, replace = TRUE),
#'   age = sample(c("<18", "18-44", "45-64", ">=65"), 100, replace = TRUE),
#'   grade = sample(LETTERS[1:4], 100, replace = TRUE),
#'   AoC = sample(c("aoc1", "aoc2", "aoc3"), 100, replace = TRUE),
#'   admin1Name = sample(c("SP", "PH"), 100, replace = TRUE),
#'   admin3Name = sample(c("Madrid", "Manila", "Sevilla", "Cotabato"), 100, replace = TRUE)
#' )
#' 
#' db["admin1Pcode"] <- ifelse(db$admin1Name == "SP", 1, 2)
#' db["admin3Pcode"] <- ifelse(db$admin3Name == "Madrid", 10,
#'   ifelse(db$admin3Name == "Manila", 20,
#'     ifelse(db$admin3Name == "Sevilla", 11, 21)
#'   )
#' )
#' 
#' library(dplyr)
#' db <- srvyr::as_survey_design(db) %>%
#'   dplyr::mutate(dplyr::across(
#'     c(sex, age, grade, AoC, dplyr::matches("^admin")),
#'     as.factor
#'   ))
#'
#' # By one single variable
#' SurveyTbl(db, sex)
#'
#' # Averages by single variable for National level
#' SurveyTbl(db, sex, .num = "pop", .admins = NULL)
#'
#' # By multiple variables, and include all admins
#' SurveyTbl(db, age, sex, .admins = c("aoc", "admin1", "admin3"))
#'
#' # By multiple variables, and include extra variable
#' SurveyTbl(db, age, sex, .extra = "grade")
#'
#' @export
#'
## -----------------------------------------------------------------------------

SurveyTbl <- function(db, ..., .num = NULL, .admins = NULL, .extra = NULL,
                      .median = FALSE, .q_method = "linear", .error = NULL,
                      .low.count = FALSE, .name = "a_table") {
  
  ############################ PREELIMINARIES ##################################

  list.df <- c()
  num.sym <- rlang::syms(.num)
  extra.sym <- rlang::syms(.extra)
  dots <- rlang::enquos(...)

  start <- Sys.time()
  cat(
    "\nAnalysis started:", as.character(start), "\n",
    "variables:", gsub("~|\\s", "", as.character(unlist(dots))), # removed rlang::list2() before as.character(unlist(rlang::list2(dots)))), which was useless
    .num, "\n"
  )
  
  db <- dplyr::mutate(db, all = 1) # create column all for num analysis with all cases

  ########################### ESTIMATES FOR ROW ################################

  # National row only
  list.df[["row.nat"]] <- db %>%
    dplyr::group_by(., ...) %>%
    {
      if (length(.num) == 0) {
        dplyr::summarise(.,
          coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
          perc.we = srvyr::survey_mean(vartype = .error, na.rm = TRUE),
          coun.un = srvyr::unweighted(dplyr::n()),
          .groups = "drop"
        )
      } else if (.median == TRUE) {
        dplyr::summarise(.,
          avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
          med.we = as.numeric(purrr::possibly(srvyr::survey_median, NA_real_)( # gives error when no variance. purrr::possibly() to continue the function
            !!!num.sym, vartype = .error, q_method = .q_method, na.rm = TRUE
          )), 
          #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
          coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
          coun.un = srvyr::unweighted(dplyr::n()),
          .groups = "drop"
        )
      } else {
        dplyr::summarise(.,
           avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
           #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
           coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
           coun.un = srvyr::unweighted(dplyr::n()),
           .groups = "drop"
        )
      }
    } %>%
    dplyr::mutate(group = "Total", adminName = "National") %>%
    {
      if (.low.count == TRUE & length(dots) > 2) {
        
        dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
          dplyr::mutate(temp.exp.total = sum(coun.un)) %>%
          dplyr::add_count(
            !!!dots[ 1:(length(dots) - 1) ], wt = coun.un, name = "temp.exp.row"
          ) %>% 
          dplyr::add_count(
            !!!dots[c(1:(length(dots) - 2), length(dots))], wt = coun.un, name = "temp.exp.col"
          ) %>% 
          dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
          dplyr::mutate(
            temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
            low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
          ) %>%
          dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
        
      } else if (.low.count == TRUE & length(dots) == 2) {
       
        dplyr::mutate(., temp.exp.total = sum(coun.un)) %>%
          dplyr::add_count(!!dots[[1]], wt = coun.un, name = "temp.exp.row") %>% 
          dplyr::add_count(!!dots[[2]], wt = coun.un, name = "temp.exp.col") %>% 
          dplyr::mutate(
            temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
            low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
          ) %>%
          dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
        
      } else if (.low.count == FALSE | length(dots) == 1) {.} 
    } %>% 
    dplyr::ungroup()


  # AoC row only
  if("aoc" %in% .admins) {
    
    list.df[["row.geo"]] <- db %>%
      dplyr::group_by(., AoC, ...) %>%
      {
        if (length(.num) == 0) {
          dplyr::summarise(.,
            coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
            perc.we = srvyr::survey_mean(vartype = .error, na.rm = TRUE),
            coun.un = srvyr::unweighted(dplyr::n()),
            .groups = "drop"
          )
        } else if (.median == TRUE) {
          dplyr::summarise(.,
             avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
             med.we = as.numeric(purrr::possibly(srvyr::survey_median, NA_real_)( # gives error when no variance (e.g. 1 non-na case only). purrr::possibly() to continue the function
               !!!num.sym, vartype = .error, q_method = .q_method, na.rm = TRUE
             )),
             #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
             coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
             coun.un = srvyr::unweighted(dplyr::n()),
             .groups = "drop"
          )
        } else {
          dplyr::summarise(.,
             avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
             #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
             coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
             coun.un = srvyr::unweighted(dplyr::n()),
             .groups = "drop"
          )
        }
      } %>%
      dplyr::mutate(group = "AoC") %>%
      dplyr::rename("adminName" = AoC) %>%
      {
        if (.low.count == TRUE & length(dots) > 2) {
          
          dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
            dplyr::mutate(temp.exp.total = sum(coun.un)) %>%
            dplyr::add_count(
              !!!dots[ 1:(length(dots) - 1) ], wt = coun.un, name = "temp.exp.row"
            ) %>% 
            dplyr::add_count(
              !!!dots[c(1:(length(dots) - 2), length(dots))], wt = coun.un, name = "temp.exp.col"
            ) %>% 
            dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
            dplyr::mutate(
              temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
              low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
            ) %>%
            dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
          
        } else if (.low.count == TRUE & length(dots) == 2) {
         
          dplyr::mutate(., temp.exp.total = sum(coun.un)) %>%
            dplyr::add_count(!!dots[[1]], wt = coun.un, name = "temp.exp.row") %>% 
            dplyr::add_count(!!dots[[2]], wt = coun.un, name = "temp.exp.col") %>% 
            dplyr::mutate(
              temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
              low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
            ) %>%
            dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
          
        } else if (.low.count == FALSE | length(dots) == 1) {.} 
      } %>% 
      dplyr::ungroup()
  }

  
  # Governorate row only
  if ("admin1" %in% .admins) {
    
    list.df[["row.gov"]] <- db %>%
      dplyr::group_by(admin1Pcode, admin1Name, ...) %>%
      {
        if (length(.num) == 0) {
          dplyr::summarise(.,
            coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
            perc.we = srvyr::survey_mean(vartype = .error, na.rm = TRUE),
            coun.un = srvyr::unweighted(dplyr::n()),
            .groups = "drop"
          )
        } else if (.median == TRUE) {
          dplyr::summarise(.,
             avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
             med.we = as.numeric(purrr::possibly(srvyr::survey_median, NA_real_)( # gives error when no variance. purrr::possibly() to continue the function
               !!!num.sym, vartype = .error, q_method = .q_method, na.rm = TRUE
             )),
             #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
             coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
             coun.un = srvyr::unweighted(dplyr::n()),
             .groups = "drop"
          )
        } else {
          dplyr::summarise(.,
             avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
             #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
             coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
             coun.un = srvyr::unweighted(dplyr::n()),
             .groups = "drop"
          )
        }
      } %>%
      dplyr::mutate(group = "governorate") %>%
      dplyr::rename_with(~"adminName", dplyr::matches("admin\\dName")) %>%
      dplyr::rename_with(~"adminPcode", dplyr::matches("admin\\dPcode")) %>%
      dplyr::arrange(adminName) %>%
      {
        if (.low.count == TRUE & length(dots) > 2) {
          
          dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
            dplyr::mutate(temp.exp.total = sum(coun.un)) %>%
            dplyr::add_count(
              !!!dots[ 1:(length(dots) - 1) ], wt = coun.un, name = "temp.exp.row"
            ) %>% 
            dplyr::add_count(
              !!!dots[c(1:(length(dots) - 2), length(dots))], wt = coun.un, name = "temp.exp.col"
            ) %>% 
            dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
            dplyr::mutate(
              temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
              low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
            ) %>%
            dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
          
        } else if (.low.count == TRUE & length(dots) == 2) {
          
          dplyr::mutate(., temp.exp.total = sum(coun.un)) %>%
            dplyr::add_count(!!dots[[1]], wt = coun.un, name = "temp.exp.row") %>% 
            dplyr::add_count(!!dots[[2]], wt = coun.un, name = "temp.exp.col") %>% 
            dplyr::mutate(
              temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
              low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
            ) %>%
            dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
          
        } else if (.low.count == FALSE | length(dots) == 1) {.} 
      } %>% 
      dplyr::ungroup()
  }


  # Extra group row only
  if (!is.null(.extra)) {
    
    list.df[["row.extra"]] <- db %>%
      dplyr::group_by(!!!extra.sym, ...) %>%
      {
        if (length(.num) == 0) {
          dplyr::summarise(.,
            coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
            perc.we = srvyr::survey_mean(vartype = .error, na.rm = TRUE),
            coun.un = srvyr::unweighted(dplyr::n()),
            .groups = "drop"
          )
        } else if (.median == TRUE) {
          dplyr::summarise(.,
             avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
             med.we = as.numeric(purrr::possibly(srvyr::survey_median, NA_real_)( # gives error when no variance. purrr::possibly() to continue the function
               !!!num.sym, vartype = .error, q_method = .q_method, na.rm = TRUE
             )), 
             #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
             coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
             coun.un = srvyr::unweighted(dplyr::n()),
             .groups = "drop"
          )
        } else {
          dplyr::summarise(.,
             avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
             #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
             coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
             coun.un = srvyr::unweighted(dplyr::n()),
             .groups = "drop"
          )
        }
      } %>%
      dplyr::mutate(group = .extra) %>%
      dplyr::rename("adminName" = {{.extra }}) %>%
      {
        if (.low.count == TRUE & length(dots) > 2) {
          
          dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
            dplyr::mutate(temp.exp.total = sum(coun.un)) %>%
            dplyr::add_count(
              !!!dots[ 1:(length(dots) - 1) ], wt = coun.un, name = "temp.exp.row"
            ) %>% 
            dplyr::add_count(
              !!!dots[c(1:(length(dots) - 2), length(dots))], wt = coun.un, name = "temp.exp.col"
            ) %>% 
            dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
            dplyr::mutate(
              temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
              low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
            ) %>%
            dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
          
        } else if (.low.count == TRUE & length(dots) == 2) {
          
          dplyr::mutate(., temp.exp.total = sum(coun.un)) %>%
            dplyr::add_count(!!dots[[1]], wt = coun.un, name = "temp.exp.row") %>% 
            dplyr::add_count(!!dots[[2]], wt = coun.un, name = "temp.exp.col") %>% 
            dplyr::mutate(
              temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
              low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
            ) %>%
            dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
          
        } else if (.low.count == FALSE | length(dots) == 1) {.} 
      } %>% 
      dplyr::ungroup()
  }


  # Sub-distric row only
  if ("admin3" %in% .admins) {
    
    list.df[["row.sub"]] <- db %>%
      #dplyr::mutate(admin3Name = paste0(admin1Name, " - ", admin3Name)) %>%
      dplyr::group_by(admin1Pcode, admin1Name, admin3Pcode, admin3Name, ...) %>%
      {
        if (length(.num) == 0) {
          dplyr::summarise(.,
            AoC = AoC[1],
            coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
            perc.we = srvyr::survey_mean(vartype = .error, na.rm = TRUE),
            coun.un = srvyr::unweighted(dplyr::n()),
            .groups = "drop"
          )
        } else if (.median == TRUE) {
          dplyr::summarise(.,
             AoC = AoC[1],
             avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
             med.we = as.numeric(purrr::possibly(srvyr::survey_median, NA_real_)( # gives error when no variance. purrr::possibly() to continue the function
               !!!num.sym, vartype = .error, q_method = .q_method, na.rm = TRUE
             )),
             #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
             coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
             coun.un = srvyr::unweighted(dplyr::n()),
             .groups = "drop"
          )
        } else {
          dplyr::summarise(.,
             AoC = AoC[1],
             avg.we = srvyr::survey_mean(!!!num.sym, vartype = .error, na.rm = TRUE),
             #sum.we = srvyr::survey_total(!!!num.sym, vartype = NULL, na.rm = TRUE),
             coun.we = srvyr::survey_total(vartype = NULL, na.rm = TRUE),
             coun.un = srvyr::unweighted(dplyr::n()),
             .groups = "drop"
          )
        }
      } %>%
      dplyr::mutate(group = "subdistrict") %>%
      dplyr::rename_with(~ "adminName", dplyr::matches("admin[3]Name")) %>%
      dplyr::rename_with(~ "adminPcode", dplyr::matches("admin[3]Pcode")) %>%
      dplyr::arrange(adminName) %>%
      {
        if (.low.count == TRUE & length(dots) > 2) {
          
          dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
            dplyr::mutate(temp.exp.total = sum(coun.un)) %>%
            dplyr::add_count(
              !!!dots[ 1:(length(dots) - 1) ], wt = coun.un, name = "temp.exp.row"
            ) %>% 
            dplyr::add_count(
              !!!dots[c(1:(length(dots) - 2), length(dots))], wt = coun.un, name = "temp.exp.col"
            ) %>% 
            dplyr::group_by(., !!!dots[ 1:(length(dots) - 2) ]) %>%
            dplyr::mutate(
              temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
              low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
            ) %>%
            dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
          
        } else if (.low.count == TRUE & length(dots) == 2) {
          
          dplyr::mutate(., temp.exp.total = sum(coun.un)) %>%
            dplyr::add_count(!!dots[[1]], wt = coun.un, name = "temp.exp.row") %>% 
            dplyr::add_count(!!dots[[2]], wt = coun.un, name = "temp.exp.col") %>% 
            dplyr::mutate(
              temp.exp.count = (temp.exp.row * temp.exp.col) / temp.exp.total,
              low.count = (sum(temp.exp.count < 5) / dplyr::n()) > 0.2 | (any(temp.exp.count == 0))
            ) %>%
            dplyr::select(-dplyr::matches("^temp\\.exp\\.(row|col|total|count)$"))
          
        } else if (.low.count == FALSE | length(dots) == 1) {.} 
      } %>% 
      dplyr::ungroup()
    
  }

  # Bind, formast and out
  tb.out <- dplyr::bind_rows(list.df) %>%
    dplyr::mutate(tbl.name = .name, .before = 1) %>%
      #dplyr::across(dplyr::matches("^coun.*$"), ~ round(., 0)),
      # dplyr::across(
      #   c(dplyr::matches("^(count|perc|sum|avg|med)\\.we$"),
      #     dplyr::matches("^(count|perc|sum|avg|med)\\.we_se$"),
      #     dplyr::matches("^(coun|sum)\\.un$")
      #   ), ~ tidyr::replace_na(., 0)
      # ),
    {
      if (length(.num) == 0) {
        dplyr::select(
          .,
          tbl.name, dplyr::matches("^group$"), adminName, ...,
          dplyr::matches("^coun\\.we$"), dplyr::matches("^perc\\.we$"),
          dplyr::matches("^perc\\.we_se$"), dplyr::matches("^coun\\.un$"),
          dplyr::everything(), dplyr::matches("^adminPcode$"), dplyr::matches("^admin\\d[NP]")
        )
      } else {
        dplyr::select(
          .,
          tbl.name, dplyr::matches("^group$"), adminName, ...,
          dplyr::matches("^sum\\.we$"), 
          dplyr::matches("^avg\\.we$"), dplyr::matches("^avg\\.we_se$"), 
          dplyr::matches("^med\\.we$"), dplyr::matches("^med\\.we_se$"),
          dplyr::matches("^sum\\.un$"), dplyr::everything()
        ) %>%
          dplyr::relocate(
            dplyr::matches("^adminPcode$"), dplyr::matches("admin\\d[NP]|AoC"),
            .after = dplyr::last_col()
          ) %>%
        {
          if(all(db[["variables"]][[.num]] %in% c(1,0, NA, ""))) {
            dplyr::rename_with(.,
             ~ gsub("(^sum)(.*$)", "coun\\2", .), 
             dplyr::matches("^sum\\.[wu][en].*$")
            ) %>%
              dplyr::rename_with(
                ~ gsub("(^avg)(.*$)", "perc\\2", .), 
                dplyr::matches("^avg\\.[we].*$")
              )
          } else .
        }
      }
    }
  
  db <- dplyr::select(db, -all) # remove previously created colum all
  
  end <- Sys.time()
  time <- end - start
  print(time)

  return(tb.out)
}


