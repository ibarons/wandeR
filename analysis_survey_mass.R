################################################################################
## @Title: analysis_survey_mass.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 02.01.2021
## @Last updated: 11.03.2021
## @Status: Deprecated
## -----------------------------------------------------------------------------
##
## Notes:
## 
## Pending to add Single.numerical * multiple.categoric (e.g. income * coping)
##
## Optional pivot_wider, at the end, purrr:: pivot_wider over final list of tables
## Optional to offer bin_rows and release all as 1 single table (Nour - PowerBI)
##
## Add an argument that takes a list of tables externally defined and merge it to
## list of tables to be output addition.tbls = list()
## 
## Integrate that is vector names is "Dep" = "Indep1 * Indep2" it input it in 
## SurveyTbl(., Indep1, Indep2, Dep, ...). Further consider a function definition
##  for dep * indep such as: `dep ~ indep1 + indep2 * indep3 + inde4 + ...`
##
## .median == TRUE and .error != "se" dotn work because of the pivot wider and the
## variable name "med..."and perce.we_se... are not recognised
##
## ----------------------------------------------------------------------------
#' @name SurveyMass
#' @title Mass production of cross-tabulation table
#'
#' @description by defininf a names vector with c(dependent = independent) for
#'  different variable types (categorical/numerical, single/multiple answer) the
#'  function produce all indicated crossed tabulations and output a list of tables
#'  and and export them to an .xlsx (optional)
#'
#' @usage
#' SurveyMass(db, vars.single = NULL, vars.num = NULL, vars.multicat = NULL,
#' vars.multinum = NULL, multipl.dep = NULL, unit = "HH",
#' .subdistrict = FALSE, .extra = NULL, .median = FALSE,
#' .q_method = "linear", .error = NULL, .low.count = FALSE,
#' add.tables = NULL, export.tables = TRUE)
#'
#' @param db data frame as survey.design class from `srvyr::as_survey_design()`
#' with variables to be analysed and admin level to be disaggregated by.
#' @param vars.single a named character vector as c("dependent" = "ïndependent").
#' Variables should be categorical and of single answer. Both variable names should
#' be found in `colnames(db)`. Each element of the vector will produce a crossed 
#' table Dep * Indep with column % for independent. All dependent variables 
#' should be a factor.
#' @param vars.num Same as `vars.single` but for numerical variables.
#' @param vars.multicat a named character vector as c("dependent" = "ïndependent").
#' Variables should be categorical and multiple answer (multiple columns in db).
#' While "dependent" need not to be found in `colnames(db)`, "ïndependent" must.
#' Each element of the vector will produce a crossed table Dep * Indep with 
#' column percentages for independent. All independent variables should be a factor.
#' @param vars.multinum Same as for `vars.multicat` but for numerical variable.
#' @param multipl.dep a two level list with the first level corresponding to each
#' of the dependent variable and the second, for each of them, to indicate the
#' `colname(db)` that correspond to the dependent and the given names. Is an auxiliar
#' list to `vars.multi...` See details and example sections.
#' @param unit a string to indicate the unit of analysis for table naming (e.g. HH or Ind...)
#' @param .subdistrict Logical to indicate if want to dis-aggregate at Sub-district 
#' level. Argument passed to `R.HNAP::SurveyTbl()`
#' @param .extra to add an extra group for disaggregation. Argument passed to 
#' `R.HNAP::SurveyTbl()`
#' @param .median logical if median need to be calculated. Argument passed to 
#' `R.HNAP::SurveyTbl()`
#' @param .q_method passed to `R.HNAP::SurveyTbl()`. Choose between "linear" 
#' or "constant"
#' @param .error measure to indicate the error of the estimate: c("se", "ci", 
#' "var", "cv"). `NULL` if no error measure desired. Argument passed to 
#' `R.HNAP::SurveyTbl()`
#' #' @param .low.count Logical if a check for low expected count cell should be done.
#' Base on SPSS check for contingency tables. Only available when at least two 
#' vars passed to `...`
#' @param add.tables a two level list with new tables to append to the output.
#' First level define sheet name (group of dfs), second define df name. Advised
#' to use same nomenclature as in the f(x) for df name in the list: 
#' "tbl.unit.dependent.independent".
#' @param export.tables Logical if export tables to .xlsx 
#'
#' @details
#' 
#' The input names vectors follow the pattern:
#' * in v <- c("x"= "y"), "y" is the independent `as.character(v)`
#' * in v <- c("x"= "y"), "x" is the dependent `names(v)`
#' 
#' By default the the function output in fist place a simple descriptive of the
#' dependent variable for the disaggregation level (no cross-tab)
#' 
#' This function is essentially a wrapper for R.HNAP::SurveyTbl() for multiple 
#'  variable types and through multiple loops. See documentation or surveyTbl() 
#'  for more details
#'  
#' `multipl.dep`: as multiple categorical and numerical are based on multiple 
#' columns in the data base, this two level list aim at:
#'   
#'  * Provide an input to `purrr::map` `SurveyTbl()` across all dependent variables
#'  * Identify wich `colnames(db)` are part of the dependent variable
#'  * Allow to rename the dependent variable (as whole) and the variables by which
#'   it is form, as usually the names in the db for this variable is not legible.
#'  * Have a single argument for both multiple categorical and numerical 
#'   
#'  The argument should return an objject of form: 
#'   `multipl.dep[["livelihoods"]][["names"]]`
#'   `multipl.dep[["livelihoods"]][["var"]]`
#'   `multipl.dep[["coping"]][["names"]]`
#'   `multipl.dep[["coping"]][["var"]]`
#'   `multipl.dep[[...]]`
#'  See example for how to easily create this list 
#'
#' @return 
#' A list of data frames containing all the defined tables, including a data.frame
#' with the description of each produced tables
#' 
#' The exported excel file include a "description" sheet with the list of tables
#' and analyzed variables for each, the subsequent sheet are organized by dependent
#' variable, with one sheet for each and in it all the tables for its defined crossing
#' with independent variable/s. the first table of each sheet will always be the
#' simple descriptive of the dependent variable (not crossing)
#'
#' @examples
#' \dontrun{
#'
#' vars.single <- c("incomeSuffice" = "HoHsex", "incomeSuffice" = "vulnerability2")
#' 
#' vars.multicat <- c("livelihoods" = "HoHsex", "livelihoods" = "vulnerability2")
#' 
#' vars.num <- c("hhIncome" = "HoHsex", "hhIncome" = "vulnerability2") # dependent's names not need to be found in colnames(db)
#' 
#' vars.multinum <- c("expenses" = "HoHsex", "expenses" = "vulnerability2") # dependent's names not need to be found in colnames(db)
#' 
#' # Define multiple vars names (manually)
#' 
#' multipl.dep <- c()
#' 
#' form <- R.HNAP::GetForm("../db/Kobo_survey_IDP_2010.xlsx")
#' 
#' multipl.dep[["livelihoods"]][["names"]] <- c("first", "second", "third") # Define manually
#' multipl.dep[["livelihoods"]][["var"]] <- rlang::set_names(
#'   grep("D2_\\d$", colnames(hh.lab.w), value = TRUE),
#'   multipl.dep[["livelihoods"]][["names"]] 
#' )
#' 
#' multipl.dep[["expenses"]][["names"]] <- c( # Define manually
#'   "rent", "shelt.mantain", "food","fuel", "electricity", "education", "health",
#'   "water", "NFI", "hygiene", "transport", "repair.assets", "communication", 
#'   "debt", "product.assets", "COVID19", "other"
#' ) 
#' multipl.dep[["expenses"]][["var"]] <- rlang::set_names(
#'   grep("D4_\\d{1,2}$", colnames(hh.lab.w), value = TRUE),
#'   multipl.dep[["expenses"]][["names"]] 
#' )
#'
#' tables <- SurveyMass(hh.lab.w, vars.single, vars.num, vars.multicat, vars.multinum, 
#'                      multipl.dep, unit = "HH", .median = FALSE, .error = "se",
#'                      export.tables = TRUE)
#' }
#'
#' @export
## -----------------------------------------------------------------------------

SurveyMass <- function(db, vars.single = NULL, vars.num = NULL, vars.multicat = NULL,
                       vars.multinum = NULL, multipl.dep = NULL, unit = "HH",
                       .subdistrict = FALSE, .extra = NULL, .median = FALSE,
                       .q_method = "linear", .error = NULL, .low.count = FALSE,
                       add.tables = NULL, export.tables = TRUE) {

  ########################### ERROR AND WARNINGS ###############################
  
  if (is.null(c(vars.single, vars.num, vars.multicat, vars.multinum ))) {
    stop("No variable have been defined for analysis. All `vars...` arguments are NULL")
  }

  if (
    ( (!is.null(vars.multicat) | !is.null(vars.multinum)) & is.null(multipl.dep) )  | 
    ( is.null(vars.multicat) & is.null(vars.multinum) & !is.null(multipl.dep) )
  ) {
    stop("Multiple categorical/numerical to analyse (`vars.multi...`) are defined, but not have not provided auxiliar list (`multipl.dep`) or vice-versa.")
  }
  
  if (any(!(c(vars.single, vars.num) %in% colnames(db)))) {
    stop("Variables defined in `vars.single` or `vars.num` are not found in the `db`")
  }
  
  if (any(is.na(names(c(vars.single, vars.num, vars.multicat, vars.multinum))))) {
    warning("Some dependent variables have not been defined: is.na() == TRUE")
  }
  
  if (any(is.na(c(vars.single, vars.num, vars.multicat, vars.multinum)))) {
    warning("Some independent variables have not been defined: is.na() == TRUE")
  }
  
  out <- c()
  ############################### SINGLE CATEGORICAL ###########################
  
  if (!is.null(vars.single)) {
  
    ##### Analysis plan
    
    analy.single.vars <- list(
      independents = as.character(vars.single), # in v <- c("x"= "y"), "y" is the independent `as.character(v)`
      dependents = names(vars.single) # in v <- c("x"= "y"), "x" is the dependent `names(v)`
    )
    
    ##### ONE-WAY
    
    out[["tbls.single.oneway"]] <- purrr::pmap(
      
      list(unique(analy.single.vars$dependents)), ~ {
        
        tbls <- db %>%
          dplyr::filter(!is.na(!!rlang::sym(..1))) %>%
          R.HNAP::SurveyTbl(
            !!rlang::sym(..1), .extra = .extra, .subdistrict = .subdistrict,
            .error = .error, .name = paste0(unit, " - ", ..1) 
          )
      }
    )
    
    names(out$tbls.single.oneway) <- paste0(
      "TBL.", unit, ".DEP.", unique(analy.single.vars$dep), ".INDEP.total"
    )
    
    ##### TWO-WAY 
    
    out[["tbls.single.twoway"]] <- purrr::pmap(
      analy.single.vars, ~ {
        
        tbls <- db %>%
          dplyr::filter(!is.na(!!rlang::sym(..1)), !is.na(!!rlang::sym(..2))) %>%
          R.HNAP::SurveyTbl(
            !!rlang::sym(..1), !!rlang::sym(..2), 
            .extra = .extra, .subdistrict = .subdistrict, .error = .error, .low.count = .low.count,
            .name = paste0(unit, " - ", ..2, " by ", ..1)
          ) %>%
          tidyr::pivot_wider(
            names_from = !!rlang::sym(..1), # When expanding to multiple independent, get the First one to pivot: f(db, v1, v2, v3, v4), get v1 to pivot!!!
            values_from = dplyr::matches("^(coun.we|perc.we|perc.we_se|coun.un)"),
            values_fill = 0
          ) %>%
          dplyr::relocate(
            dplyr::matches("^low.count$"), adminPcode, dplyr::matches("admin\\d[NP]|AoC"),
            .after = dplyr::last_col()
          )
      })
    
    names(out$tbls.single.twoway) <- paste0(
      "TBL.", unit, ".DEP.", analy.single.vars$dep, ".INDEP.", analy.single.vars$indep
    )
    
  }
  
  ############################## MULTIPLE CATEGORICAL ##########################
  
  if (!is.null(vars.multicat)) {
  
    ##### Analysis plan
    
    analy.multicat.vars <- list(
      independents = as.character(vars.multicat),
      dependents = names(vars.multicat)
    )
    
    ##### One way
    
    out[["tbls.multicat.oneway"]] <- purrr::pmap(
      
      list(unique(analy.multicat.vars$dependents)), ~ {
        
        depend <- ..1
        
        tbls <- dplyr::bind_rows(
          purrr::map2(
            multipl.dep[[..1]][["var"]], multipl.dep[[..1]][["names"]],
            ~ db %>%
              dplyr::filter(!is.na(!!rlang::sym(.x))) %>%
              R.HNAP::SurveyTbl(
                !!rlang::sym(.x), .extra = .extra, .subdistrict = .subdistrict,
                .error = .error, .name = paste0(unit, " - ", ..1)
              ) %>%
              dplyr::rename("category" = .x) %>%
              dplyr::mutate(., !!rlang::sym(depend) := .y) %>%
              dplyr::relocate(!!rlang::sym(depend), .before = category)
          )
        )
      }
    )
    
    names(out$tbls.multicat.oneway) <- paste0(
      "TBL.", unit, ".DEP.", unique(analy.multicat.vars$dep), ".INDEP.total"
    )
    
    ##### Two way
    
    out[["tbls.multicat.twoway"]] <- purrr::pmap(
      analy.multicat.vars, ~ {
        
        indep <- ..1
        depend <- ..2

        tbls.camp <- dplyr::bind_rows(
          purrr::map2(
            multipl.dep[[..2]][["var"]], multipl.dep[[..2]][["names"]],
            ~ db %>%
              dplyr::filter(!is.na(!!rlang::sym(.x))) %>%
              R.HNAP::SurveyTbl(
                !!rlang::sym(indep), !!rlang::sym(.x), 
                .extra = .extra, .subdistrict = .subdistrict, .error = .error, .low.count = .low.count,
                .name = paste0(unit, " - ", .x, " by ", indep)
              ) %>%
              dplyr::rename("category" = .x) %>%
              dplyr::mutate(., !!rlang::sym(depend) := .y) %>%
              dplyr::relocate(!!rlang::sym(depend), .before = category)
            
          )) %>%
          tidyr::pivot_wider(
            names_from = !!rlang::sym(..1),
            values_from = dplyr::matches("^(coun.we|perc.we|perc.we_se|coun.un)"),
            values_fill = 0
          ) %>%
          dplyr::relocate(
            dplyr::matches("^low.count$"), adminPcode, dplyr::matches("admin\\d[NP]|AoC"),
            .after = dplyr::last_col()
          )
      })
    
    names(out$tbls.multicat.twoway) <- paste0(
      "TBL.", unit, ".DEP.", analy.multicat.vars$dep, ".INDEP.", analy.multicat.vars$indep
    )
  
  }
  
  ############################ SINGLE NUMERICAL ################################
  
  if (!is.null(vars.num)) {
    
    ##### Analysis plan
    
    analy.num.vars <- list(
      independents = as.character(vars.num),
      dependents = names(vars.num)
    )
    
    ##### One way
    
    out[["tbls.num.oneway"]] <- purrr::pmap(
      
      list(unique(analy.num.vars$dependents)), ~ {
        
        tbls <- R.HNAP::SurveyTbl(
          db, all, .num = ..1, .extra = .extra, .subdistrict = .subdistrict,
          .median = .median, .error = .error, .name = paste0(unit, " - ", ..1)
        ) %>%
          dplyr::select(-all)
      }
    )
    
    names(out$tbls.num.oneway) <- paste0(
      "TBL.", unit, ".DEP.", unique(analy.num.vars$dep), ".INDEP.total"
    )

    ##### Two way
    
    out[["tbls.num.twoway"]] <- purrr::pmap(
      analy.num.vars, ~ {
        
        tbls <- db %>%
          dplyr::filter(!is.na(!!rlang::sym(..1))) %>%
          R.HNAP::SurveyTbl(
            !!rlang::sym(..1), .num = ..2, .extra = .extra, .subdistrict = .subdistrict,
            .median = .median, .error = .error, .low.count = .low.count,
            .name = paste0(unit, " - ", ..1, " by ", ..2)
          ) %>%
          tidyr::pivot_wider(
            names_from = !!rlang::sym(..1),
            values_from = dplyr::matches("^(avg.we|med.we|sum.we|avg.we_se|coun.we|coun.un)")
          ) %>%
          dplyr::relocate(
            dplyr::matches("^low.count$"), adminPcode, dplyr::matches("admin\\d[NP]|AoC"),
            .after = dplyr::last_col()
          )
        
      })
    
    names(out$tbls.num.twoway) <- paste0(
      "TBL.", unit, ".DEP.", analy.num.vars$dep, ".INDEP.", analy.num.vars$indep
    )
  
  }
  
  ############################ MULTIPLE NUMERICAL ##############################
  
  if (!is.null(vars.multinum)) {
  
    ##### Analysis plan
      
    analy.multinum.vars <- list(
      independents = as.character(vars.multinum),
      dependents = names(vars.multinum)
    )
    
    ##### One way
    
    out[["tbls.multinum.oneway"]] <- purrr::pmap(
      
      list(unique(analy.multinum.vars$dependents)), ~ {
        
        depend <- ..1
        
        tbls <- dplyr::bind_rows(
          purrr::map2(
            multipl.dep[[..1]][["var"]], multipl.dep[[..1]][["names"]],
            ~ db %>%
              dplyr::filter(!is.na(!!rlang::sym(.x))) %>%
              R.HNAP::SurveyTbl(
                all, .num = .x, .extra = .extra, .subdistrict = .subdistrict,
                .median = .median, .error = .error, .name = paste0(unit, " - ", ..1)
              ) %>%
              dplyr::mutate("category" = .y, .after = "adminName")
          )) %>%
          dplyr::select(-all)
      }
    )
    
    names(out$tbls.multinum.oneway) <- paste0(
      "TBL.", unit, ".DEP.", unique(analy.multinum.vars$dep), ".INDEP.total"
    )
    
    ##### Two way
    
    out[["tbls.multinum.twoway"]] <- purrr::pmap(
      analy.multinum.vars, ~ {
        
        indep <- ..1
        depend <- ..2
        
        tbls.camp <- dplyr::bind_rows(
          purrr::map2(
            multipl.dep[[..2]][["var"]], multipl.dep[[..2]][["names"]],
            ~ db %>%
              dplyr::filter(!is.na(!!rlang::sym(.x))) %>%
              R.HNAP::SurveyTbl(
                !!rlang::sym(indep), .num = .x, 
                .extra = .extra, .subdistrict = .subdistrict,
                .median = .median, .error = .error, .low.count = .low.count,
                .name = paste0(unit, " - ", .x, " by ", indep)
              ) %>%
              dplyr::mutate("category" = .y)
          )) %>%
          tidyr::pivot_wider(
            names_from = !!rlang::sym(..1),
            values_from = dplyr::matches("^(avg.we|med.we|sum.we|avg.we_se|coun.we|coun.un)")
          ) %>%
          dplyr::relocate(
            dplyr::matches("^low.count$"), adminPcode, dplyr::matches("admin\\d[NP]|AoC"),
            .after = dplyr::last_col()
          )
      })
    
    names(out$tbls.multinum.twoway) <- paste0(
      "TBL.", unit, ".DEP.", analy.multinum.vars$dep, ".INDEP.", analy.multinum.vars$indep
    )
    
  }
  
  ################################## OUTPUT ####################################
  if (!is.null(add.tables)) {
    
    if (purrr::vec_depth(add.tables) != 4)
      warning(
        "Check the additional list of tables should formated in two level list: first
      for sheet name, second a df names with table name. See documentation"
      )
    
    out <- c(out, add.tables)
    
  }
  
  out.tables <- R.HNAP::DescribeTbls(out)
  
  # # Simplify the list and keep tbl names only
  # out <- unlist(out, recursive = FALSE)
  # names(out) <- gsub(paste0("^.+\\.(TBL\\.", unit, "\\..*)$"), "\\1", names(out))
  # 
  # # Warn null tables
  # if (any(lengths(out) == 0)) {
  #   warning(
  #     "Some cross-tabs defined resulted empty. Check carefully the following: ",
  #     paste(
  #       "\n", which(lengths(out) == 0), "-", 
  #       names(which(lengths(out) == 0)), "\n"
  #     )
  #   )
  # }
  # 
  # # Group tables by dependent variable to export one xlsx sheet per dependent
  # s.names <- unique(gsub(
  #   paste0("^(TBL\\.", unit, "\\.DEP\\.)", "(.+)\\.INDEP\\.(.+$)"),
  #   "\\2",
  #   names(out)#unlist(sapply(out, names))
  # ))
  # 
  # out.tables <- purrr::map_depth(
  #   s.names, .depth = 1, 
  #   ~ {out[[as.character(.x)]] <- out[grep(paste0(
  #     "^(TBL\\.", unit, "\\.DEP\\.)", .x, "\\.INDEP\\."), names(out) #unlist(sapply(out, names))
  #   )] 
  # })
  # names(out.tables) <- s.names
  # 
  # if (!is.null(add.tables)) {
  # 
  #   if (purrr::vec_depth(add.tables) != 4)
  #     warning(
  #     "Check the additional list of tables is formated in two level list: first
  #     for sheet name, second a df names with table name. See documentation"
  #   )
  # 
  #   out.tables <- c(out.tables, add.tables)
  # 
  # }
  # 
  # # Create data frame to describe each tables
  # s.names.list <- lapply(
  #   seq_along(out.tables), function(i) 
  #     rep(names(out.tables)[[i]], length(out.tables[[i]]))
  # )
  # 
  # description <- dplyr::bind_rows(
  #   purrr::map2(out.tables, s.names.list, ~ {
  #     
  #     i = seq_along(.x)
  #     nam <- as.character(lapply(.x, function(x) unique(x$tbl.name[1]))[i])
  # 
  #     data.frame(
  #       sheet = .y,
  #       tbl.name = nam[i],
  #       vars_rows = gsub(
  #         paste0("^(TBL\\.", unit, "\\.DEP\\.)", "(.+)\\.INDEP\\.(.+$)"),
  #         "\\2", names(.x)
  #       ),
  #       vars_cols = gsub(
  #         paste0("^(TBL\\.", unit, "\\.DEP\\.)", "(.+)\\.INDEP\\.(.+$)"),
  #         "\\3", names(.x)
  #       ),
  #       description = rep("", length(names(.x)))
  #       #row.names = NULL # this would allow to output in the description the table name in R ("TBL....")
  #     )
  #   })
  # )
  # out.tables[["description"]][["description"]] <- description
  # 
  # # Move description to first page
  # out.tables <- rlist::list.subset(
  #   out.tables, c("description", names(out.tables)[-length(names(out.tables))])
  # )
  
  if (export.tables == TRUE) {
  
    # Export
    wb <- R.HNAP::ExportXLSX(out.tables)
    openxlsx::saveWorkbook(
      wb,
      paste0("output_tables/", R.HNAP::DateStamp("_survey_analysis_tables.xlsx")),
      overwrite = TRUE
    )
    cat(
      "\nSurvey tables exported to:\n",
      paste0("~/output_tables/", R.HNAP::DateStamp("_survey_analysis_tables.xlsx")), "\n"
    )
  }
 
  return(out.tables)
}


