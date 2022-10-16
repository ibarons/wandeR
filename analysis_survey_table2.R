
### ... write the analysis variable the last. Teh last variable is always the dependent

### .admins the order of the admins need to be from the Highest to the lowest admin level (e.g. c("Governorate", "Municipality", "Community")

#### group as of today only for dependents...

### weight_by  a ver name comprising the weights. For non weight leave NULL

### not a good idea to get question name to the header. Rather better all, alwasy
## column called "question", with question_name as a value, colum called category with answer categories as values


#' @name SurveyTbl
#' @title Summary tables 
#'
#' @description Produce summary tables by defined groups of aggregation for 
#' numeric or categorical variables.
#'
#' @param .data data frame with the data to analyse
#' @param ... symbols with the variables to analyse. The last variable is always
#'  the dependent
#' @param .num string if a numeric variable is the dependent
#' @param .admins character vector with the name of the admin variables. The 
#' order of the admins need to be from the Highest to the lowest e.g. c("Governorate", "Municipality", "Community")
#' @param group deprecated (pendign confirmation to update docs)
#' @param weight_by Not fully working, need review.
#' @param .grand_total logical should grand total by calculated
#' @param verbose logical should the table name and time be printed on terminal
#'
#'
#' @return data frame with the summary table
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
#'   admin3Name = sample(c("Madrid", "Manila", "Sevilla", "Cotabato"), 100, replace = TRUE),
#'   weights = runif(100, 0, 1)
#' )
#' 
#' SurveyTbl(db, sex, age, .admins = c("admin1Name", "admin3Name"))
#' 
#' SurveyTbl(db, sex, age, .num = "pop", .admins = c("admin1Name", "admin3Name"))
#' 
#' SurveyTbl(
#'  db, sex, age, .num = "pop", .admins = c("admin1Name", "admin3Name"),
#'  weight_by = "weights"
#' )
#'
#' @export

SurveyTbl <- function(.data, ..., .num = NULL, .admins = NULL, group = NULL,
                      .grand_total = TRUE, weight_by = NULL, verbose = FALSE) {
  
  if (verbose) {start <- Sys.time()}
  
  ############################ PREELIMINARIES ##################################
  
  # tidyeval
  num.sym <- rlang::syms(.num)
  dots <- rlang::enquos(...)
  admin_sym <- rlang::syms(.admins)
  
  if (!is.null(weight_by)) {
    weight_sym = rlang::sym(weight_by)
  }
  
  # Crete table name (categorical vars)
  if (is.null(.num) && length(names(dots)) > 1) {
    
    dep_var <- dots[length(names(dots))] %>% as.character()
    dep_name <- trimws(gsub("([.-])|[[:punct:]]", "\\1", dep_var))
    
    indep_var <- dots[1:(length(names(dots)) - 1)] %>% as.character()
    indep_name <- trimws(gsub("([.-])|[[:punct:]]", "\\1", indep_var))
    
    .tbl_name <- paste(dep_name, "by", paste(indep_name, collapse = "; "))
    
  } else if (is.null(.num) && length(names(dots)) == 1) {
    
    dep_var <- dots[length(names(dots))] %>% as.character()
    dep_name <- trimws(gsub("([.-])|[[:punct:]]", "\\1", dep_var))
    
    .tbl_name <- trimws(dep_name)
  }

  # Crete table name (numeric vars)
  if (!is.null(.num) && length(names(dots)) >= 1) {
    
    dep_name <- trimws(.num)
    
    indep_var <- dots[1:(length(names(dots)) - 1)] %>% as.character()
    indep_name <- trimws(gsub("([.-])|[[:punct:]]", "\\1", indep_var))
    
    .tbl_name <- paste(dep_name, "by", paste(indep_name, collapse = "; "))
    
  } else if (!is.null(.num) && length(names(dots)) == 0) {
    .tbl_name <- trimws(.num)
  }

  if (verbose) {message(.tbl_name)}
  
  ########################### ESTIMATES FOR ROW ################################
  
  ### TABLE WIT TOTAL
  tbl_total <- data.frame() # create object so bind_row at the end will not fail
  
  if (.grand_total) {
    
    tbl_total_filt <- .data %>%
      dplyr::filter(if_all(c(...), ~!is.na(.))) %>% 
      dplyr::group_by(...) # IF .x and ... in same group in map cause conflict
    
    tbl_total <- tbl_total_filt %>%
      {
        if (length(.num) == 0) {
          dplyr::summarise(., count = dplyr::n(), .groups = "drop_last") %>%
            dplyr::mutate(perc = count / sum(count))
        } else {
          dplyr::mutate(., {{.num}} := as.numeric(!!!num.sym)) %>%
            dplyr::summarise(
              question = .num,
              count = dplyr::n(),
              average = dplyr::if_else( # else NaN in Excle reflext as ugly "!#NULL"
                is.nan(mean(!!!num.sym, na.rm = TRUE)), NA_real_, mean(!!!num.sym, na.rm = TRUE)
              ),
              median = median(!!!num.sym, na.rm = TRUE),
              sum = sum(!!!num.sym, na.rm = TRUE),
              .groups = "drop"
            )
        }
      } %>%
      dplyr::ungroup()
    
    if (!is.null(weight_by)) {
      
      tbl_total_w <- tbl_total_filt %>%
        {
          if (length(.num) == 0) {
            dplyr::summarise(., count_we = sum(!!weight_sym, na.rm = TRUE), .groups = "drop_last") %>%
              dplyr::mutate(perc_we = count_we / sum(count_we))
          } else {
            dplyr::mutate(., {{.num}} := as.numeric(!!!num.sym)) %>%
              dplyr::summarise(
                #question = .num,
                count_we =  sum(!!weight_sym, na.rm = TRUE),
                average_we = dplyr::if_else( # else NaN in Excle reflext as ugly "!#NULL"
                  is.nan(weighted.mean(!!!num.sym, !!weight_sym, na.rm = TRUE)),
                  NA_real_,
                  weighted.mean(!!!num.sym, !!weight_sym, na.rm = TRUE)
                ),
                median_we = matrixStats::weightedMedian(!!!num.sym, !!weight_sym, na.rm = TRUE),
                sum = sum(!!!num.sym, na.rm = TRUE),
                .groups = "drop"
              )
          } 
        } %>%
        dplyr::ungroup()
      
      tbl_total <- suppressMessages(dplyr::full_join(tbl_total, tbl_total_w))
      
    }
    
    # add admin markers to total, if .admins !is.null
    tbl_total <- tbl_total %>%
      dplyr::mutate(admin_level = "Total", admin_name = "Total", .before = 1)
    
  }
  
  ##### TABLES FOR EACH ADMIN
  tbl_admin <- data.frame() # create object so bind_row at the end will not fail
  
  if (!is.null(.admins)) {
    
    tbl_admin <- lapply(
      admin_sym,
      function(x)
      {
        
        tbl_admin_filt <- .data %>%
          dplyr::filter(if_all(c(...), ~!is.na(.))) %>% 
          dplyr::group_by(!!x, ...)
        
        tbl_admin <- tbl_admin_filt %>%
          {
            if (length(.num) == 0) {
              dplyr::summarise(., count = dplyr::n(), .groups = "drop_last") %>%
                dplyr::mutate(perc = count / sum(count))
            } else {
              dplyr::mutate(., {{.num}} := as.numeric(!!!num.sym)) %>%
                dplyr::summarise(
                  question = .num,
                  count = dplyr::n(),
                  average = dplyr::if_else( # else NaN in Excle reflext as ugly "!#NULL"
                    is.nan(mean(!!!num.sym, na.rm = TRUE)), NA_real_, mean(!!!num.sym, na.rm = TRUE)
                  ),
                  median = median(!!!num.sym, na.rm = TRUE),
                  sum = sum(!!!num.sym, na.rm = TRUE),
                  .groups = "drop"
                )
            }
          } %>%
          dplyr::mutate(admin_level = as.character(x)) %>%
          #dplyr::rename("admin_name" = !!x) %>% # TO BE REMOVED!
          dplyr::ungroup()
        
        if (!is.null(weight_by)) {
          
          tbl_admin_w <- tbl_admin_filt %>%
            {
              if (length(.num) == 0) {
                dplyr::summarise(., count_we = sum(!!weight_sym, na.rm = TRUE), .groups = "drop_last") %>%
                  dplyr::mutate(perc_we = count_we / sum(count_we))
              } else {
                dplyr::mutate(., {{.num}} := as.numeric(!!!num.sym)) %>%
                  dplyr::summarise(
                    #question = .num,
                    count_we = sum(!!weight_sym, na.rm = TRUE),
                    average_we = dplyr::if_else( # else NaN in Excle reflext as ugly "!#NULL"
                      is.nan(weighted.mean(!!!num.sym, !!weight_sym, na.rm = TRUE)),
                      NA_real_,
                      weighted.mean(!!!num.sym, !!weight_sym, na.rm = TRUE)
                    ),
                    median_we = matrixStats::weightedMedian(!!!num.sym, !!weight_sym, na.rm = TRUE),
                    sum_we = median(!!!num.sym * !!weight_sym, na.rm = TRUE),
                    .groups = "drop"
                  )
              }
            } %>%
            dplyr::mutate(admin_level = as.character(x)) %>%
            # dplyr::rename("admin_name" = !!x) %>% # TO BE REMOVED!
            dplyr::ungroup()
          
          
          tbl_admin <- suppressMessages(dplyr::full_join(tbl_admin, tbl_admin_w))
          
        } else {
          tbl_admin
        }
      }
    )
    names(tbl_admin) <- .admins
    
    # # Join higher admin level for each
    tbl_admin <- lapply(
      names(tbl_admin), function(x)
      {
        
        temp <- rev(.admins)
        
        i <- which(temp == x) # get index of current admin evaluated (x)
        i_anti <- temp[ i:length(temp)] # get higher admisn than the one beign eval
        
        admin_join <- .data %>% dplyr::select(all_of(i_anti)) %>% dplyr::distinct()
        
        tbl_admin <- suppressMessages(dplyr::left_join(tbl_admin[[x]], admin_join)) %>%
          dplyr::rename("admin_name" = all_of(x)) %>% # if more than 1 should return an error fo duplicate colnames... which is fine
          dplyr::relocate(admin_level, admin_name, .before = 1)
        
      }
    )
    
  }
  
  # Bind tables and out
  df_tbl <- dplyr::bind_rows(tbl_total, tbl_admin) %>%
    dplyr::mutate(tbl_name = .tbl_name, .before = 1) %>%
    tibble::as_tibble()
  
  if (verbose) {
    time <- Sys.time() - start
    print(time)
  }
  
  
  return(df_tbl)
  
}
