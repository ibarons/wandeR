
#' @name SurveyMass
#' @title Mass production of summary tables 
#'
#' @description Given an analysis plan produces numerous summary tables and index
#' them into an excel file in an orderly manner.
#'
#' @param .data data frame with the data to analyse
#' @param analysis_plan as formated by `AnalysisPlan()`
#' @param ap_take string indicating if the data takes variable names or labels from the analysis plan
#' @param .grand_total logical should grand total to be calculated. Passed to SurveyTbl()
#' @param weight_by Not fully working, need review. Passed to SurveyTbl()
#' @param verbose logical should the table name and time be printed on terminal. Passed to SurveyTbl()
#' @param only_indeps pending development
#' @param file_name string with the name of the xlsx file to export
#' 
#' @return list of summary tables produces. Export an xlsx file with all the tables
#' and a description page
#'
#' @examples
#' 
#' @export

SurveyMass <- function(.data, analysis_plan, ap_take = "name",  .grand_total = FALSE, weight_by = NULL,
                       verbose = FALSE, only_indeps = FALSE, file_name = "summary_Tables") {
  
  if (verbose) {start <- Sys.time()}
  
  .data <- dplyr::mutate(.data, .total = 1) %>% # create variable .total for when desires with and without indep cross
    dplyr::ungroup() # Remove any group_by in the df
  
  
  tables_out <- c()
  for (ap in analysis_plan) { # for loop offers good balance between speed and memory alloc: https://stackoverflow.com/questions/42393658/lapply-vs-for-loop-performance-r

    admins <- ap[grep("admin", colnames(ap), value = TRUE)] %>% as.character()
    
    # WITH INDEPENDENT CROSS
    
    if (any(grepl("indep", colnames(ap)))) {
      
      # Create list name
      dep_name <- ap$name
      indep_name <- unlist(ap[1, grep("indep", colnames(ap))]) # if multipel indeps
      indep_name <- paste0(indep_name, collapse = "; ")
      l_name <- paste(dep_name, "_by_", indep_name) # "_by_" so can serve as unique separator between dep and indep
      
      indeps <- ap[grep("indep", colnames(ap), value = TRUE)] %>% as.character()
      indep_sym <- rlang::syms(indeps)
      
      # If numeric dependent
      if (grepl("integer", ap$type)) {
        
        tables_out[[l_name]] <- SurveyTbl(
          .data, !!!indep_sym, .num = dplyr::pull(ap[, ap_take]), .admins = admins, 
          weight_by = weight_by, .grand_total = .grand_total, 
          verbose = verbose
        ) %>%
          dplyr::select(-dplyr::matches("^\\.total$"))
        
      } else if (grepl("select_one|text", ap$type)) {
        
        dep_sym <- rlang::sym(dplyr::pull(ap[, ap_take]))
        tables_out[[l_name]] <- SurveyTbl(
          .data, !!!indep_sym, !!dep_sym, .admins = admins,
          weight_by = weight_by, .grand_total = .grand_total,
          verbose = verbose
        ) %>%
          dplyr::select(-dplyr::matches("^\\.total$"))
        
      } else if (grepl("select_multiple", ap$type)) { # Pending self-defined groups
        
        dep_multiple <- dplyr::select(.data, dplyr::contains(dplyr::pull(ap[, ap_take]))) %>%
          colnames()
        dep_multiple <- dep_multiple[-1] # the first is the non-binary. We always keep the non-binary and binaries for select_multiple
        
        tables_out[[l_name]] <- purrr::map_df(
          dep_multiple, function(x)
          {
            dep_sym <- rlang::sym(x)
            
            SurveyTbl(
              .data, !!!indep_sym, !!dep_sym, .admins = admins,
              weight_by = weight_by, .grand_total = .grand_total, 
              verbose = verbose
            ) %>%
              dplyr::rename("category" = !!dep_sym) %>%
              dplyr::mutate(
                question = as.character(dep_sym),
                category = dplyr::case_when(
                  category == 1 ~ "Yes", category == 0 ~ "No", 
                  TRUE ~ as.character(category))
              ) %>%
              dplyr::relocate(question, .before = category) %>%
              dplyr::select(-dplyr::matches("^\\.total$"))
          }
        )
      }
      
    } else {
      
      l_name <- ap$name
      
      # If numeric dependent
      if (grepl("integer", ap$type)) {
        
        tables_out[[l_name]] <- SurveyTbl(
          .data, .num = dplyr::pull(ap[, ap_take]), .admins = admins, 
          weight_by = weight_by, .grand_total = .grand_total, 
          verbose = verbose
        )
        
      } else if (grepl("select_one|text", ap$type)) {
        
        dep_sym <- rlang::sym(dplyr::pull(ap[, ap_take]))
        tables_out[[l_name]] <- SurveyTbl(
          .data, !!dep_sym, .admins = admins,
          weight_by = weight_by, .grand_total = .grand_total,
          verbose = verbose
        )
        
      } else if (grepl("select_multiple", ap$type)) { # Pending self-defined groups
        
        dep_multiple <- dplyr::select(.data, dplyr::contains(dplyr::pull(ap[, ap_take]))) %>%
          colnames()
        dep_multiple <- dep_multiple[-1] # the first is the non-binary. We always keep the non-binary and binaries for select_multiple
        
        tables_out[[l_name]] <- purrr::map_df(
          dep_multiple, function(x)
          {
            dep_sym <- rlang::sym(x)
            
            SurveyTbl(
              .data, !!dep_sym, .admins = admins,
              weight_by = weight_by, .grand_total = .grand_total, 
              verbose = verbose
            ) %>%
              dplyr::rename("category" = !!dep_sym) %>%
              dplyr::mutate(
                question = as.character(dep_sym),
                category = dplyr::case_when(
                  category == 1 ~ "Yes", category == 0 ~ "No", 
                  TRUE ~ as.character(category))
              ) %>%
              dplyr::relocate(question, .before = category)
          }
        )
      }
    }
  }

  table_names <- sapply(
    names(tables_out),
    function(x) as.data.frame(tables_out[[x]])$tbl_name[1]
  ) %>% as.character()
  
  description <- dplyr::bind_rows(analysis_plan) %>%
    dplyr::rename("dependent" = label, "sheet_name" = name) %>%
    dplyr::mutate(
      sheet_name = stringr::str_trunc(sheet_name, 31),
      table_name = table_names, .before = 1
    ) %>%
    dplyr::select(
      sheet_name, table_name, dplyr::matches("admin_"), 
      dependent, dplyr::matches("independent_"),
      dplyr::everything(), -dplyr::matches("^group$")
    )
  
  # add description file
  tables_out <- c(list("0.description" = description), tables_out)
  # Merge in same sheet related tables
  tables_out <- split(tables_out, c("0.description", description$sheet_name))
  
  if (verbose) {
    time <- Sys.time() - start
    message("Analysis completed in: ")
    print(time) 
  }
  
  
  if (!is.null(file_name)) {
    WeFunction::ExportXLSX(
      tables_out, file_name = file.path("outputs", WeFunction::DateStamp(file_name))
    )
  }
  
  return(tables_out)
  
}