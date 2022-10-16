#' @name AnalysisPlan
#' @title Format the analysis plan
#'
#' @description Formats analysis plan for input into Survey mass.
#'
#' @param path string with the pat to the xlsx file with the analysis plan. Use if
#' no `a_plan` is provided
#' @param a_plan data frame with the analysis plan. Use if no `path` is provided
#' labels
#'
#' @return a list with each item corresponding to the analysis of a dependent 
#' variable
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @export

AnalysisPlan <- function(path = NULL, a_plan = NULL) {
  
  ###### PROCESS ANALYSIS PLAN

  # Get analysis plan xlsx from path if provided
  if (is.null(a_plan)) {a_plan <- readxl::read_excel(path)}
  
  # # clean analysis plan
  a_plan <- Filter(function(x) !all(is.na(x)), a_plan)
  a_plan <- a_plan[rowSums(!is.na(a_plan[, grepl("indep.+_\\d|admin_\\d", colnames(a_plan))])) > 0, ]
  #dplyr::filter(dplyr::if_any(dplyr::matches("independent_\\d|admin_\\d"), ~ !is.na(.)))) (dplyr way out, remove)
  
  # Clean analysis plan template
  if (any(grepl("indep.+_", colnames(a_plan)))) {
  
    a_plan <- a_plan %>%
      tidyr::pivot_longer(
        #dplyr::matches("indep.+_|admin_\\d"), names_to = c(".value", "temp"), names_sep = "_"
        dplyr::matches("indep.+_"), names_to = "temp", values_to = "independent"
      ) %>%
      dplyr::select(-temp) %>%
      splitstackshape::cSplit( # Format when >1 indep to be crossed in same table
        "independent", sep = "*", direction = "wide", type.convert = FALSE
      ) %>%
      dplyr::distinct() %>% # remove dup rows when indep is.na 
      tibble::as_tibble()
  
  }
  
  # remove accents and other wierd chars
  a_plan <- a_plan %>%
    dplyr::mutate(dplyr::across(
      dplyr::matches("label|admin|indep"), 
      ~ stringi::stri_trans_general(., "Latin-ASCII")
    ))

  # split each line into a list (each line = one table)  
  a_plan_list <- split(a_plan, seq(nrow(a_plan)))
  
  # a_plan_singles <- a_plan %>% ##### WHEVER SELF-DEFINED GROUPS AVAILABLE
  #   dplyr::filter(is.na(group)) %>%
  #   split(., seq(nrow(.)))
  # a_plan_groups <- split(a_plan, a_plan$group) #seq(nrow(a_plan)))
  
  #a_plan_list <- c(a_plan_singles, a_plan_groups)
  
  a_plan_list <- lapply( # carefull remove the "a_plan" for "a_plan_list" whenever autodefined grouspo avail
    a_plan_list, function(row) Filter(function(x) !all(is.na(x)), row) # remove columns with NA
  ) 
  
  return(a_plan_list)
}