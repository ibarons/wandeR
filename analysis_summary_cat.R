################################################################################
## @Title: format_form_labels.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 27.02.2020
## @Last updated: 14.07.2020
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes
## na.value is ""by defualt, cause if marked as NULL, need to use a conditional
## to sort out the is.null requeirement
## ----------------------------------------------------------------------------
#' @name SummaryCat
#' @title Summarize categorical variables
#'
#' @description Summarize a set of categorical variables by given group
#' variable/s.
#'
#' @usage
#' SummaryCat(data, quest, groups = NULL, na.value = "", export.tbl = FALSE)
#'
#' @param data a data frame containing at least the variables to analysis and to
#' group by
#' @param quest a string containing the name of the variable to be analyzed
#' @param groups a character vector containing the names of the variables to
#' group by (up to two grouping variables).
#' @param na.value a string defining another value to be counted as "missing" 
#' (i.e excluded from the valid percent). This is in addition to NA and "", 
#' which are counted as missing by default.
#' @param export.tbl Logical indicating if table should be exported to a .csv 
#' file
#'
#' @examples
#' data <- data.frame(
#'   group1 = sample(LETTERS[1:3], 100, replace = TRUE),
#'   group2 = sample(letters[1:5], 100, replace = TRUE),
#'   quest1 = sample(c("lower", "middle", "higher"), 100, replace = TRUE),
#'   quest2 = sample(c("Male", "Female"), 100, replace = TRUE)
#' )
#' 
#' data[sample(1:100, 4), "quest1"] <- NA_character_
#' data[sample(1:100, 4), "quest1"] <- ""
#' data[sample(1:100, 7), "quest2"] <- "Don't Know"
#' 
#' SummaryCat(data, "quest1")
#' SummaryCat(data, "quest2", "group1", na.value = "Don't Know")
#' 
#' quest <- grep("quest", colnames(data), value = TRUE)
#' groups <- grep("group", colnames(data), value = TRUE)
#' 
#' lapply(
#'   quest, 
#'   function(x) SummaryCat(data, x, groups, na.value = "Don't Know")
#' )
#' 
#' @export
## -----------------------------------------------------------------------------
SummaryCat <- function(data, quest, groups = NULL, na.value = "",
                       export.tbl = FALSE) {

  ########################### PREELIMNARIES AND QUOSURES #######################
  # Convert arguments to symbols
  quest <- dplyr::sym(quest)
  if (length(groups) > 1) {
    groups <- dplyr::syms(groups)
  } else if (length(groups) == 1) {
    groups <- dplyr::sym(groups) 
  }
  
  ########################## COMPUTE FOR TOTAL/LEVEL 1 #########################
  # Compute total case summary
  total.s.case <- data %>%
    { 
      if (length(groups) > 1) {
        dplyr::mutate(., !!groups[[2]] := "total") %>%
          dplyr::group_by(!!!groups)

      } else if (length(groups) == 1) {
        dplyr::mutate(., !!groups := "total") %>%
          dplyr::group_by(!!groups)
      } else .
    } %>%
    dplyr::summarise(
      question = as.character(quest),
      N = dplyr::n(),
      n.in = sum(
        nchar(!!quest) != 0 & !is.na(!!quest) & !!quest != na.value,
        na.rm = TRUE
      ),
      n.ex = sum(
        nchar(!!quest) == 0 | is.na(!!quest) | !!quest == na.value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  # Calculate total summary table and join case summary
  total.s.data <- data %>%
    { 
      if (length(groups) > 1) {
        dplyr::mutate(., !!groups[[2]] := "total") %>%
          dplyr::group_by(!!groups[[1]], !!quest)
      } else if (length(groups) == 1) {
        dplyr::mutate(., !!groups := "total") %>%
          dplyr::group_by(!!groups, !!quest)
      } else dplyr::group_by(., !!quest)
    } %>%
    dplyr::summarise(
      question = as.character(quest), 
      n = dplyr::n(), 
      .groups = "keep"
    ) %>%
    dplyr::full_join(., total.s.case) %>%
    dplyr::mutate(
      perc = n / N,
      valid.perc = dplyr::if_else(
        nchar(!!quest) != 0 & !is.na(!!quest) & !!quest != na.value,
        n / n.in, NA_real_
      )
    ) %>%
    dplyr::ungroup()
  
  ############################## COMPUTE FOR GROUP #############################
  
  # grouped case summary
  group1.s.case <- data %>%
    { 
      if (length(groups) > 1) { 
        dplyr::group_by(., !!!groups)
      } else if (length(groups) == 1) {
        dplyr::group_by(., !!groups)
      } else .
    } %>%
    dplyr::summarise(
      question = as.character(quest),
      N = dplyr::n(),
      n.in = sum(
        nchar(!!quest) != 0 & !is.na(!!quest) & !!quest != na.value,
        na.rm = TRUE
      ),
      n.ex = sum(
        nchar(!!quest) == 0 | is.na(!!quest) | !!quest == na.value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  # Grouped summary table and join case summary
  group1.s.data <- data %>%
    { 
      if (length(groups) > 1) { 
        dplyr::group_by(., !!!groups, !!quest)
      } else if (length(groups) == 1) {
        dplyr::group_by(., !!groups, !!quest)
      } else dplyr::group_by(., !!quest)
    } %>%
    dplyr::summarise(
      question = as.character(quest), 
      n = dplyr::n(), 
      .groups = "keep"
    ) %>%
    dplyr::full_join(., group1.s.case) %>%
    dplyr::mutate(
      perc = n / N,
      valid.perc = dplyr::if_else(
        nchar(!!quest) != 0 & !is.na(!!quest) & !!quest != na.value,
        n / n.in, NA_real_
      )
    ) %>%
    dplyr::ungroup()
  
  ############################### JOIN TOTAL AND GROUP #########################
  
  out.table <- dplyr::bind_rows(total.s.data, group1.s.data) %>%
    dplyr::distinct(.) %>%
    dplyr::select(
      question, dplyr::matches(as.character(groups)), 
      N, n.in, n.ex, !!quest,
      n, dplyr::everything()
    ) %>%
    dplyr::rename("categories" = !!quest)

  ############################### OUTPUT #######################################
  
  if (export.tbl == TRUE) {
    # Get file names
    date.export <- gsub("^\\d{1,2}", "", gsub("-", "", Sys.Date()))
    quest.name <- unique(dplyr::pull(out.table, question))
    quest.name <- gsub(
      "[[:punct:]]", "", gsub("(.{1,25})(.*$)", "\\1", quest.name)
    )
    file.name <- paste0(date.export, ".", quest.name, ".csv")
    utils::write.csv(out.table, paste0("./output_tables/", file.name))
    cat("\nSummary table exported to:\n", paste0("~/output_tables/", file.name))
  }
  
# Print in to console
  return(out.table)
}
