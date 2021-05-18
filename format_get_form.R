################################################################################
## @Title: format_get_form.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 21.07.2020
## @Last updated: 03.09.2020
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes
## In GetForm() need to fix to extract also the Calculate questions... they seems
## to be excluded. This affects SurveyExplore()
##
## The trimws() in form <- . seems not to work well
##
## ----------------------------------------------------------------------------
#' @name GetForm
#' @title transform an ODK form into a usable R object
#'
#' @description Given a Kobo form, it merges questions and answers in a single
#' data frame to allow for relevant joining with other data frames.
#'
#' @usage
#' GetForm(form.path, arabic = FALSE)
#'
#' @param form.path the path to a XLSX file containing the Kobo form. At least,
#' the file need to:
#' - Be a XLS or XLSX file
#' - Contain a "survey" sheet with fields: type, name and label.*[Ee]ng.*$
#' - Contain a "choices" sheet with fields: list_name, name, label.*[Ee]ng.*$.
#' Advice to remove admin from choices, or rather make sure all admin are in the
#' choices sheet
#' @param arabic logical. shoudl arabic labels be included
#'
#' @details
#' This function is mainly helper for R.HNAP::LabelIt(), although it cna be used
#' by its own also.
#'
#' @examples
#' \dontrun{
#' # Code a labeled db
#' GetForm("wd/folder/file.xls")
#' }
#'
#' @export
## -----------------------------------------------------------------------------
GetForm <- function(form.path, arabic = FALSE) { 
  
  ########################### IMPORT ODK FORM ##################################
  
  # import questions sheet
  form.quest <- readxl::read_excel(form.path, sheet = "survey", guess_max = 2000) %>%
    {
      if (arabic == TRUE) {
        dplyr::select(., 
          type, "q.name" = name, "q.label" = dplyr::matches("label.*[Ee]ng.*$"),
          "q.label.ara" = dplyr::matches("label.*[Aa]ra.*$")
        )
      } else {
        dplyr::select(., 
          type, "q.name" = name, "q.label" = dplyr::matches("label.*[Ee]ng.*$")
        )
      }
    } %>%
    tidyr::separate(
      type, c("type", "list_name"),
      sep = "\\s+", fill = "right"
    ) %>%
    dplyr::filter(
      # !grepl("^ki", q.name),
      !grepl("^begin_|^end_|calculate|note", type)
    )
  
  # import answer sheet
  form.answ <- readxl::read_excel(form.path, sheet = "choices", guess_max = 5000) %>%
    {
      if (arabic == TRUE) {
        dplyr::select(.,
          "list_name" = dplyr::matches("list.*name"),
          "a.name" = name, "a.label" = dplyr::matches("label.*[Ee]ng.*$"),
          "a.label.ara" = dplyr::matches("label.*[Aa]ra.*$")
        )
      } else {
        dplyr::select(.,
          "list_name" = dplyr::matches("list.*name"),
          "a.name" = name, "a.label" = dplyr::matches("label.*[Ee]ng.*$")
        )
      }
    }
  
  # Warn of unmateched choices
  unmatched <- dplyr::left_join(form.answ, form.quest, by = "list_name") %>%
    dplyr::filter(is.na(q.name))
  
  if (nrow(unmatched) != 0) {
    warning(
      "\nThe following answer choices are not matched to a question and are excluded:\n",
      paste(as.character(unique(unmatched$list_name)), collapse = "; ")
    )
  }
  
  # Join questions and answers
  form <- dplyr::left_join( # before full_join(answ, quest)
    form.quest, form.answ,
    by = "list_name"
  ) %>%
    #dplyr::rename_with(~ paste0("q.", output), dplyr::matches("\\.y$")) %>%
    #dplyr::select(-dplyr::matches("\\.x$")) %>%
    dplyr::filter(
      !grepl("^ki", q.name), # remove admin cause is only from one gov
      !is.na(type),
      !grepl("^begin_|^end_|calculate", type),
      # !grepl("^ki_|^gender$", list_name)
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("label$")),
      ~ trimws(.) # seems not to work
    ) %>%
    dplyr::distinct(.)
  
  return(list(form = form, form.quest = form.quest, form.answ = form.answ))
}