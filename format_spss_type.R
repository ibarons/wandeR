################################################################################
## @Title: format_spss_type.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 23.02.2020
## @Last updated: 
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes
## 
## ----------------------------------------------------------------------------
#' @name SPSSType
#' @title Convert labelled vectors into R types 
#'
#' @description Convert labelled vectors (dbl+lbl) into factor, numeric or 
#' character .
#'
#' @usage
#' SPSSType(db, form)
#'
#' @param db data frame with dbl+lbl vectors to correct
#' @param form data frame correcpondign to the ODK form as imported by
#' `R.HNAP::GetForm(form.path)[["form"]]`
#'
#' @details
#' Factors are convected with haven::as_factor()
#'
#' @returns
#' The input data frame with dbl+lbl as factors, with integers as numeric and with
#' strings as characters
#'
#' @export
## -----------------------------------------------------------------------------

SPSSType <- function(db, form) {
  
  # Get multiple varaible names
  multiple.vars <- form %>%
    dplyr::filter(type == "select_multiple") %>%
    dplyr::pull(q.name)
  
  multiple.dummy <- colnames(db)[
    grep(paste0(multiple.vars, ".+", collapse = "|"), colnames(db))
  ]
  
  # Get single variables names
  i.label <- colnames(db)[which(
    colnames(db) %in% form$q.name[which(form$type %in% c("select_one"))]
  )]
  i.label <- c(i.label, multiple.dummy)
  
  # Exclude Others specifications
  if (length(grep("_[Oo]ther$", i.label)) != 0) {
    i.label <- i.label[-grep("_[Oo]ther$", i.label)]
  }

  # Get inetger variable names
  i.num <- colnames(db)[which(
    colnames(db) %in% form$q.name[which(form$type %in% c("integer", "int"))]
  )]
  
  # Correct formats
  db <- db %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(i.label), haven::as_factor),
      dplyr::across(!dplyr::all_of(i.label), haven::zap_labels),
      dplyr::across(dplyr::all_of(i.num), ~ as.numeric(as.character(.)))
    )
  
  return(db)
  
}






