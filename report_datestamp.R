################################################################################
## @Title: format_form_labels.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 04.11.2020
## @Last updated:
## @Status: Active
## -----------------------------------------------------------------------------
## Notes
## ----------------------------------------------------------------------------
#' @name DateStamp
#' @title Formatted today's date
#'
#' @description Format date of today for stamping in output files
#'
#' @param end a string/character to paste at the end of the data
#'
#' @usage
#' DateStamp(end = NULL)
#'
#' @examples
#' DateStamp("_output.file.csv")
#' 
#' @export
## -----------------------------------------------------------------------------

DateStamp <- function(end = NULL) { 
  paste0(gsub("^\\d{1,2}", "", format(Sys.Date(), "%Y%m%d")), "_", end)
}
