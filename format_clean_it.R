################################################################################
## Title: format_clean_it.R
## Author: Julian Ibarguen
## Contact: jibarguen@iom.int
## Date created: 08.06.2020
## Last updated:
## Status: Maturing
## ------------------------------------------------------------------------------
## Notes:
## Argument to choose what to clean: ws, - or , separators.
##
## Explore possibility of using purrr:reduce2 for a more optimized and efficient
## code
##
## Would be to transform NAs in numeric into 0 a desired behaviour?
## dplyr::mutate_if(is.numeric, ~ dplyr::if_else(is.na(.), 0, as.numeric(.)))
##
## ----------------------------------------------------------------------------
#' @name CleanIt
#' @title Clean format for variables
#'
#' @description Remove `,` thousand separators, converts cell with only `-` to 0,
#' remove leading and trailing spaces.
#'
#' @usage
#' CleanIt(db, variables = "all", regex = FALSE)
#'
#' @param db the kobo downloaded data frame (moblity data only)
#' @param variables character vector of variables in the data frame to clean.
#' Can be a given selection of variables or `"all"`
#' @param regex should the `variables`` argument correspond to a regular
#' expression to match with colum names? logical
#'
#' @details Applies utils::type.convert() to the input variable.
#'
#' @return a clean data frame as per the description
#'
#' @examples
#' # Create dirty data frame
#' df <- tibble::tibble(
#'   numeric = c(" 05,000$", "$ 2,000", " 100 ", NA, "$1,000,000", "90,000 $"),
#'   numeric2 = c("50", NA, "-", " - ", " -", "- "),
#'   alphanum = c("a,10", "$b,1 ", "1,c", NA, "a,1,2 $", " 1,2,b"),
#'   alphanum2 = c("10", "a", "20", " n/a ", "NA", "3c"),
#'   character = c(" leading.space", "NA", "trailing.space ", " both ", "none", "n/a "),
#'   missing = rep(NA, 6)
#' )
#' # Clean all variables
#' CleanIt(df)
#' # Clean specific variables
#' CleanIt(df, c("numeric", "character"))
#' # Clean with regex for variable selection
#' CleanIt(df, variables = "[^z]", regex = TRUE)
#' @export
## -----------------------------------------------------------------------------
CleanIt <- function(db, variables = "all", regex = FALSE) {
  # Quosures
  if (regex == FALSE) {
    if (all("all" %in% variables)) {
      variables <- dplyr::vars(dplyr::everything())
    } else {
      variables <- dplyr::vars(!!!rlang::syms(variables))
    }
  } else if (regex == TRUE) {
    variables <- dplyr::vars(!!!rlang::syms(
      grep(variables, colnames(db), value = TRUE)
    ))
  }
  # Clean the given df
  cleaned <- dplyr::mutate_at(
    db,
    variables,
    ~ trimws(
      gsub(
        "(^\\s*-\\s*$)|^\\s*_\\s*$",
        0,
        gsub(
          ",(?=\\d{3})|\\$|%.*$",
          "",
          .,
          perl = TRUE
        )
      )
    )
  ) %>%
    dplyr::mutate_at(
      variables, 
      ~ utils::type.convert(., na.strings = c("n/a", "NA"), as.is = TRUE)
    )
  return(cleaned)
}



# df <- tibble::tibble(
#   numeric = c(" 05,000$", "$ 2,000", " 100 ", NA, "$1,000,000", "90,000 $"),
#   numeric2 = c("50", NA, "-", " - ", " -", "- "),
#   alphanum = c("a,10", "$b,1 ", "1,c", NA, "a,1,2 $", " 1,2,b"),
#   alphanum2 = c("10", "a", "20", " n/a ", "NA", "3c"),
#   character = c(" leading.space", "NA", "trailing.space ", " both ", "none", "n/a "),
#   missing = rep(NA, 6)
# )

