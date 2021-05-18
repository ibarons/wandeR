################################################################################
## @Title: kobo_get_forms.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 19.02.2020
## @Last updated:
## @Status: Active
## ------------------------------------------------------------------------------
## Notes:
##
## Function name conflicts (except by the plural) with format_get_form.R. Replace
## by GetAssessments()
##
## ----------------------------------------------------------------------------
#' @name GetForms
#' @title Get details of forms in the Kobo server
#'
#' @description Get forms details
#'
#' @usage
#' GetForms(server.url, kobo.user, kobo.pass)
#'
#' @param server.url URL for kobo server, of the type: http://kc.~
#' @param kobo.user user name for th given server
#' @param kobo.pass password for the given user
#'
#' @return A a data frame containing forms key details.
#'
#' @examples
#' \dontrun{
#' GetForms(server.url, kobo.user, kobo.pass)
#' }
#'
#' @export
## ------------------------------------------------------------------------------
GetForms <- function(server.url, kobo.user, kobo.pass) {
  
  if(grepl("//kf\\.", server.url)) {
    warning("Check you entered the 'http://kc.~' url format, not the 'http://kf.~'")
  }
  
  cat("\nAccess with user:", kobo.user, "\n")
  auth <- httr::authenticate(kobo.user, kobo.pass)
  
  # form API basic url
  api.url <- paste0(server.url, "/api/v1/")
  
  # Get list of API URLs
  get.api <- httr::GET(api.url, auth)
  
  if (get.api$status_code != 200) {
    stop("\nAccess to URLs failed with status code: ", get.api$status_code, "\n")
  }
  
  cat("Succesful access to Server\n")
  
  list.api.urls <- rawToChar(get.api$content)
  list.api.urls <- jsonlite::fromJSON(list.api.urls)
  list.api.urls[["data"]] <- paste0(api.url, "data")
  
  # Get API urls for forms
  forms.url <- list.api.urls[["forms"]]
  
  # Access URL and get forms description
  get.forms <- httr::GET(forms.url, auth, httr::progress())
  
  if (get.forms$status_code != 200) {
    stop("\nAccess to Forms failed with status code: ", get.forms$status_code, "\n")
  }
  
  cat("\nSuccesful access to Forms:\n")
  
  forms.description <- rawToChar(get.forms$content)
  forms.description <- jsonlite::fromJSON(forms.description)
  forms.description <- forms.description %>%
    dplyr::select(
      title, formid, num_of_submissions, downloadable, date_created,
      date_modified
    ) %>%
    dplyr::mutate(
      date_created = as.Date(date_created),
      date_modified = as.Date(date_modified)
    )

  return(list(list.api.urls = list.api.urls, forms.description = forms.description))

}
