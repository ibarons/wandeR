################################################################################
## @Title: kobo_get_data.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 24.02.2020
## @Last updated:
## @Status: Active
## ------------------------------------------------------------------------------
## Notes:
##
## The data should be encoded as UTF-8 to capture arabic characters
##
## ----------------------------------------------------------------------------
#' @name GetData
#' @title Get data from Kobo server forms
#'
#' @description Get the data from specified Kobo forms, given a server and 
#' credentials to access it.
#'
#' @usage
#' GetData(assessment = "*", server.url = "http://kc.kobotoolbox.org",
#' kobo.user, kobo.pass, export.data = FALSE)
#'
#' @param assessment string in form of regular expresion defining the pattern
#' to be searched as form name. If empty, will download all forms found
#' @param server.url URL for kobo server, of the type: http://kc.~
#' @param kobo.user user name for th given server
#' @param kobo.pass password for the given user
#' @param export.data Export fetched forms to .csv file? defaults FALSE
#'
#' @return List of data frames fetched from Kobo server to the defined Kobo
#' forms.
#'
#' @examples
#' \dontrun{
#' list.data <- GetData("MNM"m kobo.user = "user", kobo.pass = "password")
#'
#' # Explore fetched forms
#' lapply(list.data, head)
#'
#' # Convert list to data frame (e.g. if assessment is split in multiple forms)
#' data <- dplyr::bind_rows(list.data)
#' }
#'
#' @export
## ------------------------------------------------------------------------------
GetData <- function(assessment = "*", server.url = "http://kc.kobotoolbox.org",
                    kobo.user, kobo.pass, export.data = FALSE) {
  
  auth <- httr::authenticate(kobo.user, kobo.pass)
  
  # GetForm for form ID
  forms.char <- R.HNAP::GetForms(server.url, kobo.user, kobo.pass)
  data.url <- forms.char[["list.api.urls"]][["data"]]
  print(forms.char[["forms.description"]])

  # Filter selected assessments
  assessments <- forms.char[["forms.description"]] %>%
    dplyr::filter(grepl(assessment, .[["title"]]))

  # Warning and exclude if selected DB for download with 0 submission
  if (any(assessments$num_of_submissions %in% "0")) {
    warning(
      "\nThe following forms have 0 submission; have not been downloaded:\n",
      paste(capture.output(print(
        assessments[which(assessments$num_of_submissions == "0"), c(1:3, 6)]
      )), collapse = "\n")
    )
    assessments <- dplyr::filter(assessments, !num_of_submissions == "0")
  }

  # Create storage obj for for-loop
  list.assessments <- vector("list", nrow(assessments))
  names(list.assessments) <- assessments$title
  data.url <- c()
  # Get data in .csv for all filtered db
  for (i in 1:nrow(assessments)) {
    
    data.url[i] <- paste0(
      forms.char[["list.api.urls"]][["data"]], "/", assessments[i, "formid"], ".csv"
    )
    
    cat("\n", data.url[i], "\n")
    
    get.data <- httr::GET(data.url[i], auth)
    
    if (get.data$status_code != 200) {
      warning(
        assessments$title[i], "-", assessments$formid[i], "\n",
        "Access to Data failed with status code:", get.data$status_code
      )
      next
    }
    
    cat("Succesful access to Data.\n")
    cat(paste0(assessments$formid[i], "-", assessments$title[i], "\n"))

    data <- httr::content(get.data, "raw", encoding = "UTF-8")
    list.assessments[[i]] <- as.data.frame(
      readr::read_csv(data, col_types = readr::cols(), guess_max = 10^6)
    )
  }
  
  # Print downloaded forms
  cat("\nThe following are the downloaded forms:\n")
  print(assessments)
  
  # Export individual data bases
  if (export.data == TRUE) {
    
    cat("\nExported Single DDBB (.csv):\n")
    date.export <- gsub("^\\d{1,2}", "", gsub("-", "", Sys.Date()))
    
    for (i in 1:length(list.assessments)) {
      file <- paste0(
        "output_ddbb/", date.export, ".", names(list.assessments[i]),
        ".csv"
      )
      utils::write.csv(list.assessments[[i]], file = file, fileEncoding = "UTF-8")
      print(file)
    }
  }
  
  invisible(list.assessments)
}
