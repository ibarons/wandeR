################################################################################
## @Title: merge_assessments.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 24.02.2020
## @Last updated: 05.05.2020
## @Status: Active
## ------------------------------------------------------------------------------
## Notes:
## Create argument to decide if merge columns or rows (row/col_bind)
##
## Explore how to remove group names from variable name ensuring that repeated
##  fields, which rely in group name to avoid name duplication, does not produce
##  duplicated names
## ----------------------------------------------------------------------------
#' @name Merge
#' @title Merge assessmenents in a single data frame
#'
#' @description Merge multiple assessments fetched from Kobo server in list form
#' into a single data frame. Usuful when the same assessment have been split in
#' multiple Kobo forms
#'
#' @usage
#' Merge(list.assessments, export.csv = TRUE)
#'
#' @param list.assessments a list of data frames
#' @param export.csv export output data frame into a .csv file
#'
#' @details
#' It remove any fully duplicated cases. This is cases which  all its variables
#' are the same.
#'
#' Forms fetched from Kobo tend to mark missing values as "n/a". The function
#' check for any "n/a" ocurrance and convert it into "NA" for easier management
#' in R.
#'
#' @return A data frame composed of all the unique cases comprised in the
#' inputted list of data frames
#'
#' @examples
#' \dontrun{
#' Merge(list.mnm)
#' }
#'
#' @export
## ------------------------------------------------------------------------------
Merge <- function(list.assessments, export.csv = TRUE) {

  # Check there are no NULL assessments and produce warnign
  null.assess <- lapply(list.assessments, is.null)
  if (any(null.assess == TRUE)) {
    warning(
      "\nThe following forms with no data (NULL) have been removed from the downloaded db:\n",
      paste0(names(null.assess)[which(null.assess == TRUE)])
    )
    list.assessments <- list.assessments[-which(null.assess == TRUE)]
  }
  # Get assessment name
  name.db <- gsub(
    "_", ".",
    gsub(
      "_[^_]+$", "",
      names(list.assessments[1])
    )
  )
  # Merge assesssmets data for different location
  assessments <- vector("list", length(list.assessments))
  names(assessments) <- names(list.assessments)
  for (i in 1:length(list.assessments)) {
    assessments[[i]] <- list.assessments[[i]] %>%
      dplyr::mutate_all(as.character)
  }
  assessment.db <- dplyr::bind_rows(assessments) %>%
    dplyr::rename_at(
      dplyr::vars(-dplyr::contains("D_KEY_INFORMANT_DET_ther_please_specify")),
      ~ make.unique(gsub(".*/", "", .))
    ) %>%
    dplyr::mutate(db.name = name.db) %>%
    dplyr::select(db.name, dplyr::everything())
  # Identify and warn on duplicates
  assess.full.duplicates <- assessment.db %>%
    dplyr::filter(duplicated(.)) %>%
    dplyr::select(grep(
      "(Date_of)|(admin1)|(admin4)|(_uuid)",
      colnames(assessment.db)
    ))
  warning(
    "Assessment data base have ",
    paste(
      nrow(assess.full.duplicates),
      "duplicated cases. They have been removed from DB\n",
      paste(capture.output(print(tibble::as_tibble(assess.full.duplicates))),
        collapse = "\n"
      )
    )
  )
  # Clean DB
  assessment.db <- assessment.db %>%
    dplyr::distinct() %>%
    R.HNAP::CleanIt(.)
  ############################## WRTIE OUTPUT DATA BASES #######################
  if (export.csv == TRUE) {
    cat("\nExported Merged DDBB (.csv):\n")
    ## Enconding in UTF-8, though still need to be re-encoded when opening .csv
    ## Complete MNM DB
    date.export <- gsub("^\\d{1,2}", "", gsub("-", "", Sys.Date()))
    file.assessment <- paste0("output_ddbb/", date.export, "_", name.db, ".csv")
    utils::write.csv(assessment.db,
      file = file.assessment, row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    print(file.assessment)
  }
  invisible(assessment.db)
}
