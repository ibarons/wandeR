################################################################################
## @Title: format_form_labels.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 04.11.2020
## @Last updated: 
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes
## See how to use lazy dots (...) to pass argument of SummaryCate/Num() without 
## defining them all. Also to pass for the CalcOutlier()
## 
## In GetForm() need to fix to extract also the Calculate questions... they seems
## to be excluded
## ----------------------------------------------------------------------------
#' @name SurveyExplore
#' @title Exploratory analysis for survey data
#'
#' @description Provide basic descriptive: frequencies for categorical and 
#' summary measures and outliers for numeric.
#'
#' @usage
#' SurveyExplore(db, form.path, groups = NULL, add.numeric = NULL, exclude = NULL,
#'  outliers = TRUE, outlier.ids = NULL, outlier.group = NULL, 
#'  outlier.method = "adjusted", file.name = "")
#'
#' @param db a data frame
#' @param form.path path to the Kobo form of the db
#' @param groups a character vector containing the names of the variables to
#' group by (up to two grouping variables) for the exploratory analysis.
#' @param add.numeric a string/character vector with the name of additional 
#' numeric variables that might have not been captured through Kobo form. See 
#' details
#' @param exclude a string/character vector of the variables to be excluded from
#' the exploratory analysis. Anything that is no excluded of numeric is considered
#' categorical
#' @param outliers logical to define if outliers should be calculated
#' @param outlier.ids a string/character vector of the variables to identify the
#' outliying cases
#' @param outlier.group a string/character vector of the variables by which to 
#' group for the detection of outliers
#' @param outlier.method a string/character vector to define the method to be used
#' for outlier detection. Choose between c("tukey", "adjusted")
#' @param file.name a string optional to add to the file name
#' 
#' @details 
#' The function automatically get as numeric variables defined as "Integer" or
#' "Calculate" in the Kobo form. Additional numeric variables should be given through
#' `add.numeric` argument.
#' 
#' Any variables that is no considered in `excluded` or `add.numeric` is considered
#' for the categorical exploratory analysis.
#' 
#' for outlier calculation method and details see documentation of CalcOutliers()
#'
#' @return a list of three elements with the tables for the categoric variables, 
#' the numeric variables and the outliers
#'
#' @examples
#' \dontrun{
#' 
#' # Get data and form path
#' db <- readxl::read_excel("../db/Survey_IDP_2010_data_xlm_sample2.xlsx", guess_max = 30000)
#' form.path <- "../db/Kobo_survey_IDP_2010.xlsx"
#' 
#' # Define vectors of variable names to input in function
#' add.numeric = c("NotAttendingSchool", "checkHoH", "checkMarried1", "ScholledAge",
#'   "Under18", "finalaverageincome", "NoOfHoH", "NoScholledAge", "NoUnder18", 
#'   "TotalIncomefromDemo", "NocheckMarried1")
#' 
#' exclude <- colnames(db)[grep("^_", colnames(db))]
#' exclude = c("a1", "apcode", "a10", "a11", "a12", "img", exclude)
#' 
#' outlier.ids = c("_uuid", "_id", "a2", "a8")
#' 
#' # Run function
#' exploratory <- SurveyExplore(db, form.path, groups = "a5", add.numeric = add.numeric,
#'   exclude = exclude, outlier.ids = outlier.ids, outlier.group = "a7",
#'   file.name = "HHS.IDP")
#' 
#'}
#'
#' @export
## -----------------------------------------------------------------------------

SurveyExplore <- function(db, form.path, groups = NULL, add.numeric = NULL,
                          exclude = NULL, outliers = TRUE, outlier.ids = NULL, 
                          outlier.group = NULL, outlier.method = "adjusted", 
                          file.name = "") {

  ######################### GET VARIABLES OF ANALYSIS ##########################
    
  # Get form and extract numerical variables (integer & calculate)
  form <- R.HNAP::GetForm(form.path)
  
  numerical <- c(
    dplyr::pull(form$form.quest[
      which(form$form.quest$type %in% c("integer", "int", "calculate")), "q.name"
    ]), 
    add.numeric
  )
  
  # Get relevant variable names for the given db
  numerical <- db %>%
    dplyr::select(dplyr::any_of(numerical), -dplyr::any_of(exclude)) %>% 
    colnames(.)
  
  categorical <- db %>%
    dplyr::select(
      -dplyr::any_of(numerical), -dplyr::any_of(exclude), -dplyr::any_of(groups)
    ) %>% 
    colnames(.)
  
  cat.outlier <- db %>% # detect categorical which are coded as numeric to check with outlier for out of constrain values
    dplyr::select(
      -dplyr::any_of(numerical), -dplyr::any_of(exclude), -dplyr::any_of(groups)
    )
    
   i <- which(sapply(cat.outlier, function(x) !any(grepl("[A-z]+", x))))
  
   cat.outlier <- colnames(cat.outlier[, i])
  
  ############################# EXPLORATORY ANALYSIS ###########################
  
  #CATEGORICAL VARIABLES
  categorical.tbls <- lapply(categorical, function(x) {
    
    tbl <- suppressMessages(R.HNAP::SummaryCat(db, x, groups = groups))
    
    var.name <- gsub( # trunc var name for file name
      "[^[:alnum:][:space:]_\\.]", "", gsub("(.{1,25})(.*$)", "\\1", x)
    )

    wb <- suppressMessages(R.HNAP::ExportXLSX(list(categorical = list(tbl = tbl))))
    
    filename <- paste0(R.HNAP::DateStamp("_"), file.name, "categoric_", var.name, ".xlsx")
    openxlsx::saveWorkbook(wb, paste0("output_tables/", filename), overwrite = TRUE)
    
    cat("\nOutput Categorical tbl", var.name, "into ~/output_tables/...")
    
    tbl

  })
  categorical.tbls <- purrr::set_names(categorical.tbls, categorical)
  
  # NUMERICAL VARIABLES
  numerical.tbls <- lapply(numerical, function(x) {

    tbl <- suppressMessages(R.HNAP::SummaryNum(db, x, groups = groups))
    
    var.name <- gsub( # trunc var name for file name
      "[^[:alnum:][:space:]_\\.]", "", gsub("(.{1,25})(.*$)", "\\1", x)
    )

    wb <- suppressMessages(R.HNAP::ExportXLSX(list(numerical = list(tbl = tbl))))
    
    filename <- paste0(R.HNAP::DateStamp("_"), file.name, "numeric_", var.name, ".xlsx")
    openxlsx::saveWorkbook(wb, paste0("output_tables/", filename), overwrite = TRUE)
    
    cat("\nOutput Numerical tbl", var.name, "into ~/output_tables/...")
    
    tbl
    
  })
  numerical.tbls <- purrr::set_names(numerical.tbls, paste0(numerical, "_admin"))
    
  ################################ OUTLIERS ####################################
  
  if (outliers == TRUE) {
  
    outliers <- db %>% 
      dplyr::select(
        dplyr::any_of(outlier.ids), dplyr::any_of(outlier.group), 
        dplyr::any_of(numerical), dplyr::any_of(cat.outlier)
      ) %>%
      {
        if (!is.null(outlier.group)) {
          dplyr::group_by(., !!rlang::sym(outlier.group))
        } else .
      } %>%
      dplyr::mutate(
        dplyr::across(
          c(any_of(numerical), dplyr::any_of(cat.outlier)), 
          ~ as.numeric(as.character(.))
        ),
        dplyr::across(
          c(any_of(numerical), dplyr::any_of(cat.outlier)),
          list(
            lower = ~ R.HNAP::CalcOutliers(
              ., method = "adjusted", fence = 3, na.rm = TRUE
            )[[1]][[1]][1],
            upper = ~ R.HNAP::CalcOutliers(
              ., method = "adjusted", fence = 3, na.rm = TRUE
            )[[1]][[1]][2],
            out = ~ as.numeric(R.HNAP::CalcOutliers(
              ., method = "adjusted", fence = 3, na.rm = TRUE
            )[[1]][[2]])
          )
        )
      ) %>%
      dplyr::rename_with(
        ~ gsub("$", "_value", .), 
        c(dplyr::any_of(numerical), dplyr::any_of(cat.outlier))
      ) %>%
      tidyr::pivot_longer(
        -c(dplyr::any_of(outlier.group), dplyr::any_of(outlier.ids)),
        names_to = c("question", ".value"),
        names_pattern = "(.+_*)_(.+)"
        #values_drop_na = TRUE
      ) %>% 
      dplyr::filter(out == 1, !is.na(value), !(lower == 0 & upper == 0)) %>%
      dplyr::mutate(
        distance = dplyr::if_else(value > upper, value - upper, lower - value)
      ) %>%
      dplyr::select(-out) %>%
      dplyr::ungroup()
    
    
    wb <- suppressMessages(R.HNAP::ExportXLSX(list(outliers = list(outliers = outliers))))
    openxlsx::saveWorkbook(
      wb, paste0("output_tables/", R.HNAP::DateStamp(paste0(file.name, "_outliers.xlsx"))),
      overwrite = TRUE
    )
    cat("\nOutput Outliers tbl into ~/output_tables/...\n")
    
  }
  
  return(list(
    categorical = categorical.tbls, numerical = numerical.tbls, outliers = outliers
  ))
}

  
  
  