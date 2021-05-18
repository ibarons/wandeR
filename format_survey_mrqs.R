################################################################################
## @Title: analysis_survey_table.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 22.01.2021
## @Last updated: 
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes:
## 
## do not touch the original name. Do not collapse with _. LEt them equal so you 
## can easily regex witht he ODK form
## 
## ----------------------------------------------------------------------------
#' @name SurveyMRQS
#' 
#' @title Restructure the data for Multi-Response Questions Sets analysis
#'
#' @description Given a MRQS, e.g 3 different question "Q.Priorty1", "Q.Priority2",
#'  and "Q.Priority3" asking prioritise the same list of answers. To analyse 
#'  taking into account all the answers for the three questions, the data frame 
#'  need to be wrangled. This function does that and let you ready for analysis!
#' 
#' @usage
#' SurveyMQRS(db, dep.vars, ..., .indep.vars = "^\\s{0}$", .prefix = "MRQS",
#' .recode_fct = TRUE, .form_check = NULL, .fix_nchar = 15)
#' 
#' @param db data frame with variables to be analysed and admin. level to be 
#' disaggregated by.
#' @param dep.vars a string ro character vector of the MRQS variables. 
#' If string, it can be a regex or the name of one variable. If a character 
#' vector, should be names of variables
#' @param ... any other variables that wants to be kept in the data base for 
#' ulterior use (e.g. uuid, weights, etc.)
#' @param .indep.vars a string or character vector with the names of 
#' independent. Same result as for the `...` but accepts regex for selection
#' @param .recode_fct logical should the new created variables be recoded as a 
#' factor with 2 levels "Yes"/"No"
#' @param .prefix a string to add as prefix for the newly created variables
#' @param .form_check string referring to a path were to find the Kobo (ODK) form
#'  It allow to check that the number of newly created variables maches the number
#'  of answers stated in the form (path is passed to `R.HNAP::GetForm()`)
#' @param .fix_nchar integer defining number of character to keep when the new
#'  variable names is very long. Defaults to 15, but sometime might not be 
#'  enough to keep a unique name, in which case will throw an error
#'
#' @details
#' 
#' The function automatically recognise the variables with the following regex as
#' admin leve, thus it keeps them in the output data base:
#' "^admin\\d[NP]|^location[NP]|^AoC$"
#' 
#' A common issue in this type of MRQS is that an interviewee has answered Other
#'  to more than one of the questions. In this cases, one single cell int he newly
#' `Other` created variable would contain 2 values, causing th pivot_wider display
#'  the warning of `column-list`. This function automatically coerce all values 
#'  to 0/1 and displays a warning.
#' 
#' `dep.vars` and `.indep.vars` when given a character vector are processed to 
#' convert it to a regex expression:
#' 
#' if (length(. > 1)){. <- paste0("^", ., "$", collapse = "|")}  
#' 
#' @return a data frame where the dep.vars have been pivoted so each of its 
#' answers are now new columns with 1/0 as answers. These new columns are returned
#' as factors "yes/no" if specified in arg. `.recode_fct`
#' 
#' @examples
#' db <- data.frame(
#'   id = 1:100,
#'   w = sample(1:5, 100, replace = TRUE),
#'   admin1Name = sample(c("SP", "PH", "TU", "FR", "SY"), 100, replace = TRUE),
#'   dep.var1 = sample(LETTERS[1:5], 100, replace = TRUE),
#'   dep.var2 = sample(LETTERS[1:5], 100, replace = TRUE),
#'   dep.var3 = sample(LETTERS[1:5], 100, replace = TRUE),
#'   indep.var1 = sample(letters[1:5], 100, replace = TRUE),
#'   indep.var2 = sample(letters[1:5], 100, replace = TRUE)
#' )
#' 
#' # Dependant variables as regex; Output new vars as factors
#' SurveyMQRS(
#'   db, dep.vars = "^dep\\.var\\d$", id, w, indep.var1, indep.var2,
#'    .prefix = "Factor"
#' )
#' 
#' # Output new vars as factors, discard `id` and independent variables also as regex 
#' SurveyMQRS(
#'   db, dep.vars = "^dep\\.var\\d$", .indep.vars = "^indep\\.var\\d$",
#'   .prefix = "Numeric", .recode_fct = FALSE
#' ) # warning, output nrow() does not match with input nrow()
#' 
#' # Check output with Kobo (ODK) form
#' \dontrun{
#'   form.path <- "../folder/file.xlsx"
#'   
#'  SurveyMQRS(
#'     db, dep.vars = "^dep\\.var\\d$", id, .indep.vars = "^indep\\.var\\d$",
#'     .prefix = "Numeric", .form_check = form.path
#'   )
#' }
#' 
#' @export
## -----------------------------------------------------------------------------

SurveyMQRS <- function(db, dep.vars, ..., .indep.vars = "^\\s{0}$", .prefix = "MRQS",
                       .recode_fct = TRUE, .form_check = NULL, .fix_nchar = 15) {
  
  ############################# PREELIMINARIES #################################

  if (all(class(db) != "data.frame")) {db <- as.data.frame(db)} # Common use of the fx takes a survey.design object
  
  if (length(dep.vars) > 1) {
    dep.vars <- paste0("^", dep.vars, "$", collapse = "|")
  } 
  if (length(.indep.vars) > 1) {
    .indep.vars <- paste0("^", .indep.vars, "$", collapse = "|")
  }
  
  dots <- rlang::enquos(...)
  
  ############################ PIVOT DEP VARS ##################################
  
  # Pivot db to take dep.vars answers as new variables
  db.mrqs <- db %>%
    dplyr::select(.,
      dplyr::matches("^admin\\d[NP]|^location[NP]|^AoC$"), ...,
      dplyr::matches(.indep.vars), dplyr::matches(dep.vars)
    ) %>%
    tidyr::pivot_longer(
      dplyr::matches(dep.vars), names_to = "priority", values_to = "answers"
    ) %>%
    dplyr::mutate(value = 1) %>%
    dplyr::select(-priority) %>%
    dplyr::filter(!is.na(answers)) %>% 
    tidyr::pivot_wider(names_from = answers, values_from = value, values_fn = {sum}) %>%
    dplyr::mutate(., dplyr::across(
      -c(dplyr::matches("^admin\\d[NP]|^location[NP]|^AoC$"), dplyr::matches(.indep.vars), ...),
      ~ tidyr::replace_na(., 0)
    ))
  
  
  ############################## CHECK OUTCOME #################################
  
  # Check same number of row in both input and output df
  if (nrow(db) != nrow(db.mrqs)) {
    warning("Number of rows in the output df differs from the input df. Check this is correct")
  }
  
  # Check if all answers (as per Kobo-ODK form) have returned a new column
  if (!is.null(.form_check)) {
    
  form <- R.HNAP::GetForm(form.path)$form
  
  form.var.names <- unique(
    dplyr::pull(form[which(grepl(dep.vars, form$q.name)), "a.label"])
  )
  
    if (!(all(form.var.names %in% colnames(db.mrqs)))) {
      warning("Some dep.vars answers seems have not been transformed to new variables (columns). Check this is correct")
    }
  }
  
  #Check and warn all values are 1/0
  
  check.vals <- db.mrqs %>%
    dplyr::select(., 
      -c(dplyr::matches("^admin\\d[NP]|^location[NP]|^AoC$"), dplyr::matches(.indep.vars), ...)
    )
  
  if (any(check.vals > 1)) {
    error.val <- sapply(check.vals, function(x) sum(x > 1))
    error.val[which(error.val >= 1)]
    
    warning(
      "In the new variables, ", sum(check.vals > 1), " value/s was not 0/1 (no/yes) and have been coerced. Check if this is correct\n",
      "Affected new variables are: ",
      paste(names(which(sapply(check.vals, function(x) sum(x > 1)) >= 1)), collapse = ";")
    )
    
    db.mrqs <- db.mrqs %>%
      dplyr::mutate(., dplyr::across(
        -c(dplyr::matches("^admin\\d[NP]|^location[NP]|^AoC$"), dplyr::matches(.indep.vars), ...),
        ~ dplyr::if_else(. > 1, 1, .) 
      ))
  }
  
  
  ################################## FIX NAMES #################################
  
  name.fix.chars <- paste0("(.{1,", .fix_nchar, "})(.*)")
  
  db.mrqs <- db.mrqs %>%
    {
      if (.recode_fct == TRUE) {
        dplyr::mutate(., dplyr::across(
          -c(dplyr::matches("^admin\\d[NP]|^location[NP]|^AoC$"), dplyr::matches(.indep.vars), ...),
          ~ forcats::fct_recode(as.character(.), "Yes" = "1", "No" = "0")
        ))
      } else .
    } %>%
    dplyr::rename_with(.,
      ~ gsub("\\s|[[:punct:]]", "_", gsub(name.fix.chars, "\\1", .)),
      -c(dplyr::matches("^admin\\d[NP]|^location[NP]|^AoC$"), dplyr::matches(.indep.vars), ...),
    ) %>%
    dplyr::rename_with(.,
      ~ paste0(.prefix, "_", .),
      -c(dplyr::matches("^admin\\d[NP]|^location[NP]|^AoC$"), dplyr::matches(.indep.vars), ...),
    )
  
  return(db.mrqs)
  
}