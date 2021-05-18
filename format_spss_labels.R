################################################################################
## @Title: format_spss_labelled.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 09.11.2020
## @Last updated: 22/02/2021
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes
## 
## Make if statement so if input label, automatically get names with LabeltIt()
## ; else, if input names, automatically gets labels with Label
## 
## Check if we cna export with the correct variable Type for SPSS. now ALL are 
## exported String (character)
## 
## ----------------------------------------------------------------------------
#' @name SPSSLabelled
#' @title Convert to SPSS (.sav) format (with labels) 
#'
#' @description Take a data base named with code and a data base named with and 
#' output a SPSS format (.sav) data base with labels  .
#'
#' @usage
#' SPSSLabelled(db.code, db.label, form.path, export.file = "spss.db")
#'
#' @param db.code a data frame with variable and value names coded (`names` field 
#' in Kobo (ODK) form)
#' @param db.label a data frame with variable and value names coded (`label` field 
#' in Kobo (ODK) form)
#' @param form.path string containing the path to Kobo (ODK) form
#' @param export.file string witht he name of the file to output. Note format 
#' file (.sav) is set automatically; Also date stamp of format 201109_
#'
#' @details
#' Exchange between codes and labels are based on Kobo (ODK) forms as input by 
#' R.HNAP::GetForm()
#' 
#' Admin colnames in db.code should be admin[1-4] from Governorate to Location. 
#' Note usually the given Kobo form is just for one governorate, thus the admin
#' codes and labels are removed form the Kobo and input for WoS here.
#' 
#' Admin variables are split, from one side adminPcode from the other adminName,
#' rather than value (Pcode) + label (Name)
#' 
#' @return a labeled data frame and a .sav output file
#'
#' @examples
#' \dontrun{
#' # Set relevant paths
#' form.path <- "../db/Kobo_survey_IDP_2010.xlsx"
#' db.path <- "../db/Survey_IDP_2010_data.xlsx"
#'
#' # Get and format data
#' db.hh<- readxl::read_excel(db.path, guess_max = 30000) %>%
#'   dplyr::mutate(
#'     hh.uuid = paste0("UUID_", `_uuid`, "_ID_", `_id`, "_INDEX_", `_index`)
#'   ) %>%
#'   dplyr::relocate(hh.uuid)
#'   
#' # Correct names not accepted by SPSS
#' db.hh.code <- db.hh %>%
#'   dplyr::rename_with(
#'     ~ gsub("(^_)(.*)", "X\\1\\2", .), dplyr::matches("^_")
#'   ) %>%
#'   dplyr::rename_with(~ gsub("/", ".", .), dplyr::matches("/"))
#'
#' # Get Labels db
#' db.hh.label <- R.HNAP::LabelIt(db.hh, form.path, input = "name", 
#' values = TRUE, uid = "hh.uuid")
#'  
#' SPSSLabelled(db.hh.code, db.hh.label, form.path, "IDP_2010_hh.data")
#' }
#'
#' @export
## -----------------------------------------------------------------------------

SPSSLabelled <- function(db.code, db.label, form.path, export.file = "spss.db") {
  
  form <- R.HNAP::GetForm(form.path)[["form"]]
  
  # Get Governoratre name/label for WoS and str. as in R.HNAP::GetForm() for labeling 
  admin1 <- unique(R.HNAP::admins$admin.comp[,c("admin1Name", "admin1Pcode")])
  gov.labs <- data.frame(
    type = rep("select_one", nrow(admin1)), 
    list_name = rep("governorate", nrow(admin1)),
    q.name = rep("admin1", nrow(admin1)),
    a.name = admin1$admin1Pcode,
    a.label = admin1$admin1Name
  )
  
  #District labs
  admin2 <- unique(R.HNAP::admins$admin.comp[,c("admin2Name", "admin2Pcode")])
  dis.labs <- data.frame(
    type = rep("select_one", nrow(admin2)), 
    list_name = rep("district", nrow(admin2)),
    q.name = rep("admin2", nrow(admin2)),
    a.name = admin2$admin2Pcode,
    a.label = admin2$admin2Name
  )
  # Subdistrict
  admin3 <- unique(R.HNAP::admins$admin.comp[,c("admin3Name", "admin3Pcode")])
  sub.labs <- data.frame(
    type = rep("select_one", nrow(admin3)), 
    list_name = rep("district", nrow(admin3)),
    q.name = rep("admin3", nrow(admin3)),
    a.name = admin3$admin3Pcode,
    a.label = admin3$admin3Name
  )
  # Location
  admin4 <- unique(R.HNAP::admins$admin.comp[,c("LocationName", "LocationPcode")])
  loc.labs <- data.frame(
    type = rep("select_one", nrow(admin4)), 
    list_name = rep("district", nrow(admin4)),
    q.name = rep("admin4", nrow(admin4)),
    a.name = admin4$LocationPcode,
    a.label = admin4$LocationName
  )
  
  # Join admin labels to form data
  form <- dplyr::bind_rows(
    dplyr::filter(form, !(q.name %in% c("admin1", "admin2", "admin3", "admin4"))),
    gov.labs, dis.labs, sub.labs, loc.labs
  )
  
  
  ################################## LABEL VALUES ################################
  
  # Convert all to character to avoid type conflict
  db.code <- dplyr::mutate_all(db.code, as.character)
  
  # Get list of value labels per question
  values <- purrr::map(
    unique(form$q.name), ~{
      
      values <- purrr::set_names(
        dplyr::pull(form[which(form$q.name == .x), "a.name"]),
        dplyr::pull(form[which(form$q.name == .x), "a.label"])
      )
    })
  named.values <- purrr::set_names(values, unique(form$q.name))
  
  # Remove extra variables that are in the form but not in the db (e.g individuals vars when in hh db)
  extra.no.db <- setdiff(names(named.values), colnames(db.code))
  for(i in 1:length(extra.no.db)) {
    named.values[[extra.no.db[i]]] <- NULL
  }
  
  # Remove no-labelled
  named.values <- Filter(Negate(anyNA), named.values)
  
  # Relabel
  labelled::val_labels(db.code) <- named.values
  

  # Label dummy vairables from multiple answer questions
  multiple.vars <- form %>%
    dplyr::filter(type == "select_multiple") %>%
    dplyr::pull(q.name)

  multiple.dummy <- colnames(db.code)[
    grep(paste0(multiple.vars, ".+", collapse = "|"), colnames(db.code))
  ]

  dummy.values <- c()
  dummy.values <- purrr::map(
    multiple.dummy, ~ {dummy.values[[.x]] <- c("Yes" = "1", "No" = "0")}
  )
  names(dummy.values) <- multiple.dummy

  # Relabel dummy
  labelled::val_labels(db.code) <- dummy.values
  

  ################################### LABEL NAMES ################################
  
  labelled::var_label(db.code) <- purrr::set_names(
    as.list(colnames(db.label)), colnames(db.code)
  )
  

  ################################# EXPORT #######################################

  if (!is.null(export.file)) {
    
    db.out <- db.code %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::matches("^admin\\d$"), list(Pcode = ~ as.vector(.)),
          .names = "{.col}{.fn}"
        ),
        dplyr::across(
          dplyr::matches("^admin\\d$"), ~ as.character(labelled::unlabelled(.))
        )
      ) %>%
      dplyr::rename_with(~ gsub("^(.+)", "\\1Name", .), dplyr::matches("^admin\\d$")) %>%
      dplyr::relocate(dplyr::matches("^admin\\dP"), .before = dplyr::matches("^admin\\dN"))
      
    # Correct variable type
    db.out <- R.HNAP::SPSSType(db.out, form)
    
    # SPSS valid colnames
    colnames(out)[which(nchar(colnames(out)) > 50)] <- gsub(
      "^(.{1,10}).+$", "\\1", colnames(out)[which(nchar(colnames(out)) > 50)]
    )
    spss.names <- gsub("/", ".", colnames(out)) # "/" in multiple Q. sep. the Ques from the Answ.  Repalce is by "." to avoid conflict with "_" in names
    spss.names <- gsub("-|\\s|%|:|<|>|\\(|\\)", "_", spss.names) 
    spss.names <- gsub("\\.$", "", spss.names)
    colnames(out) <- gsub("^(_|\\.)(.*)", "X\\1\\2", spss.names)
    
    
    file.dir <- paste0("output_ddbb/", R.HNAP::DateStamp(export.file), ".sav")
    haven::write_sav(db.out, file.dir)
    cat("\nSPSS data base exported to:", file.dir, "\n")
  }
  
  return(db.out)
}
