################################################################################
## @Title: report_survey_extract_tbls.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 18.03.2021
## @Last updated: 03.05.2021
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes:
##
## The open filter argument to manually define any filter you want seems the 
## most powerful and dynamic way to go. But this might be difficult if the f()
## will be handed over tot PSU. A previous version used:
##
##  args: filter.from = "group", filter.what = c("governorate", "subdistrict")
##  code: dplyr::filter(.x, !!rlang::sym(filter.from) %in% filter.what)
##
## Check with Nour if very long unique table or multipel tables
##
## When importing from files, `tbl.name` is a problem as does not have the given
## name to multicat variables, fails to repdocude accurately the description file
## and make some part of the code cumbersome:
##    Maybe if we export the R list name for the table in the description we can
##    more easily and accurately reproduce the description file.
##    Maybe instead of reproducing the description file we cna just filter it from
##    the original
##
## Need to finalize to take SurveyMass() object through list.df arg. Is almost 
## there, but havent been tested, polished and well integrated
##
## ----------------------------------------------------------------------------
#' @name SurveyExtractTbls
#' @title Extract tables for Governorate Profiles
#'
#' @description Extract defined tables from given .xlsx files or R object as 
#' produced by `R.HNAP::SurveyMass()` or identical structure
#' 
#'
#' @usage SurveyExtractTbls(target.dir, target.tbls = NULL, list.tbls = NULL,
#' filter = TRUE, pivot_long = FALSE, export.tbls = TRUE)
#'
#' @param target.dir path to directore where to fine the .xlsx fiels containing 
#' the tables
#' @param target.tbls names character vector defining the table name and
#' the name of the sheet were to find it (e.g. `c(sheet.name = tbl.name)`). Leave
#' as `NULL` if all tables in the file should be target
#' @param list.tbls a list of tables R object as produced by R.HNAP::SurveyMass()
#' @param filter string with the filter expression. Set `filter = TRUE` for no 
#' filtering Within the string use single quotes ('xx') and do not use comma 
#' (","), use "&" or "|" instead for chaining filter expressions
#' @param pivot_long Logical should all the tables be pivoted_long, leaving only
#'  stats as columns
#' @param export.tbls Logical should tables be exported to .xslx
#
#'
#' @details
#' 
#' For more details on the structure of the .xslx file or R object to input and
#' its contents see `R.HNAP::SurveyMass()`.
#'
#' @return 
#' A list of the tables reformated for governorate profiel. A .xlsx file if 
#' exported was TRUE
#'
#' @examples
#' 
#' \dontrun{
#' target.dir <- "C:/Users/jibarguen/folder_with_files"
#' 
#' target.tbls <- c(
#'   # Demographic
#'   "age_pyramid" = "Individuals - age_pyramid by PMSex",
#'   "work_18.64" = "Individuals - work_18.64 by PMSex",
#'   "covid_income_suffice" = "Households - covid_income_suffice",
#'   "ShelterType" = "Households - ShelterType"
#' )
#'
#' gov.profile.tbls <- R.HNAP::SurveyExtractTbls(target.dir, target.tbls)
#' 
#' # Extract all tables in a directory for some subdistricts only
#' R.HNAP::SurveyExtractTbls(
#'   target.dir, target.tbls = NULL, 
#'   filter = "adminPcode %in% c('SY080005', 'SY090101') | group == 'Total'",
#'   export.tbls = TRUE
#' )
#' 
#' # If only inetrested in tables in a single file, just put the single file in the directory.
#' }
#' 
#'
#' @export
## -----------------------------------------------------------------------------

SurveyExtractTbls <- function(target.dir, target.tbls = NULL, list.tbls = NULL,
                              filter = TRUE, pivot_long = FALSE,
                              export.tbls = TRUE) {

  
  ############################## GET TABLES FORM XLSX ##########################
  
  target.tbl.check <- target.tbls # Save input value as target.tbls is replaced later (mayeb just adjust the bloody name?)
  
  if (is.null(list.tbls)) {
    
    target.files <- file.path(target.dir, list.files(target.dir, pattern = "*.xlsx"))
    
    cat("Tables will be searh in the following files:\n")
    print(target.files)
    
    list.tbls <- lapply(target.files, function(x) {
      
      list.tbls <- suppressMessages(readxl::excel_sheets(x) %>%
      rlang::set_names(.) %>%
      purrr::map(readxl::read_excel, path = x, guess_max = 150000))
      
    }) %>%
      unlist(use.names = TRUE, recursive = FALSE)
    


################################ SELECT REQUESTED TABLES ######################

    if (is.null(target.tbl.check)) { # If target.tbl = NULL, set all tables as target

      tbl.sheets <- as.character(unlist(sapply(
        list.tbls[grep("description", names(list.tbls))], "[[", 1
      )))
      target.tbls <- as.character(unlist(sapply(
        list.tbls[grep("description", names(list.tbls))], "[[", 2
      )))
      names(target.tbls) <- tbl.sheets
      
      save.descript <- dplyr::bind_rows(list.tbls[grep("description", names(list.tbls))])
    }
      
    list.tbls <- purrr::imap(
      target.tbls, 
      ~ {
        
        # Select sheets
        df <- dplyr::bind_rows(purrr::map( # if two sheet w/ same name. Bidn df as.character (avoid type conflict) and filter or with tbl.name
          list.tbls[grep(paste0("^", .y, "$"), names(list.tbls))],
          dplyr::mutate_all, as.character
        ))
        
        if (length(which(df$tbl.name == .x)) == 0) {
          stop("\ntable name '", .x, "' cannot be found in the refered sheet '", .y, "'\n")
        }
        
        # Get table index (from row i.min to row i.max) # And why not simply split(., df$tbl.name) ?? Cause tbl.name for multi-cat vars, as of today take int he name the category of the var... thus creatign as many tables ad categories in the multiple repsonse Q.
        i.min <- min(which(df$tbl.name == .x)) - 1
        
        na.seq <- sort(c(i.min, which(is.na(df$tbl.name)))) # get i.max as next is.na after i.min
        
        if (which(na.seq == i.min) + 1 < length(na.seq)) {
          i.max <- na.seq[which(na.seq == i.min) + 1] 
        } else i.max <- length(df$tbl.name)
        
        if (i.min != 0 && df$tbl.name[i.min] == "tbl.name" ) { 
          
          df[i.min:i.max, ] %>%
            rlang::set_names(., .[1, ]) %>%
            Filter(function(x) !all(is.na(x)), .) %>% # faster apply|colSums, avoids dplyr::where()
            .[-1, ] 
          
        } else {
          
          df[i.min:i.max, ] %>%
            Filter(function(x) !all(is.na(x)), .) %>%
            .[-1, ] %>%
            dplyr::select(-dplyr::matches("^\\.\\.\\.\\d{1,3}"))
        }
      }
    )
      
    # Filter relevant
    filter.exp <- parse(text = filter)
    out.tbls <- purrr::map(list.tbls, ~ dplyr::filter(.x, eval(filter.exp)))
    
    ########################### RE-FORMAT TABLES ###############################    

    if (pivot_long == TRUE) {
  
      # Split total tbls from indep tables
      i.tbls <- purrr::map(out.tbls, ~ any(grepl("\\sby\\s", .x$tbl.name))) %>%
        unlist 
      list.tbls.total <- out.tbls[which(!i.tbls)]
      list.tbls.indep <- out.tbls[which(i.tbls)]
      
      # pivot longer indep tabls
      list.tbls.indep <- purrr::map(
        list.tbls.indep, 
        ~ tidyr::pivot_longer(.x,
          dplyr::matches("^(avg.we|med.we|sum.we|avg.we_se|coun.we|perc.we|coun.un)"),
          names_to = c(".value", gsub(".+\\sby\\s(.+)\\s*.*$", "\\1", .x$tbl.name[1])),
          names_pattern = "^(avg.we|med.we|sum.we|avg.we_se|coun.we|perc.we|coun.un)_(.*)$"
        )
      )
  
      # Merge total and indep
      out.tbls <- c(list.tbls.total, list.tbls.indep)
    }

    
    ################################# DESCRIBE SHEET ###########################
    
    if (!is.null(target.tbl.check)) { 
    
      # Convert list names into SurveyMass names
      tbl.names <- sapply(out.tbls, "[[", c(1, 1))
      tbl.names[!grepl("by", tbl.names)] <- gsub( 
        "^(.+)\\s-\\s(.+)$", "TBL.\\1.DEP.\\2.INDEP.total",
        tbl.names[!grepl("by", tbl.names)]
      )
      tbl.names[grepl("by", tbl.names)] <- gsub(
        "^(.+)\\s-\\s(.+)\\sby\\s(.+)$", "TBL.\\1.DEP.\\2.INDEP.\\3",
        tbl.names[grepl("by", tbl.names)]
      )
      # Get description sheet, but don't change original list names, these are the sheet names
      out.tbls.temp <- out.tbls 
      names(out.tbls.temp) <- tbl.names 

      out.tbls <- c(R.HNAP::DescribeTbls(out.tbls.temp)$description, out.tbls)
      
    } else { # If all tables are targeted i.e target.tbl = NULL, then no need to reproduce "description" sheet
      
      out.tbls <- c(list(description = save.descript), out.tbls)
      
    }
    
    out.tbls <- lapply(out.tbls, utils::type.convert, as.is = TRUE)
    
  }

  
 ################################## EXPORT TABLES ############################## 
  
  # Recompose file
  s.names <- unique(names(out.tbls))
  export.tbl <- c()
  export.tbl <- purrr::map_depth(
    s.names, .depth = 1, 
    ~ {export.tbl <- out.tbls[
      grep(paste0("^", .x, "$"), names(out.tbls))
    ]})
  names(export.tbl) <- s.names
  
  
  if (export.tbls == TRUE) {
    
    # Export
    wb <- R.HNAP::ExportXLSX(export.tbl)
    openxlsx::saveWorkbook(
      wb,
      paste0("output_tables/", R.HNAP::DateStamp("extract_tables.xlsx")),
      overwrite = TRUE
    )
    cat(
      "\nSurvey tables exported to:\n",
      paste0("~/output_tables/", R.HNAP::DateStamp("extract_tables.xlsx")), "\n"
    )
  }
  
  invisible(export.tbl)
}


### One single tables - for Nour?
#
# y <- dplyr::bind_rows(unlist(x[-1], recursive = FALSE)) %>%
#   tidyr::pivot_longer(
#     -c(
#       tbl.name,	group, adminName, coun.we, perc.we, coun.un, 
#       adminPcode, admin1Pcode, admin1Name, AoC, avg.we, med.we
#     ),
#     names_to = "question", values_to = "category"
# )
#
# out.tbls <- list(single_tbl = list(y = y))




# else if (!is.null(list.tbls)) {
#  
# if (!(purrr::vec_depth(list.tbls) %in% c(3, 4))) { # Haven't fully test it, but i really think so
#   warning("Nested list has `", purrr::vec_depth(list.tbls), "` levels. It only works with 3:4 levels. Please check your result")  
# }
# 
#   
#   list.tbls <- hh.sociecon.tbls[-grep("^description$", names(hh.sociecon.tbls))]
#   
#   
#   list.tbls <- purrr::map_depth(
#     list.tbls, 2, ~ dplyr::filter(.x, group %in% !!!groups.sym)
#   )
#      
#   # Filter out requiered tables pedign to devleop
#   
#   list.tbls.total <- list.tbls[grepl("\\.INDEP\\.total$", names(list.tbls))]
#   list.tbls.indep <- list.tbls[!grepl("\\.INDEP\\.total$", names(list.tbls))]
#   
#   list.tbls.indep <- purrr::imap(list.tbls.indep, ~  tidyr::pivot_longer(.x,
#       dplyr::matches("^(avg.we|med.we|sum.we|avg.we_se|coun.we|perc.we|coun.un)"),
#       names_to = c(".value", gsub("^.+\\.INDEP\\.(.+)$", "\\1", .y)),
#       names_pattern = "^(avg.we|med.we|sum.we|avg.we_se|coun.we|perc.we|coun.un)_(.*)$"
#     )
#   )
#   
#   out <- c(list.tbls.total, list.tbls.indep) 
#   
# }


