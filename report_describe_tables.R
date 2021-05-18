################################################################################
## @Title: report_describe_tables.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 15.03.2021
## @Last updated: 
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes:
##
## ----------------------------------------------------------------------------
#' @name DescribeTbls
#' @title Describe the tables inside a file
#'
#' @description Creates a data frame that describe the table contents of a file.
#' It position this data frame as first table to be exported in an xlsx. Further
#' it groups the tables by variable name so table from the same variable name 
#' appears on the same sheet. It is better sued with `R.HNAP::SurveyMass` and 
#' with `R.HNAP::ExportXLS`
#' 
#'
#' @usage DescribeTbls(list.tbls)
#'
#' @param list.tbls data frame as survey.design class from `srvyr::as_survey_design()`
#' with variables to be analysed and admin level to be disaggregated by.
#
#'
#' @details
#' 
#' The object nae of each data frame in the list need to follow this pattern:
#' "TBL.Unit.DEP.var1.INDEP.var2" with "Unit", "var1" and "var2" being free 
#' strings (whatever strign you want, suusally correcpond to variable names).
#' 
#' The function will group table by var1 so all table were var1 is the same appear
#' in the same sheet of the excel file.
#' 
#' The description data frame containt he following column:
#' * sheet = name of var1 (dependent variable)
#' * tbl.name = name given to the table, if empty then NULL
#' * var_rows = name of var1 (dependent variable)
#' * var_cols = name of var2 (independent variables)
#' * description = "" (to be filled at will)
#' 
#' The description data frame will be shown as the first data frame in the list (or
#' sheet in the exported .xlsx)
#'
#' @return 
#' A two level list of data frames with the generated description data frame in
#' list.tbls[["descripion"]][["description"]] or list.tbls[[1]][[1]]
#'
#' @examples
#' TBL.Unit.DEP.var1.INDEP.var2 <- data.frame(var1 = 0:10, var2 = 10:20, tbl.name = "Table1")
#' TBL.Unit.DEP.var3.INDEP.var4 <- data.frame(var3 = 20:30, var4 = 40:50, tbl.name = "Table2")
#' TBL.Unit.DEP.var4.INDEP.var3 <- data.frame(var3 = 20:30, var4 = 40:50, tbl.name = "Table3")
#' TBL.Unit.DEP.var3.INDEP.var2 <- data.frame(var5 = 50:60, var6 = 60:70, tbl.name = "Table4")
#' 
#' list.tbls <- list(
#'   list1 = list(
#'     TBL.Unit.DEP.var1.INDEP.var2 = TBL.Unit.DEP.var1.INDEP.var2,
#'     TBL.Unit.DEP.var3.INDEP.var4 = TBL.Unit.DEP.var3.INDEP.var4
#'   ),
#'   list2 = list(
#'     TBL.Unit.DEP.var1.INDEP.var2 = TBL.Unit.DEP.var1.INDEP.var2,
#'     TBL.Unit.DEP.var4.INDEP.var3 = TBL.Unit.DEP.var4.INDEP.var3,
#'     TBL.Unit.DEP.var3.INDEP.var2 = TBL.Unit.DEP.var3.INDEP.var2
#'   )
#' )
#' 
#' DescribeTbls(list.tbls)
#'
#' @export
## -----------------------------------------------------------------------------

DescribeTbls <- function(list.tbls) {
  
  if (purrr::vec_depth(list.tbls) == 4) {
    list.tbls <- unlist(list.tbls, recursive = FALSE)
  }
  
  
  # Simplify the list and keep tbl names only
  names(list.tbls) <- gsub(paste0("^.+\\.(TBL\\.", ".+", "\\..*)$"), "\\1", names(list.tbls))
  
  # Warn null tables
  if (any(lengths(list.tbls) == 0)) {
    warning(
      "Some cross-tabs defined resulted empty. Check carefully the following: ",
      paste(
        "\n", which(lengths(list.tbls) == 0), "-", 
        names(which(lengths(list.tbls) == 0)), "\n"
      )
    )
  }
  
  # Group tables by dependent variable to export one xlsx sheet per dependent
  s.names <- unique(gsub(
    paste0("^(TBL\\.", ".+", "\\.DEP\\.)", "(.+)\\.INDEP\\.(.+$)"),
    "\\2",
    names(list.tbls)#unlist(sapply(list.tbls, names))
  ))
  
  out.tbls <- purrr::map_depth(
    s.names, .depth = 1, 
    ~ {list.tbls[[as.character(.x)]] <- list.tbls[grep(paste0(
      "^(TBL\\.", ".+", "\\.DEP\\.)", .x, "\\.INDEP\\."), names(list.tbls) #unlist(sapply(list.tbls, names))
    )] 
    })
  names(out.tbls) <- s.names
  
  # Create data frame to describe each tables
  s.names.list <- lapply(
    seq_along(out.tbls), function(i) 
      rep(names(out.tbls)[[i]], length(out.tbls[[i]]))
  )
  
  description <- dplyr::bind_rows(
    purrr::map2(out.tbls, s.names.list, ~ {
      
      i = seq_along(.x)
      nam <- as.character(lapply(.x, function(x) unique(x$tbl.name[1]))[i])
      
      data.frame(
        sheet = .y,
        tbl.name = nam[i],
        vars_rows = gsub(
          paste0("^(TBL\\.", ".+", "\\.DEP\\.)", "(.+)\\.INDEP\\.(.+$)"),
          "\\2", names(.x)
        ),
        vars_cols = gsub(
          paste0("^(TBL\\.", ".+", "\\.DEP\\.)", "(.+)\\.INDEP\\.(.+$)"),
          "\\3", names(.x)
        ),
        description = rep("", length(names(.x)))
        #row.names = NULL # this would allow to output in the description the table name in R ("TBL....")
      )
    })
  )
  out.tbls[["description"]][["description"]] <- description
  
  # Move description to first page
  out.tbls <- rlist::list.subset(
    out.tbls, c("description", names(out.tbls)[-length(names(out.tbls))])
  )
  
  return(out.tbls)

}