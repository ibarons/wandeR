################################################################################
## @Title: report_export_outputs_xlsx.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 06.05.2020
## @Last updated:
## @Status: maturing
## ------------------------------------------------------------------------------
## Notes:
## 
## Rule to detect % and thousand does nto work.... old one did work, but not in 
## all cases... Consider restablishing the old rule, with the new 
## `if (format.num == TRUE)` condition.
## 
## Set argument `sheet` to specify format `c("single", "multiple)` sheets
##
## Explore more efficient ways than double for-loops (e.g apply family)
## ----------------------------------------------------------------------------
#' @name ExportXLSX
#' @title Export R.HNAP tables to .xlsx
#'
#' @description Export output tables given by other functions.
#'
#' @usage
#' ExportXLSX(outputs, format.num = TRUE)
#'
#' @param outputs Two level list with first level for sheets and second for
#' tables within the sheet
#' @param format.num logical if should number be formatted
#' @param survey logical if tables are produced by SurveyTbl
#'
#' @return Workbook object as defined by openxlsx::createWorkbook() that can be
#' exported to an .xlsx with openxlsx::saveWorkbook()
#'
#' @examples
#' # List of outputs
#' outputs <- list(
#'   sheet1 = list(
#'     table1 = data.frame(a = letters[1:5], x = 1:5, stringsAsFactors = FALSE),
#'     table2 = data.frame(b = LETTERS[6:10], y = 6:10, stringsAsFactors = FALSE)
#'   ),
#'   sheet2 = list(
#'     table1 = data.frame(a = letters[6:10], x = 6:10, stringsAsFactors = FALSE),
#'     table2 = data.frame(b = LETTERS[1:5], y = 1:5, stringsAsFactors = FALSE)
#'   )
#' )
#' # Apply ExportXLSX function
#' wb <- ExportXLSX(outputs)
#' \dontrun{
#' # Save outputs in .xlsx file
#' openxlsx::saveWorkbook(wb, "example.ExportXLSX.xlsx", overwrite = TRUE)
#' }
#'
#' @export
## ------------------------------------------------------------------------------
ExportXLSX <- function(outputs, format.num = TRUE) {
  ############################## CREATE STYLES #################################
 
  if (any(lengths(outputs) == 0)) {
    warning("Sheets defined with NO table inside. Have been removed and not exported")
    outputs <- outputs[lengths(outputs) != 0]
  }

  ## fonts require a workbook
  hs <- openxlsx::createStyle(
    textDecoration = "BOLD", fontColour = "#FFFFFF",
    fontSize = 12, fontName = "Arial Narrow",
    fgFill = "#4F80BD"
  )
  thousand_style <- openxlsx::createStyle(numFmt = "#,#0")
  percent_style <- openxlsx::createStyle(numFmt = "0.00%")
  decimal_style <- openxlsx::createStyle(numFmt = "#,#0.00")

  ################################# CREATE WORK BOOK ##########################
  # Open WorkBook
  wb <- openxlsx::createWorkbook()

  # Open loop for sheets
  message("\nThe following tables will be exported to .xlsx file:")
  for (k in names(outputs)) {
    message("\nOutput:", k)
    openxlsx::addWorksheet(wb, k, gridLines = FALSE, tabColour = "#0351C1")
    # Paste first table
    st_col <- 1
    st_row <- 1
    n_rows <- nrow(outputs[[k]][[1]])
    r_rows <- (st_row + 1):(st_row + n_rows)
    # Warn+Next on empty tables
    if (nrow(outputs[[k]][[1]]) == 0) {
      message(names(outputs[[k]][1])) # output table name for debugging
      openxlsx::writeData(wb, # Try writeDataTable() to output as table format
        sheet = k,
        data.frame(
          tbl.name = names(outputs[[k]][1]),
          result = "EMPTY TABLE",
          reason = "TABLE HAS NO DATA"
        ),
        startCol = st_col, startRow = st_row,
        borderColour = getOption("openxlsx.borderColour", "#FF0000"),
        borderStyle = getOption("openxlsx.borderStyle", "thick")
      )
      warning(
        "In ", k, " the following tables are empty: ", 
        names(outputs[[k]][1])
      )
      next
    }
    # Write the first table to get nrows and ncols for the rest
    message(names(outputs[[k]][1])) # output table name for debugging
    openxlsx::writeData(wb,
      sheet = k,
      outputs[[k]][[1]],
      startCol = 1, startRow = 1,
      colNames = TRUE, rowNames = FALSE,
      headerStyle = hs,
      borders = "all",
      borderColour = getOption("openxlsx.borderColour", "black"),
      borderStyle = getOption("openxlsx.borderStyle", "thin")
    )
    
    if (format.num == TRUE) {

      # add styles for percentages
      openxlsx::addStyle(wb,
        sheet = k,
        percent_style,
        rows = r_rows,
        cols = which(apply(as.data.frame(outputs[[k]][[1]] <= 1),
          2, all,
          na.rm = TRUE
        )),
        # &
        #   apply(as.data.frame(outputs[[k]][[1]] >= 0),
        #     2, all,
        #     na.rm = TRUE
        #   )),
        gridExpand = TRUE, stack = TRUE
      )
      # add styles for thousands
      openxlsx::addStyle(wb,
        sheet = k,
        thousand_style,
        rows = r_rows,
        cols = which(apply(as.data.frame(outputs[[k]][[1]] > 1),
          2, any,
          na.rm = TRUE
        )),
        # &
        #   apply(as.data.frame(outputs[[k]][[1]] > 0),
        #     2, all,
        #     na.rm = TRUE
        #   )),
        gridExpand = TRUE, stack = TRUE
      )
      # add style for ratio
      if (any(grepl("ratio", colnames(outputs[[k]][[1]])))) {
        openxlsx::addStyle(wb,
          sheet = k,
          percent_style,
          rows = r_rows,
          cols = grep("perc\\.", colnames(outputs[[k]][[1]])), # which(apply(as.data.frame((outputs[[k]][[1]] >= 0 & outputs[[k]][[1]] <= 1)), 2, all, na.rm = TRUE))
          gridExpand = TRUE, stack = TRUE
        )
        
        # add styles for thousands
        openxlsx::addStyle(wb,
          sheet = k,
          thousand_style,
          rows = r_rows,
          cols = grep("^(sum\\.|coun\\.|avg\\.|med\\.)", colnames(outputs[[k]][[1]])), #which(apply(as.data.frame(outputs[[k]][[1]] > 1), 2, any, na.rm = TRUE))
          gridExpand = TRUE, stack = TRUE
        )
        # add style for ratio
        if (any(grepl("ratio", colnames(outputs[[k]][[1]])))) {
          openxlsx::addStyle(wb,
            sheet = k,
            decimal_style,
            rows = r_rows,
            cols = grep("ratio", colnames(outputs[[k]][[1]])),
            gridExpand = TRUE, stack = TRUE
          )
        }
      }
    }

    # Loop for the remaining tables within sheet (excludign the first one)
    for (l in names(outputs[[k]][-1])) {
      # get tables coordinates in sheet for location taking first table as ref.
      st_col <- 1
      st_row <- st_row + n_rows + 3
      n_rows <- nrow(outputs[[k]][[l]])
      r_rows <- (st_row + 1):(st_row + n_rows)
      # Warn+Next on empty tables
      if (nrow(outputs[[k]][[l]]) == 0) {
        message(names(outputs[[k]][l])) # output table name for debugging
        openxlsx::writeData(wb,
          sheet = k,
          data.frame(
            tbl.name = names(outputs[[k]][l]),
            result = "EMPTY TABLE",
            reason = "TABLE HAS NO DATA"
          ),
          startCol = st_col, startRow = st_row,
          borderColour = getOption("openxlsx.borderColour", "#FF0000"),
          borderStyle = getOption("openxlsx.borderStyle", "thick")
        )
        warning(
          "In ", k, " the following tables are empty: ",
          names(outputs[[k]][l])
        )
        next
      }
      # Write the remaining tables
      message(names(outputs[[k]][l])) # output table name for debugging
      openxlsx::writeData(wb,
        sheet = k,
        outputs[[k]][[l]],
        startCol = st_col, startRow = st_row,
        colNames = TRUE, rowNames = FALSE,
        headerStyle = hs,
        borders = "all",
        borderColour = getOption("openxlsx.borderColour", "black"),
        borderStyle = getOption("openxlsx.borderStyle", "thin")
      )
      
      if (format.num == TRUE) {

        # add styles for percentages
        openxlsx::addStyle(wb,
          sheet = k,
          percent_style,
          rows = r_rows,
          cols = which(apply(as.data.frame(outputs[[k]][[l]] <= 1),
            2, all,
            na.rm = TRUE
          ) &
            apply(as.data.frame(outputs[[k]][[l]] >= 0),
              2, all,
              na.rm = TRUE
            )),
          gridExpand = TRUE, stack = TRUE
        )
        # add styles for thousands
        openxlsx::addStyle(wb,
          sheet = k,
          thousand_style,
          rows = r_rows,
          cols = which(apply(as.data.frame(outputs[[k]][[l]] > 1),
            2, any,
            na.rm = TRUE
          ) &
            apply(as.data.frame(outputs[[k]][[l]] > 0),
              2, any,
              na.rm = TRUE
            )),
          gridExpand = TRUE, stack = TRUE
        )
        # style for ratio column
        if (any(grepl("ratio", colnames(outputs[[k]][[l]])))) {
          openxlsx::addStyle(wb,
            sheet = k,
            percent_style,
            rows = r_rows,
            cols = grep("perc\\.", colnames(outputs[[k]][[l]])), # which(apply(as.data.frame(outputs[[k]][[l]] >= 0 & outputs[[k]][[l]] <= 1), 2, all, na.rm = TRUE))
            gridExpand = TRUE, stack = TRUE
          )
          
          # add styles for thousands
          openxlsx::addStyle(wb,
            sheet = k,
            thousand_style,
            rows = r_rows,
            cols = grep("^(sum\\.|coun\\.|avg\\.|med\\.)", colnames(outputs[[k]][[l]])), #which(apply(as.data.frame(outputs[[k]][[l]] > 1), 2, any, na.rm = TRUE))
            gridExpand = TRUE, stack = TRUE
          )
          # style for ratio column
          if (any(grepl("ratio", colnames(outputs[[k]][[l]])))) {
            openxlsx::addStyle(wb,
              sheet = k,
              decimal_style,
              rows = r_rows,
              cols = grep("ratio", colnames(outputs[[k]][[l]])),
              gridExpand = TRUE, stack = TRUE
            )
          }
        }
      }
    }
  }
  invisible(wb)
}

