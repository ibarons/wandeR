################################################################################
## @Title: format_form_labels.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 06.07.2020
## @Last updated: 14.12.2020
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes
## 
## Consider if the function should correct names to a more standard format (e.g.
## names accepted in SPSS...)
## 
## Consider removing warnign and juts cat() into the console. At the end the 
## truncated warning
## 
## Warning on multiple answer questions seems not working LL:260:270
## 
## 
## values_fn = length add this as argument so easily can be fixz which variables
## are duplicating, ratehrt than enting into the funciton.
## 
## Improvement to better distingues between group names and question names
##
## The trimws() in form <- . seems not to work well
##
## Think about removing "others" and "specify" cause they usually share label 
## with multiple questions and generate duplication when pivoting wider. Maybe 
## can find a way to create them unique, similar to the data base... rather than 
## remove them
##
## ----------------------------------------------------------------------------
#' @name LabelIt
#' @title Label questions and answer of a data base
#'
#' @description Given an ODK form (Kobo/XLM), it replaces form names by form 
#' labels, and vice-versa, for questions and/or answer.
#'
#' @usage
#' LabelIt(db, form.path, input = "label", values = TRUE, uid = NULL, values_fn = NULL)
#'
#' @param db a data frame containing the data base to be labeled/coded. Note if
#' the input is a labeled data base, it should have been imported with
#' `check.names = FALSE`
#' @param form.path the path to a XLSX file containing the Kobo form. At least,
#' the file need to:
#' - A XLSX file
#' - Contains a "survey" sheet with fields: type, name and label.*[Ee]ng.*$
#' - Contains a "choices" sheet with fields: list_name, name, label.*[Ee]ng.*$.
#' Advice to remove admin from choices, or rather make sure all admin are in the
#' choices sheet
#' @param input  a string of "label" or "name" indicating what the db input
#' contains. It will output the contrary (i.e. if input labeled db, will output
#' coded data base)
#' @param values Logical values should be converted too, or only column names
#' @param uid string with an id to uniquely identify cases. If NULL it take the 
#' ODK (XLM) meta-variables (id and uuid) to create a unique id.
#' @param values_fn passed to tidyr::pivot_wider to check the lenght of variables
#' when "output will contain list-cols" warnings.
#'
#' @details
#' The column that contain the compilation of answers of a multiple question in 
#' a single cell is ignored and not treated by the function.
#'
#' When questions name/labels are duplicated, the function de-duplicate them
#' by pasting the question name + question label separated by "/"
#' 
#' @return The same input data frame, but with column names and/or values changed
#'
#' @examples
#' \dontrun{
#' # Change only column names to XLM names with owns uuid
#' ind.w.code <- LabelIt(ind.w.lab, form.path, input = "label", 
#' values = FALSE, uid = "ind.uuid")
#' 
#' # Change XLM names to labels (column names and values)
#' ind.w.code <- LabelIt(ind.w.lab, form.path, input = "name")
#' }
#'
#' @export
## -----------------------------------------------------------------------------
LabelIt <- function(db, form.path, input = "label", values = TRUE, uid = NULL,
                    values_fn = NULL) {
  
  ####################### PREELIMINARIES AND QUOSURES ##########################
  
  if (input == "name") {
    output <- "label"
  } else if (input == "label") {
    output <- "name"
  }
  
  # get quosures
  if (!is.null(uid)) {uid.sym <- rlang::sym(uid)}
  q.sym.in.char <- paste0("q.", input)
  q.sym.in <- rlang::sym(q.sym.in.char)
  a.sym.in <- rlang::sym(paste0("a.", input))
  q.sym.out <- rlang::sym(paste0("q.", output))
  a.sym.out <- rlang::sym(paste0("a.", output))
  
  # Get form
  form.list <- R.HNAP::GetForm(form.path)
  form <- form.list$form
  form.quest <- form.list$form.quest
  form.answ<- form.list$form.answ
  
  ########################### CHANGE ONLY COLNAMES #############################
  
  colnames(db) <- trimws(colnames(db))
  miss.in.form <- !(colnames(db) %in% dplyr::pull(form.quest[, q.sym.in.char]))
  miss.in.db <- !(dplyr::pull(form[, q.sym.in.char]) %in% colnames(db))

  # Warn on irregular question names
  if (any(grepl("^$|^\\s*$", colnames(db))) | any(is.na(colnames(db)))) {
    cat(
      "\nThe following column names has an empty name:\n",
      paste(
        utils::capture.output(grep("^$|^\\s*$", colnames(db), value = TRUE)),
        collapse = "\n"
      )
    )
  }
  if (any(duplicated(colnames(db)))) {
    cat(
      "\nThe following column names are duplicated:\n",
      paste(
        utils::capture.output(as.character(colnames(db)[which(duplicated(colnames(db)))])),
        collapse = "\n"
      )
    )
  }
  if (any(miss.in.form)) {
    cat(
      "\nThe following data base names are not found in the form. They will be ignored:\n",
      paste(
        utils::capture.output(colnames(db)[which(miss.in.form)]), 
        collapse = "\n"
      )
    )
  }
  if (any(miss.in.db)) {
    cat(
      "\nThe following form names are not found in the data base. They will be ignored:\n",
      paste(
        utils::capture.output(form[which(miss.in.db), q.sym.in.char]), 
        collapse = "\n"
      )
    )
    form <- form[which(miss.in.db == FALSE), ] # filter out of form, names not exist in db
  }
    
  ############################# CHANGE ANSWERS #################################

  # db with label and names to choose
  db.form <- db %>%
    {
      if(is.null(uid)) {
        dplyr::mutate(.,
          `_uuid` = dplyr::if_else(
            `_uuid` == "",
            as.character(1:dplyr::n()),
            `_uuid`
          ),
          `_id` = dplyr::if_else(
            is.na(`_id`),
            1:dplyr::n(),
            as.integer(`_id`) # changed. "as.integer() not test in CORP
          )
        ) %>%
          tidyr::unite("unique.id", c("_uuid", "_id"), remove = FALSE)
      } else if (!is.null(uid)) {
        dplyr::rename(., "unique.id" = !!uid.sym)
      }
    } %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(
      -dplyr::matches("unique.id"),
      names_to = as.character(q.sym.in),
      values_to = as.character(a.sym.in)
    ) %>%
    dplyr::left_join(
      ., form,
      by = c(paste0("q.", input), paste0("a.", input))
    ) %>%
    dplyr::select(
      dplyr::matches("unique.id"),
      q.name, q.label, a.name, a.label,
      dplyr::everything()
    ) %>%
    dplyr::left_join(
      ., form.quest[, c("q.name", "q.label")],
      by = paste0("q.", input)
    ) %>%
    dplyr::rename_with(~ paste0("q.", output), dplyr::matches("\\.y$")) %>%
    dplyr::select(-dplyr::matches("\\.x$")) %>%
    {
      if (input == "name") {
        dplyr::mutate(.,
          a.label = dplyr::if_else(is.na(a.label), a.name, a.label),
          q.label = dplyr::if_else(is.na(q.label), q.name, q.label)
        )
      } else if (input == "label") {
        dplyr::mutate(.,
          a.name = dplyr::if_else(is.na(a.name), a.label, a.name),
          q.name = dplyr::if_else(is.na(q.name), q.label, q.name)
        )
      }
    } %>%
    dplyr::select(
      unique.id, dplyr::matches("^q"), dplyr::matches("^a"),
      dplyr::everything()
    )

  ####################### PIVOT OUTPUT DATA BASE ###############################

  # Pivot db wider
  db.label <- db.form %>%
    dplyr::select(
      dplyr::matches("unique.id"),
      dplyr::matches(paste0("\\.", output)),
      dplyr::matches(paste0("\\.", input))
    ) %>%
    {
      if (values == TRUE) {
        tidyr::pivot_wider(.,
          dplyr::matches("unique.id"),
          names_from = dplyr::all_of(q.sym.out),
          values_from = dplyr::all_of(a.sym.out),
          values_fn = values_fn
        )
      } else{
        tidyr::pivot_wider(.,
          dplyr::matches("unique.id"),
          names_from = dplyr::all_of(q.sym.out),
          values_from = dplyr::all_of(a.sym.in),
          values_fn = values_fn
        )
      }
    }
  
  # if (any(sapply(db.label, function(x) any(grepl("^c\\(.+,\\s.*\\)$", x))))) {
  #   
  #   warning("Columns not uniquely identify (list-col) have been automatically split. Please check")
  #   
  #   i <- names(which(sapply(db.label, function(x) any(grepl("^c\\(.+,\\s.*\\)$", x)))))
  #   
  #   # clean the list-col of c() characters
  #   db.label <- db.label[, grep("pecify", colnames(db.label))] %>%
  #     dplyr::mutate(
  #       dplyr::across(dplyr::all_of(i), ~ gsub("^c\\((.+)\\)$", "\\1", .))
  #     ) 
  #   # Separate for each existing value int he list-col
  #   db.label <- purrr::map_df(i, ~ {
  #     n <- stringr::str_count(db.label[1, .x], ",") + 1 
  #     tidyr::separate(db.label, col = .x, paste0(.x, "_", 1:n), ",")
  #   })
  #   
  # }
  

  ############################ MULTIPLE ANSWER QUESTIONS #######################

  # Get multiple answer question from form
  q.multi.form <- form[which(form$type == "select_multiple"), ]

  # Get character vector of question names/label to replace with
  q.multi.replace <- q.multi.form %>%
    dplyr::select(dplyr::matches(paste0("\\.", output))) %>%
    tidyr::unite(
      !!paste0("q.", output),
      c(paste0("q.", output), paste0("a.", output)),
      sep = "/"
    ) %>%
    dplyr::pull(.)

  # Get character vector of question names/label that will be replaced
  q.multi.find <- q.multi.form %>%
    dplyr::select(dplyr::matches(paste0("\\.", input))) %>%
    tidyr::unite(
      !!paste0("q.", input),
      c(paste0("q.", input), paste0("a.", input)),
      sep = "/"
    ) %>%
    dplyr::pull(.)
  

  if (any(!(q.multi.find %in% colnames(db.label)))) {

    cat(
      "\nThe following multiple answer question names are the problematic in the data base:\n",
      paste(
        utils::capture.output(
          #q.multi.find[which(!(q.multi.find %in% colnames(db.label)))]
          unique(gsub("(.+)/.*$", "\\1", q.multi.find[which(!(q.multi.find %in% colnames(db.label)))]))
        ), 
        collapse = "\n"
      ) #, immediate. = TRUE
    )
  }
  
  # named vector with find and replace
  q.multi <- rlang::set_names(q.multi.find, q.multi.replace)

  # Replace for multiple answers
  db.out <- db.label %>%
     dplyr::rename(!!!q.multi) %>%
    {
      if (!is.null(uid)) {
        dplyr::rename(., !!uid.sym := unique.id)
      } else .
    }
    
  ################################ CLEAN AND OUTPUT ############################
  
  # Clean and return
  db.out <- R.HNAP::CleanIt(db.out)

  return(db.out)
}
