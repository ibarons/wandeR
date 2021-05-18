################################################################################
## @Title: ftp_get_data.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 11.05.2020
## @Last updated:
## @Status: Questioning (It bot being used, FTP server is good patch for data centralization, depends much on updating library and IM naming)
## ------------------------------------------------------------------------------
## Notes:
## Add option to select the type of file (.xlsx, .pdf, .doc)
##
## Scale to work also in other servers different than HNAP platform.
## 
## Replace "curl::" by "httr::" to optimize package dependencies
##
## ----------------------------------------------------------------------------
#' @name FTPGetData
#' @title Connect to HNAP Platform and downlaod anyt selected files
#'
#' @description Access a the platfomr, explore its directories and cache files
#' that matched the input regex.
#'
#' @usage
#' FTPGetData(ftp.url = "ftp://hnap.info", userpwd, lib.regex = NULL,
#' dir.regex = NULL)
#'
#' @param ftp.url URL for the HNAP server, of the type: "ftp://~"
#' @param userpwd as string separated by ":", containing the credentials to access
#' the server: user and password, of the type "user:pass"
#' @param lib.regex a regex to match the files names of the library dictionary the
#' files to download (e.g `MNM.*2019`` download all MNM files of 2019; if
#' want to download all files use `.``).
#' @param dir.regex a regex to match directly the folder id to select the files to
#' download (e.g. `\\/2[3-3]\\/$``). Note there is only one file per folder
#' in hnap server. Chose between dir.regex and raw.regex, do not use
#' simultaneously
#'
#' @return a directory called "~/99.HNAP_Platform/" with select files downloaded
#'
#' @examples
#' \dontrun{
#' R.HNAP::FTPGetData(userpwd, dir.regex = "[3][0-9][0-9]")
#' }
#' @export
## ------------------------------------------------------------------------------
FTPGetData <- function(ftp.url = "ftp://hnap.info", userpwd, lib.regex = NULL,
                       dir.regex = NULL) {

  ######################### PREELIMINARIES & SOURCE ############################
  start.all <- Sys.time()
  # base ftp.url to build dirs urls
  ftp_base <- paste0(ftp.url, "/%s/")
  # Local directory to store download
  dir.cache <- "./hnap_platform"
  dir.create(dir.cache, showWarnings = FALSE)

  ######################## GET & SELECR DIRECTORY URLS #########################
  # options for handleling the connection
  dir.handle <- curl::new_handle(
    ftp_use_epsv = FALSE, dirlistonly = TRUE,
    crlf = TRUE, ssl_verifypeer = FALSE,
    ftp_response_timeout = 30, dirlistonly = TRUE
  )
  curl::handle_setopt(dir.handle, userpwd = userpwd)

  # Fetch server directories (folders)
  dir.res <- curl::curl_fetch_memory(ftp.url, handle = dir.handle)
  dir.con <- rawConnection(dir.res$content)
  dir <- readLines(dir.con)
  close(dir.con)
  # Build directories urls
  dir.url <- sprintf(ftp_base, dir)
  print(dir.url)

  # select directory
  if (!is.null(dir.regex)) {
    dir.down <- grep(dir.regex, dir.url, value = TRUE)
  } else if (!is.null(lib.regex)) {
    ftp.lib <- R.HNAP::ftp.file.hnap # This is not update in `sysdata.rda`!!
    lib.regex <- paste0("/", ftp.lib[grep(lib.regex, ftp.lib[, 2]), 1], "/",
      collapse = "|"
    )
    dir.down <- grep(lib.regex, dir.url, value = TRUE)
  } else if (!is.null(dir.regex) & !is.null(lib.regex)) {
    warning("Please enter a regex to select files. If you want to download all 
        files enter '.'")
  }

  ############################## GET FILES URL ###################################
  cat("\nThe following", length(dir.down), "files will be downloaded:")

  file.url <- vector("list", length(dir.down))
  names(file.url) <- dir.down
  for (i in dir.down) {
    file.res <- curl::curl_fetch_memory(i, handle = dir.handle)
    file.con <- rawConnection(file.res$content)
    file.temp <- readLines(file.con)
    file.url[[i]] <- paste0(i, grep("xls.*$|pdf$|doc.*$", file.temp,
      value = TRUE
    ))
    close(file.con)
    cat("\n", file.url[[i]], "\n")
  }

  ############################## DOWNLOAD FILES ################################
  # File's destination local directory for download
  file.cache <- file.path(dir.cache, gsub("(:|/|\\.)", "_", file.url))
  lapply(file.cache, function(x) dir.create(x, showWarnings = FALSE))

  cat("\nStarting to download files:\n")
  # Apply download function to the multipel url
  mapply(CurlFethDisk, file.url, file.cache, userpwd)

  ############################## RETURN ########################################
  invisible(dir.url)

  end.all <- Sys.time()
  time.all <- end.all - start.all
  cat("\nWhole completed in:\n")
  print(time.all)
}
