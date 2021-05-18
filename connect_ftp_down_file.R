################################################################################
## @Title: ftp_download_file.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 11.05.2020
## @Last updated:
## @Status: Questioning (see ftp_get_data.R)
## ------------------------------------------------------------------------------
## Notes:
##
## ----------------------------------------------------------------------------
#' @name CurlFethDisk
#' @title Downlaod files from FTP to disk
#'
#' @description Helper to cache files from an FTP server to disk
#'
#' @usage
#' CurlFethDisk(file.url, file.cache, userpwd)
#'
#' @param file.url URL to the file
#' @param file.cache an existing directory where to cache the file
#' @param userpwd user and passwor to acces the server in the form "user:pass"
#'
#' @details
#' If the a file with the same name already exists in the cache directory, it
#' triggers a message informing and does not download the file
#'
#' @return Files downloaded into the cache directory
#'
#' @examples
#' \dontrun{
#' # Create directories to cache files
#' dir.cache <- "./hnap_platform"
#' file.cache <- file.path(dir.cache, gsub("(:|/|\\.)", "_", file.url))
#' lapply(file.cache, function(x) dir.create(x, showWarnings = FALSE))
#'
#' # Cache multiple files from a list of URLs
#' mapply(CurlFethDisk, file.url, file.cache, userpwd)
#' }
#'
#' @export
## ------------------------------------------------------------------------------
CurlFethDisk <- function(file.url, file.cache, userpwd) {
  start.file <- Sys.time()
  # check if file exist
  if (length(list.files(file.cache)) != 0) {
    cat("\nThis file already exist, will not be downloaded:", file.url)
    return()
  } else {
    # file cache as path for destination
    file.cache <- paste0(file.cache, "/", basename(file.url))
  }
  # handle auth
  file.handle <- curl::new_handle()
  curl::handle_setopt(file.handle, userpwd = userpwd)
  # Download
  curl::curl_download(file.url, file.cache, handle = file.handle)
  # Report downloadign times
  cat("\nFile downloaded to: ", file.cache)
  end.file <- Sys.time()
  time.file <- end.file - start.file
  cat("\nTime download:", time.file)
}
