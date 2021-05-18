################################################################################
## @Title: analysis_geom_series.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 18.12.2020
## @Last updated:
## @Status: Questioning
## ------------------------------------------------------------------------------
## Notes:
##
## ----------------------------------------------------------------------------
#' @name GeomSerieZ
#' @title Create geometrict series
#'
#' @description Create geometric series and recale (0-1) or standardize (Z) them
#'
#' @usage
#' GeomSerieZ(k, n, a = 1, output = c("natural", "rescale", "standardize"))
#'
#' @param k multiplicative factor. defined the distance between the elements of the serie
#' @param n length of the serie
#' @param a Starting number
#' @param output chose which serie you want to output: c("natural", "rescale", "standardize")
#'
#' @return A matrix with each type of output in one colums.
#'
#' @examples
#' \dontrun{
#' GetForms(server.url, kobo.user, kobo.pass)
#' }
#'
#' @export
## ------------------------------------------------------------------------------

GeomSerieZ <- function(k, n, a = 1, output = c("natural", "rescale", "standardize")) {
  
  x <- c()
  z <- c() 
  out <- c()

  for (n in 1:n) {
    e <- n
    x[n] <- a * k^e
  }
  
  if("natural" %in% output){
    out[["natural"]] <- x
  }
  
  if ("rescale" %in% output) {
    r <- x / sum(x)
    out[["rescale"]] <- r
  }

  if ("standardize" %in% output) {
    z <- (x - mean(x)) / sd(x)
    out[["standard"]] <- z
  }
  
  return(as.matrix(as.data.frame(out)))
}
