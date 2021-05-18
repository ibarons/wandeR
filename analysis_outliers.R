################################################################################
## @Title: CalcOutliers
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 15.04.2020
## @Last updated: 04.06.2020
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes:
## Scale to become a general; function for outliers with arg `method` to specify
## to method
##
## Create argument output that allow to chose if report fences, number of cases
## and/or case value
##
## ----------------------------------------------------------------------------
#' @name CalcOutliers
#' @title Identify outliers
#'
#' @description Identify outliers with various methods
#'
#' @usage
#' CalcOutliers(x, method = "tukey", fence = 1.5, ...)
#'
#' @param x a numeric vector
#' @param method method for calculating outliers `c("tukey", "adjusted")`.
#' @param fence a numeric scales indicating the factor off Q1 and Q3 from which
#' we consider outlier values. Defaults to 1.5, inner fence (a.k.a possible
#' outlier); For more robust result use outer fence = 3 (a.k.a extreme outlier)
#' @param ... any other argument passed to the function (e.g. na.rm)
#'
#' @details
#' Adjusted boxplot method calculated with robustbase::adjboxStats()
#'
#' @references
#' Hubert, M., & Vandervieren, E. (2008). An adjusted boxplot for skewed
#'  distributions. Computational Statistics & Data Analysis, 52(12), 5186–5201.
#'  https://doi.org/10.1016/j.csda.2007.11.008
#'
#' Seo, S. (2006). A Review and Comparison of Methods for  Detecting Outliers in
#'  Univariate Data Sets [Thesis]. http://d-scholarship.pitt.edu/7948/1/Seo.pdf
#'
#' @return A nested list with the first level items containing the results for 
#' each method (tukey and/or adjusted), For each method, the seconds level
#' contains two outputs: 1st the boundary values; 2nd a logical vector indicating
#' if the value is outlier (TRUE) or no (FALSE) 
#'
#' @examples
#' x <- mtcars[, "wt"]
#'
#' # Box plot of the distribution
#' boxplot(x, lwd = 2, ylab = "NUMS")
#' stripchart(x, vertical = TRUE, method = "jitter", add = TRUE, col = "grey80")
#'
#' # Identify lower/upper outlying bound for different method
#' CalcOutliers(x, method = c("tukey", "adjusted"), fence = 3)
#'
#' @export
## -----------------------------------------------------------------------------
CalcOutliers <- function(x, method = "tukey", fence = 1.5, ...) {
  q25 <- stats::quantile(x, ...)[[2]]
  q75 <- stats::quantile(x, ...)[[4]]
  iqr <- stats::IQR(x, ...)

  # Classic box plot as per Tukey 1977
  tukey <- c()
  if ("tukey" %in% method) {
    # Get lower/upper bound
    lower <- q25 - fence * iqr
    upper <- q75 + fence * iqr

    tukey[["bounds"]] <- data.frame(lower = lower, upper = upper)
    tukey[["out"]]<- dplyr::if_else(x < lower | x > upper, TRUE, FALSE)
  }
  # Adjust box plot as per Hubert (2008)
  adjusted <- c()
  if ("adjusted" %in% method) {
    adjusted[["bounds"]] <- robustbase::adjboxStats(x, coef = fence)[["fence"]]

    adjusted[["out"]]<- dplyr::if_else(
      x < adjusted[["bounds"]][1] | x > adjusted[["bounds"]][2], TRUE, FALSE
    )
  }
  outputs <- list(tukey = tukey, adjusted = adjusted)
  return(outputs[lengths(outputs) != 0])
}


