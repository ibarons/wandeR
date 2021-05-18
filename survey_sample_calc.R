################################################################################
## @Title: get_base_kfinds.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 03.04.2020
## @Last updated: 05.04.2020
## @Status: Questioning: Eventually will be replace by V2
## @Description: output sample size or error, depends on input. Formulas based
##  on United Nations Statistics Division (2008) Designing household survey
##  samples: practical guidelines
## ------------------------------------------------------------------------------
##
## Notes:
## Pending inclusion of sample for mean and sample for comparisions
##
## Pending inclusion of N & N_avg parameter in the calculation of errors
##
## Consider adding qt for t-distribution confidence level (optional?)
##
## Consider adding SRSWR and SRSWOR options
##
## ------------------------------------------------------------------------------
################################################################################
## ----------------------------------------------------------------------------
#' @name SampleCalc
#' @title Calculate sample size/error.
#'
#' @description Calculate sample size or error (if `n`` is given) for the
#' defined parameters.
#'
#' @usage
#' SampleCalc(e = 0.05, n = NULL, a = 0.05, p = 0.5, deff = 2, k = 0.1, N = 1, 
#' N_prop = 1, N_avg = 1)
#'
#' @param n sample size, default to NULL. If n is provided returns error.
#' @param e marging of error, default to 0.1. If n is provided, return error.
#' @param a alpha level to comput level of confidence. default to 0.05 for a 95%.
#' confidence level.
#' @param p estimated probability of the indicator.
#' conservative  estimate.
#' @param deff design eefect for cluster designs.
#' design, set to 1.
#' @param k expected percentage of non-response.
#' non-response rate.
#' @param N percentage of the target population over the total popualtion.
#' Defualt to 1, i.e. all population is targeted. If wanted for example to target
#' women 15-49, set percentage of 15-49 over total pop.
#' @param N_prop proportion of the target population over the total population. 
#' Use when the target population is a specific population group, within the  
#' Basic Sample Unit (e.g. group of individuals within a household); it returns 
#' the needed sample of BSU to achieve the sampled target population. 
#' @param N_avg Define the average household size. Use togetehr with N_prop
#'
#' @details
#' The following formulas are applied...
#'
#' @references
#' Shalabh. (n.d.). Sampling Theory [University Course]. MTH 417:
#' Sampling Theory. Retrieved May 15, 2020, from
#' http://home.iitk.ac.in/~shalab/course1.htm
#' United Nations Statistics Division (Ed.). (2005). Household surveys in
#' developing and transition countries. United Nations.
#' https://ec.europa.eu/eurostat/ramon/statmanuals/files/UNSD_household_sample_surveys_developing_transition_countries_2005_EN.pdf
#' United Nations Statistics Division (Ed.). (2008). Designing household survey
#' samples: Practical guidelines. United Nations.
#' https://unstats.un.org/unsd/demographic/sources/surveys/Series_F98en.pdf
#' Lohr (2009) Sampling: Design and Analysis
#'
#' @return The correspongin sample size or error.
#'
#' @examples
#' # Get sample size for a given error with defautl parameters
#' SampleCalc(0.05)
#'
#' # Get error for a given sample size with defautl parameters
#' SampleCalc(n = 845)
#'
#' @export
## ----------------------------------------------------------------------------
SampleCalc <- function(e = 0.05, n = NULL, a = 0.05, p = 0.5, deff = 2,
                       k = 0.1, N = 1, N_prop = 1, N_avg = 1) {

  # Calculate Z for the given alpha
  Z <- stats::qnorm(1 - (a / 2))
  # Calculate sample size / erro  for given parameters
  if (is.null(n)) {
    
    n0 <- (Z^2 * p * (1 - p)) / e^2
    #n_deff <- n0 * deff * (1 + k) 
    
  #n <- (Z^2 * p * (1 - p) * deff * (1 + k)) / (e^2 * N_prop * N_avg)
    
    if (N != 1) {
      frac = n0 / N
      fpc <- 1 - frac
      
      # if (n0 >= N) { # Lohr says if n0 > N -> n = N. While that might make sense to pop of ~1000 were diff between n0 and n_fpc are small, it does not for pops of ~100 might not do for smaller population of ~100. Preferred to provide both and choose the one you like
      #   n_fpc <- N
      #   n <- n_fpc
      # 
      # } else {
        
        n_fpc <- n0 / (1 + frac)
        n <- n_fpc * (1+k) * deff / (N_prop * N_avg)
        
        if (n >= N) {n <- N}
      #}
      
      return(list(n = n, n_fpc = n_fpc, n0 = n0, frac = frac, fpc = fpc))

    } else {
      
      n <- n0 * (1+k) * deff / (N_prop * N_avg)  
      if (n >= N) {n <- n0}
      
      return(list(n = n, n0 = n0))
    }

  
  } else {
      
    e <- Z * sqrt(p * (1 - p) / (n / (deff * (1 + k))))
    
    return(e)
  }
}
