################################################################################
## @Title: analysis_similarity.R
## @Author: Julian Ibarguen
## @Contact: jibarguen@iom.int
## @Date created: 21.10.2020
## @Last updated: 
## @Status: Maturing
## -----------------------------------------------------------------------------
## Notes:
##  Enhance by making the filtering not limited to two grouping variables
##
##
## ----------------------------------------------------------------------------
#' @name Similarity
#' @title Similarity measures for HH Survey Interviews
#'
#' @description Calculates ICC and/or Fleiss' Kappa as similarity measure 
#' between household survey interviews
#'
#' @usage
#' Similarity(db, groups, variables, num.vars = NULL, cat.vars = NULL, 
#' alpha = FALSE, p.value = TRUE, detail = TRUE, plots = FALSE) 
#'
#' @param db a data frame containing the grouping variables for 
#' similarity analysis
#' @param groups a character vector with the name of the variables to be used tp
#' conform the groups of household for which similarity of interviews wants to be
#' measure. Works for two grouping variables
#' @param num.vars a character vector with the name of the numeric variables for
#' which similarity want to be assess. Intra-Class Correlation coefficient is 
#' calculated for numeric variables
#' @param cat.vars a character vector with the name of the categorical variables
#' for which similarity want to be assess. Fleiss' Kappa is calculated for 
#' categorical variables
#' @param alpha logical to compute for Cronbach's alpha, Guttman's gamma
#' and Persons rho for numeric variables
#' @param p.value logical to return the p-values for the ICC coefficients
#' @param detail As given by irr:kappam.fleiss(). A logical indicating whether 
#' category-wise Kappas should be computed
#' @param plots logical to print heatmap plots to viasualize similarity
#'
#' @details
#' Uses psych:ICC() and psych::alpha to return the ICC, Cronbach's alpha,
#' Guttman's gamma and Persons rho for numeric variables (num.vars) and
#' irr::kappam.fleiss() for categorical variables (cat.num). Refer to these 
#' packages for more info on the measurement.
#' 
#' A typical use for this function in R.HNAP is to observe the similarity of the
#' interviews conducted by an interviewer in a specific pop.group/admin level/area.
#' In this cases it will return: 1) the number of interviews conducted by an 
#' interviewer in the given area; 2) the similarity as per ICC between the 
#' interviews for the given variables; 3) the p-value of the the ICC coefficient  
#'
#' @return A data frame with the sample, the requested similarity measures 
#' coefficients and its corresponding p-value for each group defined by the 
#' grouping variables.
#' 
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   interviewer = sample(letters[1:3], 100, replace = TRUE),
#'   admin = sample(letters[6:10], 100, replace = TRUE),
#'   num1 = rnorm(100, 4.3, 2.3),
#'   num2 = rnorm(100, 7.5, 10.4),
#'   num3 = rnorm(100, 6.8, 3.6),
#'   cat1 = sample(letters[11:15], 100, replace = TRUE),
#'   cat2 = sample(letters[16:22], 100, replace = TRUE)
#' )
#' 
#' s <- Similarity(
#'   dat, groups = c("interviewer", "admin"),
#'   num.vars = c("num1", "num2", "num3") #, cat.vars = c("cat1", "cat2")
#' )
#'
#' @export
## -----------------------------------------------------------------------------

Similarity <- function(db, groups, variables, num.vars = NULL, cat.vars = NULL, 
                       alpha = FALSE, p.value = TRUE, detail = TRUE, plots = FALSE) {
  
  names.sym <- rlang::syms(groups)
  
  # remove cases with only one interview (one single rater)
  db <- db %>%
    dplyr::add_count(!!names.sym[[1]], !!names.sym[[2]]) %>%
    dplyr::filter(n > 1 )
  
  # Get units for which to compute ICC
  index <- lapply(dplyr::distinct(db[, groups]), "[") 

  cat("\nVariables for similarity:\n")
  print(paste(num.vars, cat.vars))
  
  simil <- purrr::pmap(
    index, ~ {

      # Filter and format to matrix n*m df for analysis (get number of locations)
      x <- db %>% 
        dplyr::filter(
          !!names.sym[[1]] == ..1, #"ALP52" , 
          !!names.sym[[2]] == ..2 #"SY020600"
        )

      # create empty storage df with idneitfiers
      simil <- tibble::tibble(!!names.sym[[1]] := ..1, !!names.sym[[2]] := ..2)
      simil["n"] <- x$n[1]
      
      print(paste(..1, ..2))

      # ICC for continous vairables
      if (!is.null(num.vars)) {
        
        # Subset for numerical variables
        num.x <- x %>%
          dplyr::select(dplyr::all_of(num.vars), -c(!!!names.sym), -n) %>%
          t(.)
        
        if (plots == TRUE) {
          p <- as.data.frame(num.x) %>%
            dplyr::mutate(quest = row.names(num.x)) %>%
            tidyr::pivot_longer(-quest, names_to = "rater") %>%
            {
              ggplot2::ggplot(., ggplot2::aes(x = rater, y = quest, fill = value)) +
                ggplot2::geom_tile() +
                ggplot2::labs(
                  title = paste(..1, "-", ..2),
                  caption = paste(
                    "ICC1:", simil["coef_ICC1"], "; ICC2:", simil["coef_ICC2"],
                    "; ICC3:", simil["coef_ICC3"],
                  )
                )
            }
          print(p)
        }

        # calculate icc and p values
        icc <- psych::ICC(num.x, lmer = FALSE, check.keys = FALSE)
        
          simil["coef_ICC1"] <- icc$results[1, 2]  
          simil["coef_ICC2"] <- icc$results[2, 2] 
          simil["coef_ICC3"] <- icc$results[3, 2]
          
        if (p.value == TRUE) {

          simil["p_ICC1"] <- icc$results[1, 6]
          simil["p_ICC2"] <- icc$results[2, 6]
          simil["p_ICC3"] <- icc$results[3, 6]
          simil["p_ICC3"] <- icc$results[3, 6]
          
        }
        # calculate alpha, Guttman icc and average Perason's r
        
        if (alpha == TRUE) {
          
          max <- max(apply(num.x, 2, function(x) length(unique(x))))
  
          num.alpha <- x %>%
            dplyr::select(dplyr::all_of(num.vars), -c(!!!names.sym), -n) # should be traspose? I believe no
  
          alpha <- try(psych::alpha(num.alpha, check.keys = TRUE, max = max))

          if (class(alpha) != "try-error") {
            simil["coef_alpha"] <- alpha$total$std.alpha
            simil["coef_g6"] <- alpha$total$`G6(smc)`
            simil["coef_avg.r"] <- alpha$total$average_r
          }
        }
      } 
      
      # Fleiss Kappa for categoricla and short ordinals
      if (!is.null(cat.vars)) {
        
        # Subset for categorical variables
        cat.x <- x %>%
          dplyr::select(dplyr::all_of(cat.vars), -c(!!!names.sym), -n) %>%
          t(.)

        # calculate icc and p values
        fleiss <- irr::kappam.fleiss(cat.x, detail = detail)
        
        simil["coef_fleiss"] <- fleiss$value 
        
        if (p.value == TRUE) {
          simil["p_fleiss"] <- fleiss$p.value 
        }
      }
      
      invisible(simil)
    })
  
  simil.df <- dplyr::bind_rows(simil) %>%
    dplyr::rename("n.hh" = n) # added to better clarify what is "n" int he data frame
  
  return(simil.df)
}




