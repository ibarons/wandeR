################################################################################
## Title: format_join_lists.R
## Author: https://stackoverflow.com/questions/18538977/combine-merge-lists-by-elements-names
## Contact: jibarguen@iom.int
## Date created: 10.03.2021
## Last updated: 
## Status: maturing
## ------------------------------------------------------------------------------
## Notes:
## Consider extending to 3 or more list
## ----------------------------------------------------------------------------
#' @name JoinLists
#' @title Join two list by name
#'
#' @description Join two list by name
#'
#' @usage
#' JoinLists(list1, list2)
#'
#' @param list1 a names list
#' @param list2 another names list
#'
#' @return a single list with element under the same name joined
#'
#' @examples
#' 
#' l1 <- list(x1 = 1:10, x2 = 1:10, x3 = 1:10)
#' l2 <- list(y1 = 1:10, x2 = 1:10, y3 = 1:10)
#' 
#' JoinLists(l1, l2)
#'
#' @export
## -----------------------------------------------------------------------------

JoinLists <- function(list1, list2) {  
  
  keys <- unique(c(names(list1), names(list2)))
  purrr::map2(list1[keys], list2[keys], c) %>% 
    set_names(keys)  
  
}





