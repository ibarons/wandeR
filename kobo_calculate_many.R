
# path.to.xlsx. Enter a path to an xlsx name with the `name` of the questions you want to calculate
# Enter the operator


KoboCalculate <- function(path.to.xlsx, operator = "+") {
  
  db <- readxl::read_excel(path.to.xlsx)
  
  calculate <- paste0("${", db$name, "}", collapse = operator)
  
  return(calculate)
  
}


KoboCalculate("../db/kobo_calculation.xlsx", "+")