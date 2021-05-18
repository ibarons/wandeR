
df <- data.frame(
  grp = sample(letters[1:5], 1000, replace = TRUE),
  x = runif(n = 1000),
  y = sample(1:10, 1000, replace = TRUE)
)


# Filter with expression. Potential incorporation to SurveyTbl and SurveyExtracttbl

f <- function(df, filter = TRUE) {
  # Enter quoted filter expression. If string should be added in the expression use
  # single quotes ('xx') CANNOT take comma ",", use "&" or "|" instead for chaining filter expressions
  # filter = TRUE for NO filter
  
  filter.exp <- parse(text = filter)
  
  dplyr::filter(df, eval(filter.exp))
  
}

f(df, filter = "grp == 'a' & y %in% 6:7") # filter with desired exp
f(df, filter = TRUE) # no filter