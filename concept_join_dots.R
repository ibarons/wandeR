

df1 <- data.frame(x = 1:10, y = letters[1:10], z = LETTERS[1:10])
df2 <- data.frame(x = 1:10, y = letters[1:10], z = LETTERS[1:10])


f <- function(...) {
  
  dots <- rlang::enquos(...)

  df1 %>%
    dplyr::group_by(...) %>%
    dplyr::full_join(., df2, by = purrr::map_chr(dots, rlang::as_label))


}

f(z)