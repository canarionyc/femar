
# ?match
#' @export
currency_to_number <- function(cur_vec){
  # browser()

  x <- stri_extract_first_regex(cur_vec,'\\d+\\.?\\d+') %>% as.numeric() # %>% suppressWarnings()
  # print(table(x))
  y <- stri_extract_last_regex(cur_vec, "\\w")
  # print(table(y))
  abb <- c('', 'K', 'M', 'B')
  act <- c(1, 1e3, 1e6, 1e9)
  x * act[match(y, abb)]
}

