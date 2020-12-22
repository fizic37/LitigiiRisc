# Basic string search function in a safely way

check_dosare <- function(my_pattern, my_string) {
  stringr::str_which(string = my_string, pattern = my_pattern)
}

check_dosare_safely <- purrr::safely(.f = check_dosare)


get_index <- function(my_string,my_pattern) {
  purrr::map(.x = my_pattern,
             ~check_dosare_safely(my_pattern = .x, my_string = my_string)) %>% 
    purrr::map("result") %>% 
    purrr::map_dbl(.x = .,~purrr::detect_index(.x,~!is.null(.x))) %>% which(x = .>0)
}