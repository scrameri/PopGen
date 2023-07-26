addN <- function(x, N.char = "n=", brackets.char = "()", sep = " ") {
  require(dplyr)
  b1 <- substring(brackets.char,1,1)
  b2 <- substring(brackets.char,2,2)
  dx <- (data.frame(V = x) %>% dplyr::add_count(V))
  unname(apply(dx, 1, function(x) {
    paste0(x[1], sep, b1, N.char, as.numeric(x[2]), b2)
  }))
}
