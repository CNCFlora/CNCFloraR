#' Round down to one decimal place
#'
#' Arredonda a primeira casa decimal para baixo

roundDownFirstDecimalPlace <- function(x) {

  x <- x * 10
  floor(x) / 10

}
