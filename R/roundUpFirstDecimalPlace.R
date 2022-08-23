#' Round up to one decimal place
#'
#' Arredonda a primeira casa decimal para cima

roundUpFirstDecimalPlace <- function(x) {

  x <- x * 10
  x <- ceiling(x) / 10
  return(x)

}
