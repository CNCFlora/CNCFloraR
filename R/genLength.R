#' Generation length
#'
#' Calcula o tempo de geração
#'
#' @param firstRep First year of reproduction (numeric)
#' @param lastRep Last year of reproduction (numeric)
#'
#' @returns a numeric value
#'
#' @author Eduardo Amorim, \email{eduardoamorim@@jbrj.gov.br}


genLength <- function(firstRep, lastRep){

  firstRep <- firstRep
  lastRep <- lastRep
  coef <- 0.5 # Taxa de recrutamento
  lengthRep <- lastRep - firstRep
  genLength <- firstRep + (coef * lengthRep)
  return(genLength)

}
