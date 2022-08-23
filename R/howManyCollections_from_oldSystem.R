howManyCollections_from_oldSystem <- function(occurrenceRecords){

  collectionsNumber <- occurrenceRecords %>%
    html_nodes(".col-md-5") %>%
    html_nodes(".form-control") %>%
    html_attr("value")

  collectionsNumber <- sub("^[0-9][0-9]$", NA, collectionsNumber)
  collectionsNumber <- sub("^[0-9]$", NA, collectionsNumber)

  collectionsNumber <- length(collectionsNumber)

  return(collectionsNumber)

}
