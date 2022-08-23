yearOfCollections_from_oldSystem <- function(occurrenceRecords){

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(rvest)
        library(tidyr)
        library(dplyr)

      })
    })
  })

  collectionYear <- occurrenceRecords %>%
    html_nodes(".col-md-5") %>%
    html_nodes(".form-control") %>%
    html_attr("value")

  collectionYear <- sub("^[0-9][0-9]$", NA, collectionYear)
  collectionYear <- sub("^[0-9]$", NA, collectionYear)

  n_coletas <- length(collectionYear)

  collectionYear <- as.numeric(collectionYear)
  collectionYear <- drop_na(as.data.frame(collectionYear))
  collectionYear <- collectionYear %>% arrange(collectionYear)
  collectionYear <- unique(collectionYear)

  return(collectionYear$collectionYear)

}

