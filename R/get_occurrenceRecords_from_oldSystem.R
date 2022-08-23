#' Load occurrence records from old system of CNCFlora
#'
#' Carrega os dados dos registros de ocorrências do arquivo HTML obtido a partir do antigo sistema CNCFlora


get_occurrenceRecords_from_oldSystem <- function(Species){

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(rvest)

      })
    })
  })

  # Get local path of the downloaded occurrences file ####

  occurrences_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/occurrences/oldSystem/",
    Species,
    ".html"

    )


  # Import the occurrences file from local path ####

  occurrences <- read_html(occurrences_localPath)

  return(occurrences)

}

