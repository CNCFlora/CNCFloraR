fill_followUpTable_with_citations_from_FloraFungaBrasil <- function(list = ""){

  # Load packages ####

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(httr)
        library(jsonlite)
        library(rlist)
        library(kewr)
        library(dplyr)
        library(tidyr)
        library(stringr)
        library(rapportools)
        library(googledrive)
        library(data.table)

      })
    })
  })


  if(list[1] == ""){

    # Get list of species file (species_for_FloraFungaBrasil_citations.csv) ####

    ## Get local path of the downloaded list of species file ####

    listOfSpecies_localPath <- paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_FloraFungaBrasil_citations.csv"

    )

    ## Import the list of species file from local path ####

    listOfSpecies <- fread(

      listOfSpecies_localPath,
      header = F,
      sep = ";",
      encoding = "UTF-8"

    )

  } else {

    listOfSpecies <- data.frame(

      V1 = list

    )

  }


  # Import the citations from Flora e Funga do Brasil file ####

  citationsFloraFungaBrasil <- fread(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/outputs/citationsFloraFungaBrasil/citationsFloraFungaBrasil.csv"

      ),
    header = T,
    sep = ";",
    encoding = "UTF-8"

  )


  # Get follow-up table (follow-up_table.csv)

  ## Get local path of the downloaded follow-up table file ####

  followUpTable_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/follow-up_table/follow-up_table.csv"

    )

  ## Import the list of species file from local path ####

  followUpTable <- fread(

    followUpTable_localPath,
    header = T,
    sep = ";",
    encoding = "UTF-8"

  )

  for(sp in as.vector(as.matrix(listOfSpecies[,1]))){

    if(sp %in% as.vector(as.matrix(listOfSpecies[,1])))
    followUpTable$zcitationFB2020[followUpTable$NameFB_semAutor == sp] <-
        citationsFloraFungaBrasil$zcitationFB[citationsFloraFungaBrasil$species == sp]
    followUpTable$zcitationFB2020_short[followUpTable$NameFB_semAutor == sp] <-
      citationsFloraFungaBrasil$zcitationFB_short[citationsFloraFungaBrasil$species == sp]

  }

  write.csv2(

    followUpTable,
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/follow-up_table/follow-up_table.csv"

      ),
    row.names = F,
    fileEncoding = "UTF-8"


  )

  message("Done.")

}
