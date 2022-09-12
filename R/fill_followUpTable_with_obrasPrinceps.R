fill_followUpTable_with_obrasPrinceps <- function(){

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


  # Get list of species file (species_for_obraPrinceps.csv) ####

  ## Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_obraPrinceps.csv"

    )


  ## Import the list of species file from local path ####

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = T,
    sep = ";",
    encoding = "UTF-8"

  )


  # Import the obras princeps file ####

  obrasPrinceps <- fread(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/outputs/obrasPrinceps/obrasPrinceps.csv"

      ),
    header = T,
    sep = ";",
    encoding = "UTF-8"

  )

  obrasPrinceps <- obrasPrinceps %>% dplyr::select(Taxon, Revision)

  colnames(obrasPrinceps) <- c("Species", "obra")


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
    followUpTable$zobra[followUpTable$NameFB_semAutor == sp] <- obrasPrinceps$obra[obrasPrinceps$Species == sp]

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
