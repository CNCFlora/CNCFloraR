#' Get data of a list of species from follow up table
#'
#' Obter os dados de uma lista de espécies da tabela de acompanhamento

get_species_from_followUpTable <- function(){

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(googlesheets4)
        library(dplyr)
        library(tidyr)
        library(rredlist)
        library(data.table)

      })
    })
  })


  # Get list of species file (species_to_get_from_followUpTable.csv) ####

  ## Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_to_get_from_followUpTable.csv"

    )

  ## Ask to open the list of species file ####

  answer <- ""

  while(answer != "Y" |
        answer != "N" ){

    answer <-
      toupper(readline("Open the list of species file? (y/n): "))

    if(answer == "Y"){

      shell(listOfSpecies_localPath)

      answer2 <- ""

      while(answer2 != "Y"){

        answer2 <-
          toupper(readline("List of species file ready? (y): "))

        if(answer2 == "Y"){

          break

        }

      }

      break

    }

    if(answer == "N"){

      break

    }

  }

  ## Import the list of species file from local path ####

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = F,
    sep = ",",
    encoding = "UTF-8"

  )


  # Get follow-up table (follow-up_table.csv) ####

  ## Get local path of the downloaded follow-up table file ####

  followUpTable_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/follow-up_table/follow-up_table.csv"

    )


  ## Import the list of species file from local path ####

  followUpTable <- fread(

    followUpTable_localPath,
    header = T,
    sep = ";"

  )


  # Number of species vector ####

  colnames(listOfSpecies) <- "NameFB_semAutor"
  species_n <- 1:as.numeric(count(listOfSpecies))


  # Load follow-up table from GoogleSheets ####

  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")
  read_ss <- read_sheet(ss, sheet = 1)


  # Bind followUpTable and listOfSpecies ####

  followUpTable <- bind_rows(followUpTable, listOfSpecies)


  # Loop start ####

  for(i in species_n){

    SPECIES <- listOfSpecies[i,]
    SPECIES <- as.vector(as.matrix(SPECIES))
    info_species <- read_ss %>% dplyr::filter(NameFB_semAutor == SPECIES)
    species_i <- grep(SPECIES, followUpTable$NameFB_semAutor)

    if(length(followUpTable$NameFB_semAutor[species_i]) == 1){

      followUpTable$recorte[species_i] <- info_species$Recorte
      followUpTable$FB2020_Family[species_i] <- info_species$FB2020_Family
      followUpTable$FB2020_AcceptedNameUsage[species_i] <- info_species$FB2020_AcceptedNameUsage
      followUpTable$FB2020_Taxonomic_Status[species_i] <- info_species$FB2020_Taxonomic_Status
      followUpTable$taxonID[species_i] <- info_species$taxonID
      followUpTable$vernacular_names[species_i] <- info_species$vernacular_names
      followUpTable$FB2020_endemism[species_i] <- info_species$FB2020_endemism
      followUpTable$occurrenceRemarks[species_i] <- info_species$occurrenceRemarks
      followUpTable$location[species_i] <- info_species$location
      followUpTable$lifeForm[species_i] <- info_species$lifeForm
      followUpTable$vegetationType[species_i] <- info_species$vegetationType
      followUpTable$habitat[species_i] <- info_species$habitat
      followUpTable$IUCN_assessment_presence[species_i] <- if(is.na(info_species$IUCN_assessment_presence)|info_species$IUCN_assessment_presence=="NO"){NA} else{IUCN_sp<-rl_sp_citation(SPECIES, key = "f4f963c4608333f2829e262fa6bdfa1e6a18cd0dfc3cc2cbf82c13fa3c78deeb"); IUCN_url<-sub("^.*https", "https", IUCN_sp$result$citation); IUCN_url<-sub("\\s\\.Downloaded.*$", "", IUCN_url); sub("\\s\\(.*\\)", "", paste('<a href="', IUCN_url, '" target="_blank">', as.character(info_species$IUCN_assessment_presence), '</a>', sep = ''))}
      followUpTable$CITES[species_i] <- info_species$CITES
      followUpTable$use[species_i] <- info_species$url
      followUpTable$url[species_i] <- info_species$url
      followUpTable$Validacao.Registro.REDBOOK2013[species_i] <- if(is.na(info_species$CategoriaAvaliacao1_CNCFlora)){NA} else{paste('<a href="http://www.cncflora.jbrj.gov.br/portal/pt-br/profile/', SPECIES, '/" target="_blank">', info_species$CategoriaAvaliacao1_CNCFlora, '</a>', sep = '')}
      followUpTable <- followUpTable %>% replace(., is.na(.), "")

      followUpTable$FB2020_endemism <- sub("NO", "Não", followUpTable$FB2020_endemism)
      followUpTable$FB2020_endemism <- sub("YES", "Sim", followUpTable$FB2020_endemism)

      if(followUpTable$FB2020_endemism[species_i]==""){

        cat(paste("FB2020_endemism VAZIO:", followUpTable$NameFB_semAutor[species_i]), "\n")

      }

      if(followUpTable$occurrenceRemarks[species_i]==""){

        cat(paste("occurrenceRemarks VAZIO:", followUpTable$NameFB_semAutor[species_i]), "\n")

      }

      if(followUpTable$location[species_i]==""){

        cat(paste("location VAZIO:", followUpTable$NameFB_semAutor[species_i]), "\n")

      }

      if(followUpTable$lifeForm[species_i]==""){

        cat(paste("lifeForm VAZIO:", followUpTable$NameFB_semAutor[species_i]), "\n")

      }

      if(followUpTable$vegetationType[species_i]==""){

        cat(paste("vegetationType VAZIO:", followUpTable$NameFB_semAutor[species_i]), "\n")

      }

      if(followUpTable$habitat[species_i]==""){

        cat(paste("habitat VAZIO:", followUpTable$NameFB_semAutor[species_i]), "\n")

      }

    }

    if(length(followUpTable$NameFB_semAutor[species_i]) > 1){

      cat(paste("DUPLICADA:", followUpTable$NameFB_semAutor[species_i][1]), "\n")
      delete_species_from_followUpTable <-
        grep(SPECIES, followUpTable$NameFB_semAutor)[2]
      followUpTable <- followUpTable[-delete_species_from_followUpTable,]

    }

  }


  write.csv2(

    followUpTable,
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/follow-up_table/follow-up_table.csv"

      ),
    row.names = FALSE

  )

}
