prepare_listOfSpecies_files_for_all_methods <- function(){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species()

  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }

  listOfSpecies <- colnames(df)



  # Load follow-up table from GoogleSheets ####

  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")

  followUpTable <- read_sheet(ss, sheet = 6)

  followUpTable2 <- read_sheet(ss, sheet = 1)


  followUpTable.filtered <- followUpTable %>% dplyr::filter(Espécie %in% listOfSpecies)

  if(TRUE %in% duplicated(followUpTable.filtered$Espécie)){

    followUpTable.filtered <- followUpTable.filtered[-duplicated(followUpTable.filtered$Espécie),]

  }


  followUpTable.filtered2 <- followUpTable2 %>% dplyr::filter(NameFB_semAutor %in% listOfSpecies) %>% select(NameFB_semAutor, FB2020_AcceptedNameUsage)

  if(TRUE %in% duplicated(followUpTable.filtered2$NameFB_semAutor)){

    followUpTable.filtered2 <- followUpTable.filtered2[-duplicated(followUpTable.filtered2$NameFB_semAutor),]

  }


  colnames(followUpTable.filtered2) <- c(

    "Espécie",
    "FB2020_AcceptedNameUsage"

  )

  followUpTable.filtered2 <- left_join(followUpTable.filtered, followUpTable.filtered2, by = "Espécie")

  followUpTable.filtered2 <- followUpTable.filtered2 %>%
    select(Espécie, FB2020_AcceptedNameUsage)


  x <- 0
  repeat{

    x <- x + 1
    followUpTable.filtered2$FB2020_AcceptedNameUsage <-
      sub("\\w*\\s", "", followUpTable.filtered2$FB2020_AcceptedNameUsage)

    if(x == 2) {

      break

    }

  }

  output <- data.frame(

    flow = followUpTable.filtered$`PA/PNA`,
    recorte = followUpTable.filtered$Recorte,
    species = followUpTable.filtered$Espécie,
    author = followUpTable.filtered2$FB2020_AcceptedNameUsage,
    records = followUpTable.filtered$Registros,
    SIG = followUpTable.filtered$SIG

  )

  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme="dark"

    )

  )


  # Ask to write the `get_occurrenceRecords.csv` file ####

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species (get_occurrenceRecords.csv)? (y/n): "))

    if(answer == "Y"){

      ## get_occurrenceRecords.csv ####

      write.table(

        data.frame(

          output$flow,
          output$recorte,
          output$species

        ),
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/get_occurrenceRecords.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/get_occurrenceRecords.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }



  # Ask to write the `species_to_get_from_followUpTable.csv` file ####

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species (species_to_get_from_followUpTable.csv)? (y/n): "))

    if(answer == "Y"){

      # species_to_get_from_followUpTable.csv ####

      write.table(

        output$species,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_to_get_from_followUpTable.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_to_get_from_followUpTable.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }


  # Ask to write the `species_for_obraPrinceps.csv` file ####

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species (species_for_obraPrinceps.csv)? (y/n): "))

    if(answer == "Y"){

      ## species_for_obraPrinceps.csv ####

      output_obraPrinceps <- output
      output_obraPrinceps <- rbind(

        data.frame(

          flow = "",
          recorte = "",
          species = "Genero especie var. variedade",
          author = "Author",
          records = "",
          SIG = ""

        ),
        output_obraPrinceps

      )

      if(TRUE %in% is.na(output_obraPrinceps$author)){

        output_obraPrinceps <- output_obraPrinceps[is.na(output_obraPrinceps$author) == F,]

        message("Species excluded:")

        print(

          colorDF(

            output_obraPrinceps[is.na(output_obraPrinceps$author) == F,],
            theme="dark"

          )

        )

      }

      write.table(

        data.frame(

          species = output_obraPrinceps$species,
          author = output_obraPrinceps$author

        ),
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_obraPrinceps.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_obraPrinceps.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }

  # Ask to write the `species_for_FloraFungaBrasil_citations.csv` file ####

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species (species_for_FloraFungaBrasil_citations.csv)? (y/n): "))

    if(answer == "Y"){

      # species_for_FloraFungaBrasil_citations.csv ####

      write.table(

        output$species,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_FloraFungaBrasil_citations.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_FloraFungaBrasil_citations.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }


  # Ask to write the `validationOccurrences.csv` file ####

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (validationOccurrences.csv)? (y/n): "))

    if(answer == "Y"){

      # validationOccurrences.csv ####

      write.table(

        output$species,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/validationOccurrences.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/validationOccurrences.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }


  # Ask to write the `validationCoordinates.csv` file ####

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (validationCoordinates.csv)? (y/n): "))

    if(answer == "Y"){

      # validationCoordinates.csv ####

      write.table(

        output$species,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/validationCoordinates.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/validationCoordinates.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }


  # Ask to write the `species_intersect_UCs_PANs_TERs.csv` file ####

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species (species_intersect_UCs_PANs_TERs.csv)? (y/n): "))

    if(answer == "Y"){

      ## species_intersect_UCs_PANs_TERs.csv ####

      write.table(

        data.frame(

          output$species,
          output$flow,
          output$records,
          output$SIG

        ),
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_intersect_UCs_PANs_TERs.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_intersect_UCs_PANs_TERs.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }


  # Ask to write the `species_profileOfSpeciesHTML.csv` file ####

  output_profileOfSpeciesHTML <- output

  output_profileOfSpeciesHTML <- output_profileOfSpeciesHTML[output_profileOfSpeciesHTML$records != "#N/A",]

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species (species_profileOfSpeciesHTML.csv)? (y/n): "))

    if(answer == "Y"){

      ## species_profileOfSpeciesHTML.csv ####

      write.table(

        data.frame(

          output_profileOfSpeciesHTML$species,
          output_profileOfSpeciesHTML$flow,
          output_profileOfSpeciesHTML$records

        ),
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_profileOfSpeciesHTML.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_profileOfSpeciesHTML.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }


  # Ask to write the `species_landCover-MapBiomas.csv` file ####

  output_overlayMapBiomas <- output

  output_overlayMapBiomas <- output_overlayMapBiomas[output_overlayMapBiomas$records != "#N/A",]

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species (species_landCover-MapBiomas.csv)? (y/n): "))

    if(answer == "Y"){

      ## species_landCover-MapBiomas.csv ####

      output_overlayMapBiomas <- output_overlayMapBiomas %>%
        filter(flow == "PA")

      write.table(

        data.frame(

          output_overlayMapBiomas$species,
          output_overlayMapBiomas$flow,
          output_overlayMapBiomas$records,
          output_overlayMapBiomas$records

        ),
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_landCover-MapBiomas.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_landCover-MapBiomas.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }


  # Ask to write the `species_landCover-MapBiomasFire.csv` file ####

  output_overlayMapBiomasFire <- output

  output_overlayMapBiomasFire <- output_overlayMapBiomas[output_overlayMapBiomasFire$records != "#N/A",]

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species (species_landCover-MapBiomasFire.csv)? (y/n): "))

    if(answer == "Y"){

      ## species_landCover-MapBiomasFire.csv ####

      output_overlayMapBiomasFire <- output_overlayMapBiomasFire %>%
        filter(flow == "PA")

      write.table(

        data.frame(

          output_overlayMapBiomasFire$species,
          output_overlayMapBiomasFire$flow,
          output_overlayMapBiomasFire$records,
          output_overlayMapBiomasFire$records

        ),
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_landCover-MapBiomasFire.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message(

        paste0(

          "File ",
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_landCover-MapBiomasFire.csv"

          ),
          " created."

        )

      )

      break

    }

    if(answer == "N"){

      break

    }

  }


  #Done

}
