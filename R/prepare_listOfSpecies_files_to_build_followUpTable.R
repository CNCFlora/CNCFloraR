prepare_listOfSpecies_files_to_build_followUpTable <- function(){

  library(stringr)
  library(googlesheets4)
  library(colorDF)
  library(data.table)

  df <- check_all_files_of_species()


  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }


  output <- data.frame(Species = colnames(df))

  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme="dark"

    )

  )


  # Ask to write the `species_to_get_from_followUpTable.csv` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (species_to_get_from_followUpTable.csv)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_to_get_from_followUpTable.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message("File created.")

      break

    }

    if(answer == "N"){

      break

    }

  }


  # Ask to write the `species_for_obraPrinceps.csv` file ####

  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")

  followUpTable <- read_sheet(ss, sheet = 1)

  followUpTable.filtered <-
    followUpTable %>%
    dplyr::filter(NameFB_semAutor %in% output$Species) %>%
    dplyr::select(NameFB_semAutor, FB2020_AcceptedNameUsage)

  if(TRUE %in% duplicated(followUpTable.filtered$NameFB_semAutor)){

    followUpTable.filtered <- followUpTable.filtered[-duplicated(followUpTable.filtered$NameFB_semAutor),]

  }

  output_obraPrinceps <- followUpTable.filtered

  output_obraPrinceps$FB2020_AcceptedNameUsage <- sub("\\w+\\s", "", output_obraPrinceps$FB2020_AcceptedNameUsage)
  output_obraPrinceps$FB2020_AcceptedNameUsage <- sub("\\w+\\s", "", output_obraPrinceps$FB2020_AcceptedNameUsage)

  colnames(output_obraPrinceps) <- c("species", "author")

  print(

    colorDF(

      output_obraPrinceps,
      theme="dark"

    )

  )

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species (species_for_obraPrinceps.csv)? (y/n): "))

    if(answer == "Y"){

      ## species_for_obraPrinceps.csv ####

      output_obraPrinceps <- rbind(

        data.frame(

          species = "Genero especie var. variedade",
          author = "Author"
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
        sep = ";",
        fileEncoding = "UTF-8"


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


  # Ask to write the `species_for_FloraFungaBrasil_citations.csv` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (species_for_FloraFungaBrasil_citations.csv)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_FloraFungaBrasil_citations.csv"

        ),
        col.names = F,
        row.names = F,
        sep = ";"

      )

      message("File created.")

      break

    }

    if(answer == "N"){

      break

    }

  }


  #Done

}
