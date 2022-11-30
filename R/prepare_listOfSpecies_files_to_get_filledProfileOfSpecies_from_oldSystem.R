prepare_listOfSpecies_files_to_get_filledProfileOfSpecies_from_oldSystem <- function(onlyNonExistentProfile = F, ask_to_open_file = T, ask_to_write_file = T){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species(ask_to_open_file = F)

  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }

  if(onlyNonExistentProfile == F){

    listOfSpecies <- colnames(df)

  } else {

    if(onlyNonExistentProfile == T){

      listOfSpecies <- colnames(

        df[

          which(

            df["HTMLprofile",] == T &
              df["filledProfile_from_oldSystem",] == F

          )

        ]

      )

    }

  }


  # Load follow-up table from GoogleSheets ####

  Acomp_spp_followUpTable <- get_sheet_Acomp_spp_from_followUpTable_in_cloud()

  Acomp_spp_followUpTable.filtered <- Acomp_spp_followUpTable %>%
    dplyr::filter(`NameFB_semAutor(textPlane)` %in% listOfSpecies) %>%
    dplyr::filter(`Perfil Completo` %in% "x")

  output <- data.frame(

    recorte = Acomp_spp_followUpTable.filtered$Recorte,
    species = Acomp_spp_followUpTable.filtered$`NameFB_semAutor(textPlane)`

  )

  return(output)

  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme="dark"

    )

  )


  # Ask to write the `get_filledProfileOfSpecies.csv` file

  if(ask_to_write_file == T){

    answer <- ""

    while(

      answer != "Y" |
      answer != "N"

    ){

      answer <-
        toupper(readline("Write the list of species file (get_filledProfileOfSpecies.csv)? (y/n): "))

      if(answer == "Y"){

        write.table(

          output,
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/get_filledProfileOfSpecies.csv"

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

  } else {

    write.table(

      output,
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/listOfSpecies_for_processing/get_filledProfileOfSpecies.csv"

      ),
      col.names = F,
      row.names = F,
      sep = ";"

    )

    message("File created.")

  }


  #Done

}
