prepare_listOfSpecies_files_to_fill_profiles_in_oldSystem <- function(list = "", ask_to_write_file = T){

  library(stringr)
  library(googlesheets4)
  library(colorDF)


  if(list[1] == ""){

    df <- check_all_files_of_species()


    for(i in 1:length(df)){

      df[,i] <- str_detect(df[,i], "TRUE")

    }

    listOfSpecies <- colnames(

      df[

        which(

          df["HTMLprofile",] == T

        )

      ]

    )

  } else {

    listOfSpecies <- list

  }


  # Load follow-up table from GoogleSheets ####

  ss <- gs4_get(ss_followUpTable_URL)
  followUpTable <- read_sheet(ss, sheet = 1)

  followUpTable.filtered <-
    followUpTable %>%
    dplyr::filter(

      is.na(`Preenc. Autom.`) == T &
        NameFB_semAutor %in% listOfSpecies

    ) %>%
    dplyr::select(Recorte, NameFB_semAutor, `PA/PNA`)

  output <- data.frame(

    recorte = followUpTable.filtered$Recorte,
    species = followUpTable.filtered$NameFB_semAutor,
    flow = unlist(followUpTable.filtered$`PA/PNA`)

  )

  if(T %in% duplicated(output)){

    output <- output[-which(duplicated(output)),]

  }


  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme="dark"

    )

  )


  if(ask_to_write_file == T){

    # Ask to write the `fill_profiles_in_oldSystem.csv` file

    answer <- ""

    while(

      answer != "Y" |
      answer != "N"

    ){

      answer <-
        toupper(readline("Write the list of species file (fill_profiles_in_oldSystem.csv)? (y/n): "))

      if(answer == "Y"){

        write.table(

          output,
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/fill_profiles_in_oldSystem.csv"

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
        "/CNCFlora_data/inputs/listOfSpecies_for_processing/fill_profiles_in_oldSystem.csv"

      ),
      col.names = F,
      row.names = F,
      sep = ";"

    )

  }

  #Done

}
