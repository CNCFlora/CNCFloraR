prepare_listOfSpecies_files_to_intersectPANsTERsUCs <- function(onlyNonExistentFile = F, ask_to_write_file = T){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species(ask_to_open_file = F)


  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }


  if(onlyNonExistentFile == T){

    listOfSpecies <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["intersectPANs",] == F &
            df["intersectTERs",] == F &
            df["intersectUCs",] == F

        )

      ]

    )

  } else{

    if(onlyNonExistentFile == F){

      listOfSpecies <- colnames(

        df[

          which(

            df["occurrenceRecords",] == T

          )

        ]

      )

    }

  }

  # Load follow-up table from GoogleSheets ####

  ss <- gs4_get(ss_followUpTable_URL)
  followUpTable <- read_sheet(ss, sheet = which(ss$sheets$name == "List_for_HTML_profile"))

  followUpTable.filtered <- followUpTable %>% dplyr::filter(Espécie %in% listOfSpecies)

  output <- data.frame(

    species = followUpTable.filtered$Espécie,
    flow = followUpTable.filtered$`PA/PNA`,
    records = followUpTable.filtered$Registros


  )

  output <- output[output$records != "#N/A",]

  if(T %in% duplicated(output)){

    output <- output[-which(duplicated(output)),]

  }

  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme = "dark"

    )

  )


  if(ask_to_write_file == T){

    # Ask to write the `species_intersect_UCs_PANs_TERs.csv` file

    answer <- ""

    while(

      answer != "Y" |
      answer != "N"

    ){

      answer <-
        toupper(readline("Write the list of species file (species_intersect_UCs_PANs_TERs.csv)? (y/n): "))

      if(answer == "Y"){

        write.table(

          output,
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_intersect_UCs_PANs_TERs.csv"

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
        "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_intersect_UCs_PANs_TERs.csv"

      ),
      col.names = F,
      row.names = F,
      sep = ";"

    )

  }


  #Done

}
