prepare_listOfSpecies_files_to_getOccurrences <- function(ask_to_open_file = T, ask_to_write_file = T){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  if(ask_to_open_file == T){

    df <- check_all_files_of_species()

  }

  if(ask_to_open_file == F){

    df <- check_all_files_of_species(ask_to_open_file = F)

  }

  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }

  listOfSpecies <- colnames(df[which(df["occurrenceRecords",] == FALSE)])



  # Load follow-up table from GoogleSheets ####

  ss <- gs4_get(ss_followUpTable_URL)
  followUpTable <- read_sheet(ss, sheet = which(ss$sheets$name == "List_for_HTML_profile"))

  followUpTable.filtered <- followUpTable %>% dplyr::filter(Espécie %in% listOfSpecies)

  output <- data.frame(

    flow = followUpTable.filtered$`PA/PNA`,
    recorte = followUpTable.filtered$Recorte,
    species = followUpTable.filtered$Espécie

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


  # Ask to write the `get_occurrenceRecords.csv` file

  if(ask_to_write_file == T){

    answer <- ""

    while(

      answer != "Y" |
      answer != "N"

    ){

      answer <-
        toupper(readline("Write the list of species file (get_occurrenceRecords.csv)? (y/n): "))

      if(answer == "Y"){

        write.table(

          output,
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/get_occurrenceRecords.csv"

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

  }

  return(output[, 3])

  #Done

}
