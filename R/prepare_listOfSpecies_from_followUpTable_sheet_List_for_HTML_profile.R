prepare_listOfSpecies_from_followUpTable_sheet_List_for_HTML_profile <- function(onlyNonExistentOverlayAnalysis = F){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  # Load follow-up table from GoogleSheets ####

  followUpTable <- gs4_get(ss_followUpTable_URL)
  followUpTable_sheet_List_for_HTML_profile <- read_sheet(followUpTable, sheet = which(followUpTable$sheets$name == "List_for_HTML_profile"))

  followUpTable_sheet_List_for_HTML_profile.filtered <-
    followUpTable_sheet_List_for_HTML_profile %>%
    dplyr::filter(is.na(as.character(Espécie)) == F)

  output <- data.frame(

    species = followUpTable_sheet_List_for_HTML_profile.filtered$Espécie,
    flow = followUpTable_sheet_List_for_HTML_profile.filtered$`PA/PNA`,
    records = followUpTable_sheet_List_for_HTML_profile.filtered$Registros


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
      theme="dark"

    )

  )


  # Ask to write the `check_all_files_of_species.csv` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (check_all_files_of_species.csv)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/check_all_files_of_species.csv"

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
