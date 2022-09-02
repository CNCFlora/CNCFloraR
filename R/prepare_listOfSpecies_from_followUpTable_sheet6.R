prepare_listOfSpecies_from_followUpTable_sheet6 <- function(onlyNonExistentOverlayAnalysis = F){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  # Load follow-up table from GoogleSheets ####

  followUpTable <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")
  followUpTable_sheet6 <- read_sheet(followUpTable, sheet = 6)

  followUpTable_sheet6.filtered <-
    followUpTable_sheet6 %>%
    dplyr::filter(is.na(as.character(Espécie)) == F) %>%
    dplyr::filter(is.na(as.character(Status)) == T) %>%
    unique()

  output <- data.frame(

    species = followUpTable_sheet6.filtered$Espécie,
    flow = followUpTable_sheet6.filtered$`PA/PNA`,
    records = followUpTable_sheet6.filtered$Registros


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
