prepare_listOfSpecies_from_followUpTable_sheet7 <- function(onlyNonExistentOverlayAnalysis = F){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  # Load follow-up table from GoogleSheets ####

  followUpTable <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=674274536")
  followUpTable_sheet7 <- read_sheet(followUpTable, sheet = 7)

  followUpTable_sheet7.filtered <-
    followUpTable_sheet7 %>%
    dplyr::filter(is.na(as.character(Espécie)) == F) %>%
    dplyr::filter(is.na(as.character(Feito)) == T) %>%
    unique()

  output <- data.frame(

    species = followUpTable_sheet7.filtered$Espécie

  )

  if(T %in% duplicated(output)){

    output <- output[-which(duplicated(output)),]

  }

  output <- data.frame(

    species = output

  )


  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme="dark"

    )

  )


  # Ask to write the `species_to_prepare_assessment.csv` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (species_to_prepare_assessment.csv)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_to_prepare_assessment.csv"

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
