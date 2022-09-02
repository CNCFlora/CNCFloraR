prepare_listOfSpecies_files_to_validationOccurrences <- function(onlyNonExistentOverlayAnalysis = F){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species()


  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }


  if(onlyNonExistentOverlayAnalysis == T){

    listOfSpecies <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["overlayMapBiomasTodosOsAnos",] == F

        )

      ]

    )

  } else{

    if(onlyNonExistentOverlayAnalysis == F){

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

  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")
  followUpTable <- read_sheet(ss, sheet = 6)

  followUpTable.filtered <- followUpTable %>%
    dplyr::filter(Espécie %in% listOfSpecies & `PA/PNA` == "PA")

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
      theme="dark"

    )

  )


  # Ask to write the `validationOccurrences.csv` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (validationOccurrences.csv)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/validationOccurrences.csv"

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
