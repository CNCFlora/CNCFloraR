prepare_listOfSpecies_files_to_get_filledProfileOfSpecies_from_oldSystem <- function(onlyNonExistentProfile = F){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species()

  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }

  if(onlyNonExistentProfile == F){

    listOfSpecies <- colnames(df[which(df["filledProfile_from_oldSystem",] == F)])

  } else {

    if(onlyNonExistentProfile == T){

      listOfSpecies <- colnames(df[which(df["filledProfile_from_oldSystem",] == T)])

    }

  }


  # Load follow-up table from GoogleSheets ####

  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")
  followUpTable <- read_sheet(ss, sheet = 6)

  followUpTable.filtered <- followUpTable %>% dplyr::filter(Espécie %in% listOfSpecies)

  output <- data.frame(

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


  # Ask to write the `get_filledProfileOfSpecies.csv` file

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

  #Done

}
