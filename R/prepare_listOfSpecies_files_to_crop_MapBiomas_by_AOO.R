prepare_listOfSpecies_files_to_crop_MapBiomas_by_AOO <- function(onlyNonExistentFiles = F){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species()


  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }

  if(onlyNonExistentFiles == T){

    listOfSpecies <- colnames(

      df[

        which(

          df["shapefileAOO",] == T &
            df["shapefileAOO_MapBiomas",] == F

        )

      ]

    )

  } else {

    listOfSpecies <- colnames(

      df[

        which(

          df["shapefileAOO",] == T

        )

      ]

    )

  }

  output <- data.frame(

    Species = listOfSpecies

  )


  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme = "dark"

    )

  )


  # Ask to write the `species_crop_MapBiomas_by_AOO.csv` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (species_crop_MapBiomas_by_AOO.csv)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_crop_MapBiomas_by_AOO.csv"

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
