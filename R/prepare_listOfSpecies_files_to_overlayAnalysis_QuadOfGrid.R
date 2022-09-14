prepare_listOfSpecies_files_to_overlayAnalysis_QuadOfGrid <- function(onlyNonExistentAnalysis = F){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species()


  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }

  if(onlyNonExistentAnalysis == T){

    listOfSpecies <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["overlayMapBiomasQuadOfGrid",] == F

        )

      ]

    )

  } else {

    if(onlyNonExistentAnalysis == F){

      listOfSpecies <- colnames(

        df[

          which(

            df["occurrenceRecords",] == T

          )

        ]

      )

    }

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


  # Ask to write the `species_landCover-MapBiomas_by_QuadOfGrid.csv` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (species_landCover-MapBiomas_by_QuadOfGrid.csv)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_landCover-MapBiomas_by_QuadOfGrid.csv"

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
