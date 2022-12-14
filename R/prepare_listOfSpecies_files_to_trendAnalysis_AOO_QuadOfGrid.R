prepare_listOfSpecies_files_to_trendAnalysis_AOO_QuadOfGrid <- function(onlyNonExistentAnalysis = F, ask_to_write_file = T){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species(ask_to_open_file = F)


  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }

  if(onlyNonExistentAnalysis == T){

    listOfSpecies <- colnames(

      df[

        which(

          df["overlayMapBiomasQuadOfGrid",] == T &
            df["trendQuadOfGrid",] == F

        )

      ]

    )

  } else{

    if(onlyNonExistentAnalysis == F){

      listOfSpecies <- colnames(
        df[
          which(

            df["overlayMapBiomasQuadOfGrid",] == T

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


  # Ask to write the `species_trendAnalysis_QuadOfGrid.csv` file

  if(ask_to_write_file == T){

    answer <- ""

    while(

      answer != "Y" |
      answer != "N"

    ){

      answer <-
        toupper(readline("Write the list of species file (species_trendAnalysis_QuadOfGrid.csv)? (y/n): "))

      if(answer == "Y"){

        write.table(

          output,
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_trendAnalysis_QuadOfGrid.csv"

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

  #Done

}
