trendAnalysis_QuadOfGrid_create_scripts <- function(list = "", ask_to_open_file = T){

  library(data.table)
  library(readtext)


  if(list[1] == ""){

    # Get local path of the downloaded list of species file ####

    listOfSpecies_localPath <-
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_trendAnalysis_QuadOfGrid.csv"

      )


    # Ask to open the list of species file ####

    if(ask_to_open_file == T){

      answer <- ""

      while(answer != "Y" |
            answer != "N" ){

        answer <-
          toupper(readline("Open the list of species file? (y/n): "))

        if(answer == "Y"){

          shell(listOfSpecies_localPath)

          answer2 <- ""

          while(answer2 != "Y"){

            answer2 <-
              toupper(readline("List of species file ready? (y): "))

            if(answer2 == "Y"){

              break

            }

          }

          break

        }

        if(answer == "N"){

          break

        }

      }

    }

    # Import the list of species file from local path ####

    message("Importing the list of species file...")

    listOfSpecies <- fread(

      listOfSpecies_localPath,
      header = F,
      sep = ";",
      encoding = "UTF-8"

    )

    message("List of species file imported.")

  } else {

    listOfSpecies <- data.frame(

      V1 = list

    )

  }


  suppressWarnings(

    script <-
      readtext(

        paste0(

          getwd(),
          "/Packages/CNCFloraR/R/trendAnalysis_QuadOfGrid.txt"

        )

      )

  )


  for(element in listOfSpecies$V1){

    script_for_element <- sub("ESPECIEselect", element, script$text)
    sink(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/trendAnalysis scripts/QuadOfGrid/",
        element,
        " - script.R"

      )

    )

    cat(script_for_element)

    sink()

  }

}
