crop_MapBiomas_by_EOO_create_scripts <- function(){

  # Load package ####

  library(data.table)
  library(readtext)

  # Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <-
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_crop_MapBiomas_by_EOO.csv"

    )

  ## Ask to open the list of species file ####

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

  # Import the list of species file from local path ####

  message("Importing the list of species file...")

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = F,
    sep = ";",
    encoding = "UTF-8"

  )

  message("List of species file imported.")

  suppressWarnings(

    script <-
      readtext(

        paste0(

          getwd(),
          "/Packages/CNCFloraR/R/crop_MapBiomas_by_EOO.txt"

        )

      )

  )


  for(element in listOfSpecies$V1){

    script_for_element <- sub("ESPECIEselect", element, script$text)
    sink(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/shapefiles scripts/EOO_MapBiomas/",
        element,
        " - script.R"

      )

    )

    cat(script_for_element)

    sink()

  }

  message("Scripts created.")

}
