create_profileOfSpeciesHTML_create_scripts <- function(list = "", ask_to_open_file = T, flow = ""){

  Flow = flow

  # Load package ####

  library(data.table)
  library(readtext)
  library(googlesheets4)


  if(list[1] == ""){

    # Get local path of the downloaded list of species file ####

    listOfSpecies_localPath <-
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_profileOfSpeciesHTML.csv"

      )

    ## Ask to open the list of species file ####

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

      # Import the list of species file from local path ####

      message("Importing the list of species file...")

      listOfSpecies <- fread(

        listOfSpecies_localPath,
        header = F,
        sep = ";",
        encoding = "UTF-8"

      )

    }

  } else {

    listOfSpecies <- data.frame(

      V1 = list

    )

    ## Get sheet List_for_HTML_profile from the follow-up table in GoogleSheets ####

    cli_h2("Checking and getting data from follow-up table in cloud")

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()

    listOfSpecies <-
      List_for_HTML_profile_followUpTable %>%
      dplyr::filter(Espécie %in% listOfSpecies$V1) %>%
      dplyr::mutate(V1 = Espécie, V2 = `PA/PNA`, V3 = Registros) %>%
      dplyr::select(V1, V2, V3)

  }



  if(Flow == "PNT"){

    listOfSpecies <- listOfSpecies[listOfSpecies$V2 == "PNA",]

  }

  if(Flow == "PT"){

    listOfSpecies <- listOfSpecies[listOfSpecies$V2 == "PA",]

  }

  message("List of species file imported.")

  suppressWarnings(

    script <-
      readtext(

        paste0(

          getwd(),
          "/Packages/CNCFloraR/R/profileOfSpeciesHTML.txt"

        )

      )

  )


  for(element in listOfSpecies$V1){

    script_for_element <- sub("ESPECIEselect", element, script$text)
    sink(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/profileOfSpeciesHTML scripts/",
        element,
        " - script.R"

      )

    )

    cat(script_for_element)

    sink()

  }

  message("Scripts created.")

}
