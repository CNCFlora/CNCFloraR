create_profileOfSpeciesHTML_execute_scripts <- function(list = "", flow = ""){

  Flow = flow

  # Load package ####

  library(data.table)


  if(list == ""){

    # Get local path of the downloaded list of species file ####

    listOfSpecies_localPath <-
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_profileOfSpeciesHTML.csv"

      )


    # Import the list of species file from local path ####

    message("Importing the list of species file...")

    listOfSpecies <- fread(

      listOfSpecies_localPath,
      header = F,
      sep = ";",
      encoding = "UTF-8"

    )

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


  for(element in listOfSpecies$V1){

    rstudioapi::jobRunScript(

      path = paste0(

        getwd(),
        "/CNCFlora_data/outputs/profileOfSpeciesHTML scripts/",
        element,
        " - script.R"

        ),
      encoding = "UTF-8",
      workingDir = getwd()

    )

  }

}
