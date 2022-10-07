intersect_PANs_TERs_UCs_execute_scripts <- function(list = ""){

  library(data.table)

  if(list[1] == ""){

    # Get local path of the downloaded list of species file ####

    listOfSpecies_localPath <-
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_intersect_UCs_PANs_TERs.csv"

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

  }


  message("List of species file imported.")

  for(element in listOfSpecies$V1){

    rstudioapi::jobRunScript(

      path = paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs scripts/",
        element,
        " - script.R"

      ),
      encoding = "UTF-8",
      workingDir = getwd()

    )

  }

}
