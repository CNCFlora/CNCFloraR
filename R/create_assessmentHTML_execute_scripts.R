create_assessmentHTML_execute_scripts <- function(){

  # Load package ####

  library(data.table)

  # Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <-
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_assessment.csv"

    )


  # Import the list of species file from local path ####

  message("Importing the list of species file...")

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = F,
    sep = ";",
    encoding = "UTF-8"

  )

  message("List of species file imported.")


  for(element in listOfSpecies$V1){

    rstudioapi::jobRunScript(

      path = paste0(

        getwd(),
        "/CNCFlora_data/outputs/assessmentHTML scripts/",
        element,
        " - script.R"

        ),
      encoding = "UTF-8",
      workingDir = getwd()

    )

  }

}
