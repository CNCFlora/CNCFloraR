#' Generate the script to produce multiple HTMLs of the panel of species profiles
#'
#' Gera o script para produzir múltiplos painéis dos perfis das espécies

generate_script_of_multiple_profileOfSpeciesHTMLs <- function(){

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(googledrive)
        library(readtext)
        library(data.table)

        options("encoding" = "UTF-8")

      })
    })
  })


  # Download the list of species file from GoogleDrive ####

  with_drive_quiet(
    drive_download(

      drive_get("species_profileOfSpeciesHTML.csv"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/listOfSpecies_for_processing/species_profileOfSpeciesHTML.csv"),
      overwrite = TRUE

    )
  )

  ## Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <-
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_profileOfSpeciesHTML.csv"

      )


  ## Import the list of species file from local path ####

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = F,
    sep = ";",
    encoding = "UTF-8"

  )

  colnames(listOfSpecies) <- c(

    "species",
    "flow",
    "record",
    "SIG"

  )

  # Start of the script ####

  ## Download the file containing the start of script from Google Drive ####

  with_drive_quiet(
    drive_download(

      drive_get("input_profileOfSpecies_script_start.txt"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_start.txt"),
      overwrite = TRUE

    )
  )


  ## Get local path of the file containing the start of script ####

  profileOfSpecies_script_start_localPath <-
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_start.txt"

      )


  ## Import the file containing the start of script from local path ####

  input_script_start <- readtext(profileOfSpecies_script_start_localPath)


  # End of the script of Possibly Threatened flow (PA: Possivelmente ameaçada) ####

  ## Download the file containing the end of the script of Possibly Threatened flow from Google Drive ####

  with_drive_quiet(
    drive_download(

      drive_get("input_profileOfSpecies_script_end_PA.txt"),
      path = paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_end_PA.txt"

        ),
      overwrite = TRUE

    )
  )


  ## Get local path of the file containing the end of the script of Possibly Threatened flow ####

  profileOfSpecies_script_end_PA_localPath <-
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_end_PA.txt"

      )


  ## Import the file containing end of the script of Possibly Threatened flow from local path ####

  input_end_PA <- readtext(profileOfSpecies_script_end_PA_localPath)
  input_end_PA$text <- sub("$ï»¿", "", input_end_PA$text)
  input_end_PA$text <- sub("$\\<U\\+FEFF\\>", "", input_end_PA$text)


  # End of the script of Possibly Not Threatened (PNA: Possivelmente Não Ameaçada) ####

  ## Records not validated ####

  ### Download the file containing the end of the script of Possibly Threatened with records not validated flow from Google Drive ####

  with_drive_quiet(
    drive_download(

      drive_get("input_profileOfSpecies_script_end_PNA_recordsNotValidated.txt"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_end_PNA_recordsNotValidated.txt"),
      overwrite = TRUE

    )
  )


  ### Get local path of the file containing the end of the script of Possibly Threatened with records not validated flow ####

  profileOfSpecies_script_end_PA_localPath <- paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_end_PNA_recordsNotValidated.txt")


  ### Import the file containing end of the script of Possibly Threatened with records not validated flow from local path ####

  input_end_PNA_recordsNotValidated <- readtext(profileOfSpecies_script_end_PA_localPath)
  input_end_PNA_recordsNotValidated$text <- sub("$ï»¿", "", input_end_PNA_recordsNotValidated$text)
  input_end_PNA_recordsNotValidated$text <- sub("$\\<U\\+FEFF\\>", "", input_end_PNA_recordsNotValidated$text)


  ## Records validated and SIG revised ####

  ### Download the file containing the end of the script of Possibly Threatened with records validated and SIG revised flow from Google Drive ####

  with_drive_quiet(
    drive_download(

      drive_get("input_profileOfSpecies_script_end_PNA_recordsValidated_SIGrevised.txt"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_end_PNA_recordsValidated_SIGrevised.txt"),
      overwrite = TRUE

    )
  )


  ### Get local path of the file containing the end of the script of Possibly Threatened with records validated and SIG revised flow ####

  profileOfSpecies_script_end_PNA_recordsValidated_SIGrevised_localPath <- paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_end_PNA_recordsValidated_SIGrevised.txt")


  ### Import the file containing end of the script of Possibly Threatened with records validated and SIG revised flow from local path ####

  input_end_PNA_recordsValidated_SIGrevised <- readtext(profileOfSpecies_script_end_PNA_recordsValidated_SIGrevised_localPath)
  input_end_PNA_recordsValidated_SIGrevised$text <- sub("$ï»¿", "", input_end_PNA_recordsValidated_SIGrevised$text)
  input_end_PNA_recordsValidated_SIGrevised$text <- sub("$\\<U\\+FEFF\\>", "", input_end_PNA_recordsValidated_SIGrevised$text)


  ## Records validated and SIG revised ####

  ### Download the file containing the end of the script of Possibly Threatened with records validated and SIG revised flow from Google Drive ####

  with_drive_quiet(
    drive_download(

      drive_get("input_profileOfSpecies_script_end_PNA_recordsValidated_SIGrevised.txt"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_end_PNA_recordsValidated_SIGrevised.txt"),
      overwrite = TRUE

    )
  )


  ### Get local path of the file containing the end of the script of Possibly Threatened with records validated and SIG not revised flow ####

  profileOfSpecies_script_end_PNA_recordsValidated_SIGNotRevised_localPath <- paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/scripts/profileOfSpecies/input_profileOfSpecies_script_end_PNA_recordsValidated_SIGNotRevised.txt")


  ### Import the file containing end of the script of Possibly Threatened with records validated and SIG not revised flow from local path ####

  input_end_PNA_recordsValidated_SIGNotRevised <- readtext(profileOfSpecies_script_end_PNA_recordsValidated_SIGNotRevised_localPath)
  input_end_PNA_recordsValidated_SIGNotRevised$text <- sub("ï»¿", "", input_end_PNA_recordsValidated_SIGNotRevised$text)

  # Get the number of species ####

  species_n <- nrow(listOfSpecies)


  # How many files of scripts ####

  filesOfScripts_n <- round(species_n / 30)


  # Start loop ####

  for(fileOfScripts_n in 1:filesOfScripts_n){

    ## Produce a script to generate multiple HTMLs of the profile of species  ####

    if(fileOfScripts_n == 1){

      sink(

        paste0(

          paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/temp_scripts/"),
          "script_for_multiple_profileOfSpeciesHTML_",
          fileOfScripts_n,
          ".R"

        )

      )

      cat("library(readtext)\n")
      cat("library(data.table)\n")
      cat("library(dplyr)\n")
      cat("library(tidyr)\n")
      cat("library(tidyverse)\n")
      cat("library(stringi)\n")
      cat("library(stringr)\n")
      cat("library(tibble)\n")
      cat("library(knitr)\n")
      cat("library(kableExtra)\n")
      cat("library(htmlTable)\n")
      cat("library(installr)\n")
      cat("library(purrr)\n")
      cat("library(magicfor)\n")
      cat("library(rvest)\n\n")

      for(i in 1:30){

        if(is.na(listOfSpecies$species[i]) == F){

          for(species in listOfSpecies$species[i]){

            cat(input_script_start$text)
            cat('\n\nESPECIE<-"')
            cat(species)
            cat('"\n')

            if(listOfSpecies$flow[i] == "PA"){

              cat(input_end_PA$text)

            }

            if(listOfSpecies$flow[i] == "PNA"){

              # Occurrence records validated?

              if(listOfSpecies$record[i] == "x"){

                # SIG revised?

                if(listOfSpecies$SIG[i] == "x"){

                  cat(input_end_PNA_recordsValidated_SIGrevised$text)

                } else {

                  cat(input_end_PNA_recordsValidated_SIGNotRevised$text)

                }

              } else {

                cat(input_end_PNA_recordsNotValidated$text)

              }

            }

            cat('\n')
            cat('\n')
            cat('\n')

          }

        }

      }

      sink()

    } else {

      sink(

        paste0(

          paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/temp_scripts/"),
          "script_for_multiple_profileOfSpeciesHTML_",
          fileOfScripts_n,
          ".R"

        )

      )

      cat("library(readtext)\n")
      cat("library(data.table)\n")
      cat("library(dplyr)\n")
      cat("library(tidyr)\n")
      cat("library(tidyverse)\n")
      cat("library(stringi)\n")
      cat("library(stringr)\n")
      cat("library(tibble)\n")
      cat("library(knitr)\n")
      cat("library(kableExtra)\n")
      cat("library(htmlTable)\n")
      cat("library(installr)\n")
      cat("library(purrr)\n")
      cat("library(magicfor)\n")
      cat("library(rvest)\n\n")

      for(i in ((fileOfScripts_n * 30) - 29):(fileOfScripts_n * 30)){

        if(is.na(listOfSpecies$species[i]) == F){

          for(species in listOfSpecies$species[i]){

            cat(input_script_start$text)
            cat('\n\nESPECIE<-"')
            cat(species)
            cat('"\n')

            if(listOfSpecies$flow[i] == "PA"){

              cat(input_end_PA$text)

            }

            if(listOfSpecies$flow[i] == "PNA"){

              # Occurrence records validated?

              if(listOfSpecies$record[i] == "x"){

                # SIG revised?

                if(listOfSpecies$SIG[i] == "x"){

                  cat(input_end_PNA_recordsValidated_SIGrevised$text)

                } else {

                  cat(input_end_PNA_recordsValidated_SIGNotRevised$text)

                }

              } else {

                cat(input_end_PNA_recordsNotValidated$text)

              }

            }

            cat('\n')
            cat('\n')
            cat('\n')

          }

        }

      }

      sink()

    }

  }

  if(filesOfScripts_n > 1){

    message(paste0(filesOfScripts_n, " script files generated!"))
    message(

      paste0("Files at: ", sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/scripts/profileOfSpecies")

    )

  } else {

    message("1 script file generated!")
    message(

      paste0("Files at: ", sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/scripts/profileOfSpecies")

    )

  }

}
