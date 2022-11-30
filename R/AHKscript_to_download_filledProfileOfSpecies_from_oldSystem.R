#' AHK script to download filled profile of species from old system
#'
#' AHK script para baixar os perfis das espécies preenchidos no antigo sistema


AHKscript_to_download_filledProfileOfSpecies_from_oldSystem <- function(

  list = "",
  ask_to_open_file = T,
  ask_to_open_filePath = T

){

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(data.table)

      })
    })
  })

  # List of Species file (get_filledProfileOfSpecies.csv) ####

  if(list[1] == ""){

    ## Get local path of the downloaded list of species file ####

    listOfSpecies_localPath <- paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/get_filledProfileOfSpecies.csv"

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

    }

    ## Import the list of species file from local path ####

    listOfSpecies <- fread(

      listOfSpecies_localPath,
      header = F,
      sep = ";",
      encoding = "UTF-8"

    )

  } else {

    listOfSpecies <- list

    # Load follow-up table from GoogleSheets ####

    ss <- gs4_get(ss_followUpTable_URL)
    followUpTable <- read_sheet(ss, sheet = which(ss$sheets$name == "List_for_HTML_assessment"))

    followUpTable.filtered <- followUpTable %>% dplyr::filter(Espécie %in% listOfSpecies)

    listOfSpecies <- data.frame(

      V1 = followUpTable.filtered$`PA/PNA`,
      V2 = followUpTable.filtered$Recorte,
      V3 = followUpTable.filtered$Espécie

    )

  }



  listOfSpecies_n <- 1:nrow(listOfSpecies)

  message("Writing the script...")

  sink(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/outputs/AHK_scripts/get_filledProfileOfSpecies.ahk"

    )

  )

  cat("F4::\n")
  cat("\n")

  for(i in listOfSpecies_n){

    especie <- listOfSpecies$V3[i]
    recorte <- listOfSpecies$V2[i]
    i_URL <- especie
    i_URL<-sub("\\s","\\`\\%20", i_URL)

    cat("run opera.exe -new http://cncflora.jbrj.gov.br/profiles/")
    cat(recorte)
    cat("/specie/")
    cat(i_URL)
    cat("\n")

    cat("Sleep 2000\n")

    cat("Send ^s\n")

    cat("Sleep 4000\n")
    cat("Send {Text}")
    cat(

      gsub(

        "/",
        "\\\\",
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/speciesProfile_filled/oldSystem/"

        )

      )

    )
    cat(especie)
    cat("\n")
    cat("Sleep 1000\n")
    cat("Send {Enter}\n")
    cat("Sleep 2000\n")
    cat("Send ^w\n")
    cat("\n")

  }

  cat("return")

  sink()

  message("Script done!")


  if(ask_to_open_filePath == T){

    answer <- ""

    while(answer != "Y" |
          answer != "N" ){

      answer <-
        toupper(readline("Open the file path? (y/n)"))

      if(answer == "Y"){

        suppressWarnings(
          shell(

            paste0(

              "explorer.exe ",
              gsub(

                "/", "\\\\",

                paste0(

                  sub(

                    "Packages/CNCFloraR",
                    "",
                    getwd()

                  ),
                  "/CNCFlora_data/outputs/AHK_scripts"

                )

              )

            ),
            intern = TRUE

          )

        )

        break

      }

      if(answer == "N"){

        break

      }

    }

  }

  # Done

}

