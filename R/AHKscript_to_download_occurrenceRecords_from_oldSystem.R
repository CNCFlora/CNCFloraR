#' AHK script to download occurrence records from old system
#'
#' AHK script para baixar os registros de ocorrências do antigo sistema


AHKscript_to_download_occurrenceRecords_from_oldSystem <- function(){

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(data.table)

      })
    })
  })

  # List of Species file (get_occurrenceRecords.csv) ####

  ## Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/listOfSpecies_for_processing/get_occurrenceRecords.csv"

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

  ## Import the list of species file from local path ####

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = F,
    sep = ";",
    encoding = "UTF-8"

  )



  listOfSpecies_n <- 1:nrow(listOfSpecies)

  message("Writing the script...")

  sink(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/outputs/AHK_scripts/get_occurrences.ahk"

      )

  )

  cat("F4::\n")
  cat("\n")
  for(i in listOfSpecies_n){
    especie <- listOfSpecies$V3[i]
    recorte <- listOfSpecies$V2[i]
    flow <- listOfSpecies$V1[i]
    i_URL <- especie
    i_URL<-sub("\\s","\\`\\%20", i_URL)

    cat("run iexplore.exe -new http://cncflora.jbrj.gov.br/occurrences/")
    cat(recorte)
    cat("/specie/")
    cat(i_URL)
    cat("\n")

    if(flow == "PNA"){
      cat("Sleep 70000\n")
    }

    if(flow == "PA"){
      cat("Sleep 20000\n")
    }

    cat("Send ^s\n")

    cat("Sleep 4000\n")
    cat("Send {Text}")
    cat(especie)
    cat("\n")
    cat("Sleep 2000\n")
    cat("Send {Tab}\n")
    cat("Send {Down 2}\n")
    cat("Send +{Tab}\n")
    cat("Sleep 1000\n")
    cat("Send {End}\n")
    cat("Sleep 1000\n")
    cat("Send {Text}.html\n")
    cat("Sleep 1000\n")
    cat("Send {Enter}\n")
    cat("Sleep 7000\n")
    cat("Send ^w\n")
    cat("\n")
  }

  cat("return")

  sink()

  message("Script done!")

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

