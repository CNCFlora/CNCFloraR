get_profileIDs_from_oldSystem <- function(){

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
    "/CNCFlora_data/inputs/listOfSpecies_for_processing/fill_profiles_in_oldSystem.csv"

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
      "/CNCFlora_data/outputs/AHK_scripts/get_profileIDs.ahk"

    )

  )

  cat("F4::\n")
  cat("run notepad.exe\n\n")
  cat("Sleep 2000\n")
  for(i in listOfSpecies_n){
    i_URL<-listOfSpecies$V2[i]
    i_URL<-sub("\\s","\\`\\%20", i_URL)
    recorte<-listOfSpecies$V1[i]
    cat("run opera.exe -new http://cncflora.jbrj.gov.br/occurrences/")
    cat(recorte)
    cat("/specie/")
    cat(i_URL)
    cat("\n")
    cat("Sleep 7000\n")
    cat("Click, 241, 408\n")
    cat("Sleep 2000\n")
    cat("Click, 241, 428\n")
    cat("Sleep 4000\n")
    cat("Click, 413, 49\n")
    cat("Sleep 2000\n")
    cat("Send ^c\n")
    cat("Sleep 1000\n")
    cat("Send ^w\n")
    cat("Sleep 1000\n")
    cat("Send !{Tab}\n")
    cat("Sleep 3000\n")
    cat("Send {Text}")
    cat(listOfSpecies$V2[i])
    cat(";\n")
    cat("Sleep 1000\n")
    cat("Send ^v\n")
    cat("Sleep 1000\n")
    cat("Send {Enter}\n")
    cat("Send !{Tab}\n\n")
  }

  cat("Sleep 1000\n")
  cat("Send !{Tab}\n")
  cat("Sleep 1000\n")
  cat("Send ^s\n")
  cat("Sleep 3000\n")
  cat("Send {Text}")
  cat(

    gsub(

      "\\/",
      "\u005c\u005c",
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/profileIDs_in_oldSystem/profileIDs.txt"

      )

    )

  )
  cat("\n")
  cat("Sleep 3000\n")
  cat("Send {Enter}\n")
  cat("return\n")

  sink()
  #Done

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
