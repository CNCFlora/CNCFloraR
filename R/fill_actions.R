fill_actions <- function(){

  library(readtext)
  library(ragtop)
  library(dplyr)
  library(magicfor)
  library(stringr)

  # List of Species file (fill_profiles_in_oldSystem.csv) ####

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

  profileIDs <- fread(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/profileIDs_in_oldSystem/profileIDs.csv"

    ),
    header = F

  )

  profileIDs$V2 <- gsub("\\/edit","", profileIDs$V2)
  profileIDs$V2 <- gsub("http.*profile:","", profileIDs$V2)

  colnames(profileIDs) <- c("V2", "profileID")

  listOfSpecies <- left_join(listOfSpecies, profileIDs, by = "V2")

  colnames(listOfSpecies) <- c("recorte", "species", "flow", "profileID")


  actionsTable <- fread(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/oldSystem_fill_tables/actions_table.csv"

    ),
    header = T,
    sep = ";",
    encoding = "UTF-8"

  )

  data <- actionsTable %>% filter(species %in% listOfSpecies$species)

  listOfSpecies_n <- 1:length(listOfSpecies$species)

  for(i in listOfSpecies_n){

    recorte <- listOfSpecies$recorte[i]
    Species <- listOfSpecies$species[i]
    profile_sp <- listOfSpecies$profileID[i]

    data_species <- data %>% dplyr::filter(species==Species)




  }

  #Renderizando o output final (All)

  sink(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/outputs/AHK_scripts/fill_actions.ahk"

    )

  )
  cat("F4::")
  cat("\n")

  for(i in listOfSpecies$species){

    Script <- readtext(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/AHK_scripts/fill_actions-", i, ".ahk"

      )

    )
    Script$text<-sub("return","Sleep 5000\n",Script$text)
    Script$text<-sub("F4\\:\\:","",Script$text)

    cat(Script$text)
    cat("\n")

    file.remove(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/AHK_scripts/fill_actions-", i, ".ahk"

      )

    )

  }

  cat("\nreturn")
  sink()

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
