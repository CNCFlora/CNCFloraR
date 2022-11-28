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
      "/CNCFlora_data/inputs/profileIDs_in_oldSystem/profileIDs.txt"

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

  reference2 <- str_extract(data$reference, "\\.\\s\\s.*")
  reference2 <- substr(reference2, 4, nchar(reference2))
  data$reference2 <- reference2
  data$reference <- sub("\\.\\s\\s.*", ".", data$reference)

  listOfSpecies_n <- 1:length(listOfSpecies$species)

  for(i in listOfSpecies_n){

    recorte <- listOfSpecies$recorte[i]
    Species <- listOfSpecies$species[i]
    profile_sp <- listOfSpecies$profileID[i]

    data_species <- data %>% dplyr::filter(species == Species)

    data_species$action <- gsub("1.1 Site/area protection","2", data_species$action)
    data_species$action <- gsub("5.1.2 National level","27", data_species$action)
    data_species$action <- gsub("5.1.3 Sub-national level","28", data_species$action)
    data_species$situation <- gsub("on going","2", data_species$situation)
    data_species$situation <- gsub("needed","3", data_species$situation)

    how_many_actions <- nrow(data_species)

    sink(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/AHK_scripts/fill_actions-", Species, ".ahk"

      )

    )

    if(how_many_actions == 1){

      cat("F4::\n")
      cat("\n")
      cat("run opera.exe -new http://cncflora.jbrj.gov.br/profiles/")
      cat(recorte)
      cat("/profile/profile:")
      cat(profile_sp)
      cat("/edit\n")
      cat("Sleep 7000\n")
      cat("Click, 180, 619\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 5}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$action[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$situation[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Text}")
      cat(data_species$text[1])
      cat("\n")
      cat("Sleep 1000\n")

      #Checando se tem referência
      if(is.blank(data_species$reference[1])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[1])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")
          cat("Send {Tab 4}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 7000\n")
          cat("Send ^w\n")
          cat("return")
          sink()

        } else{

          cat("Send {Tab 4}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 7000\n")
          cat("Send ^w\n")
          cat("return")
          sink()

        }

      } else{

        cat("Send {Tab 3}\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("\n")
        cat("return")
        sink()

      }

    }

    if(how_many_actions == 2){

      cat("F4::\n")
      cat("\n")
      cat("run opera.exe -new http://cncflora.jbrj.gov.br/profiles/")
      cat(recorte)
      cat("/profile/profile:")
      cat(profile_sp)
      cat("/edit\n")
      cat("Sleep 7000\n")
      cat("Click, 180, 619\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 5}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$action[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$situation[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Text}")
      cat(data_species$text[1])
      cat("\n")
      cat("Sleep 1000\n")
      #Checando se tem referência
      if(is.blank(data_species$reference[1])==F){
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[1])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[2])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[2])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()

      } else{

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()

      }

    }

    if(how_many_actions == 3){

      cat("F4::\n")
      cat("\n")
      cat("run opera.exe -new http://cncflora.jbrj.gov.br/profiles/")
      cat(recorte)
      cat("/profile/profile:")
      cat(profile_sp)
      cat("/edit\n")
      cat("Sleep 5000\n")
      cat("Click, 180, 619\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 5}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$action[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$situation[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Text}")
      cat(data_species$text[1])
      cat("\n")
      cat("Sleep 1000\n")

      #Checando se tem referência
      if(is.blank(data_species$reference[1])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[1])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[2])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[2])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[3])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[3])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[3])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[3])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()

      } else{

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()
      }
    }

    if(how_many_actions == 4){

      cat("F4::\n")
      cat("\n")
      cat("run opera.exe -new http://cncflora.jbrj.gov.br/profiles/")
      cat(recorte)
      cat("/profile/profile:")
      cat(profile_sp)
      cat("/edit\n")
      cat("Sleep 7000\n")
      cat("Click, 180, 619\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 5}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$action[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$situation[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Text}")
      cat(data_species$text[1])
      cat("\n")
      cat("Sleep 1000\n")

      #Checando se tem referência
      if(is.blank(data_species$reference[1])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[1])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[2])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[2])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[3])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[3])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[3])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[3])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[4])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[4])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[4])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[4])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()

      } else{

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()

      }

    }

    if(how_many_actions == 5){

      cat("F4::\n")
      cat("\n")
      cat("run opera.exe -new http://cncflora.jbrj.gov.br/profiles/")
      cat(recorte)
      cat("/profile/profile:")
      cat(profile_sp)
      cat("/edit\n")
      cat("Sleep 7000\n")
      cat("Click, 180, 619\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 5}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$action[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$situation[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Text}")
      cat(data_species$text[1])
      cat("\n")
      cat("Sleep 1000\n")

      #Checando se tem referência
      if(is.blank(data_species$reference[1])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[1])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[2])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[2])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[3])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[3])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[3])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[3])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[4])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[4])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[4])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[4])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[5])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[5])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[5])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[5])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[5])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[5])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[5])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[5])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()

      } else{

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()

      }

    }

    if(how_many_actions == 6){

      cat("F4::\n")
      cat("\n")
      cat("run opera.exe -new http://cncflora.jbrj.gov.br/profiles/")
      cat(recorte)
      cat("/profile/profile:")
      cat(profile_sp)
      cat("/edit\n")
      cat("Sleep 7000\n")
      cat("Click, 180, 619\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 5}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$action[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(data_species$situation[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Text}")
      cat(data_species$text[1])
      cat("\n")
      cat("Sleep 1000\n")

      #Checando se tem referência
      if(is.blank(data_species$reference[1])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[1])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[2])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[2])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[2])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[2])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[3])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[3])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[3])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[3])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[3])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[4])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[4])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[4])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[4])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[4])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[5])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[5])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[5])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[5])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[5])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[5])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[5])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[5])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[6])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[6])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[6])
        cat("\n")
        cat("Sleep 1000\n")

      } else{

        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send +{Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$action[6])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Down ")
        cat(data_species$situation[6])
        cat("}\n")
        cat("Sleep 1000\n")
        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$text[6])
        cat("\n")
        cat("Sleep 1000\n")

      }

      if(is.blank(data_species$reference[6])==F){

        cat("Send {Tab}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(data_species$reference[6])
        cat("\n")
        cat("Sleep 1000\n")

        #Checando se tem 2 referências
        if(is.blank(data_species$reference2[1])==F){

          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(data_species$reference2[1])
          cat("\n")
          cat("Sleep 1000\n")

        }

        cat("Send {Tab 4}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()

      } else{

        cat("Send {Tab 3}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 7000\n")
        cat("Send ^w\n")
        cat("return")
        sink()

      }

    }

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
    Script$text <- sub("return","Sleep 5000\n", Script$text)
    Script$text <- sub("F4\\:\\:","", Script$text)
    Script$text <- sub("%","`%", Script$text)

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
