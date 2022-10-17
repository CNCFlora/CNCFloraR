fill_threats <- function(list = "", ask_to_open_file = T){

  library(readtext)
  library(ragtop)
  library(dplyr)
  library(magicfor)
  library(stringr)


  if(list == ""){

    # List of Species file (fill_profiles_in_oldSystem.csv) ####

    ## Get local path of the downloaded list of species file ####

    listOfSpecies_localPath <- paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/fill_profiles_in_oldSystem.csv"

    )

    if(ask_to_open_file == T){

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

    }

    ## Import the list of species file from local path ####

    listOfSpecies <- fread(

      listOfSpecies_localPath,
      header = F,
      sep = ";",
      encoding = "UTF-8"

    )

    listOfSpecies <- listOfSpecies %>%
      dplyr::filter(V3 == "PA")

  } else {

    listOfSpecies <- data.frame(

      V1 = list

    )

  }


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


  threatsTable <- fread(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/oldSystem_fill_tables/threats_table.csv"

    ),
    header = T,
    sep = ";",
    encoding = "UTF-8"

  )

  data <- threatsTable %>% filter(species %in% listOfSpecies$species)

  listOfSpecies_n <- 1:length(listOfSpecies$species)

  for(i in listOfSpecies_n){

    recorte <- listOfSpecies$recorte[i]
    Species <- listOfSpecies$species[i]
    profile_sp <- listOfSpecies$profileID[i]

    data_species <- data %>% dplyr::filter(species==Species)

    Species_1.1 <- data_species %>% filter(threat=="1.1 Housing & urban areas")
    if(count(Species_1.1) == 0){rm(Species_1.1)} else{

      Ameaca_cod <- "2"
      Estresse <- "2"
      if(is.na(Species_1.1$reference[1])==T){} else{Species_1.1_refs_1 <- t(data.frame(strsplit(Species_1.1$reference[1],"  ")))}
      if(is.na(Species_1.1$reference[2])==T){} else{Species_1.1_refs_2 <- t(data.frame(strsplit(Species_1.1$reference[2],"  ")))}
      if(is.na(Species_1.1$reference[3])==T){} else{Species_1.1_refs_3 <- t(data.frame(strsplit(Species_1.1$reference[3],"  ")))}
      if(is.na(Species_1.1$reference[4])==T){} else{Species_1.1_refs_4 <- t(data.frame(strsplit(Species_1.1$reference[4],"  ")))}
      if(is.na(Species_1.1$reference[5])==T){} else{Species_1.1_refs_5 <- t(data.frame(strsplit(Species_1.1$reference[5],"  ")))}
      if(is.na(Species_1.1$reference[6])==T){} else{Species_1.1_refs_6 <- t(data.frame(strsplit(Species_1.1$reference[6],"  ")))}
      Species_1.1_refs <- bind_rows(if(exists("Species_1.1_refs_1")==T){as.data.frame(Species_1.1_refs_1)}, if(exists("Species_1.1_refs_2")==T){as.data.frame(Species_1.1_refs_2)}, if(exists("Species_1.1_refs_3")==T){as.data.frame(Species_1.1_refs_3)}, if(exists("Species_1.1_refs_4")==T){as.data.frame(Species_1.1_refs_4)}, if(exists("Species_1.1_refs_5")==T){as.data.frame(Species_1.1_refs_5)}, if(exists("Species_1.1_refs_6")==T){as.data.frame(Species_1.1_refs_6)})
      Species_1.1$text <- gsub("%", "`%", Species_1.1$text)
      Species_1.1 <- data.frame(Species_1.1, Ameaca_cod, Estresse, Species_1.1_refs)
      rownames(Species_1.1) <- NULL
      Species_1.1 <- Species_1.1[,-1]
      how_many_1.1 <- as.numeric(count(Species_1.1))
      how_many_1.1_vect <- 1:how_many_1.1

    }

    Species_2.1.4 <- data_species %>% filter(threat=="2.1.4 Scale Unknown/Unrecorded")
    if(count(Species_2.1.4) == 0){rm(Species_2.1.4)} else{

      Ameaca_cod <- "10"
      Estresse <- "2"
      if(is.na(Species_2.1.4$reference[1])==T){} else{Species_2.1.4_refs_1 <- t(data.frame(strsplit(Species_2.1.4$reference[1],"  ")))}
      if(is.na(Species_2.1.4$reference[2])==T){} else{Species_2.1.4_refs_2 <- t(data.frame(strsplit(Species_2.1.4$reference[2],"  ")))}
      if(is.na(Species_2.1.4$reference[3])==T){} else{Species_2.1.4_refs_3 <- t(data.frame(strsplit(Species_2.1.4$reference[3],"  ")))}
      if(is.na(Species_2.1.4$reference[4])==T){} else{Species_2.1.4_refs_4 <- t(data.frame(strsplit(Species_2.1.4$reference[4],"  ")))}
      if(is.na(Species_2.1.4$reference[5])==T){} else{Species_2.1.4_refs_5 <- t(data.frame(strsplit(Species_2.1.4$reference[5],"  ")))}
      if(is.na(Species_2.1.4$reference[6])==T){} else{Species_2.1.4_refs_6 <- t(data.frame(strsplit(Species_2.1.4$reference[6],"  ")))}
      Species_2.1.4_refs <- bind_rows(if(exists("Species_2.1.4_refs_1")==T){as.data.frame(Species_2.1.4_refs_1)}, if(exists("Species_2.1.4_refs_2")==T){as.data.frame(Species_2.1.4_refs_2)}, if(exists("Species_2.1.4_refs_3")==T){as.data.frame(Species_2.1.4_refs_3)}, if(exists("Species_2.1.4_refs_4")==T){as.data.frame(Species_2.1.4_refs_4)}, if(exists("Species_2.1.4_refs_5")==T){as.data.frame(Species_2.1.4_refs_5)}, if(exists("Species_2.1.4_refs_6")==T){as.data.frame(Species_2.1.4_refs_6)})
      Species_2.1.4$text <- gsub("%", "`%", Species_2.1.4$text)
      Species_2.1.4 <- data.frame(Species_2.1.4, Ameaca_cod, Estresse, Species_2.1.4_refs)
      rownames(Species_2.1.4) <- NULL
      Species_2.1.4 <- Species_2.1.4[,-1]
      how_many_2.1.4 <- as.numeric(count(Species_2.1.4))
      how_many_2.1.4_vect <- 1:how_many_2.1.4

    }

    Species_2.2.3 <- data_species %>% filter(threat=="2.2.3 Scale Unknown/Unrecorded")
    if(count(Species_2.2.3) == 0){rm(Species_2.2.3)} else{

      Ameaca_cod <- "14"
      Estresse <- "2"
      if(is.na(Species_2.2.3$reference[1])==T){} else{Species_2.2.3_refs_1 <- t(data.frame(strsplit(Species_2.2.3$reference[1],"  ")))}
      if(is.na(Species_2.2.3$reference[2])==T){} else{Species_2.2.3_refs_2 <- t(data.frame(strsplit(Species_2.2.3$reference[2],"  ")))}
      if(is.na(Species_2.2.3$reference[3])==T){} else{Species_2.2.3_refs_3 <- t(data.frame(strsplit(Species_2.2.3$reference[3],"  ")))}
      if(is.na(Species_2.2.3$reference[4])==T){} else{Species_2.2.3_refs_4 <- t(data.frame(strsplit(Species_2.2.3$reference[4],"  ")))}
      if(is.na(Species_2.2.3$reference[5])==T){} else{Species_2.2.3_refs_5 <- t(data.frame(strsplit(Species_2.2.3$reference[5],"  ")))}
      if(is.na(Species_2.2.3$reference[6])==T){} else{Species_2.2.3_refs_6 <- t(data.frame(strsplit(Species_2.2.3$reference[6],"  ")))}
      Species_2.2.3_refs <- bind_rows(if(exists("Species_2.2.3_refs_1")==T){as.data.frame(Species_2.2.3_refs_1)}, if(exists("Species_2.2.3_refs_2")==T){as.data.frame(Species_2.2.3_refs_2)}, if(exists("Species_2.2.3_refs_3")==T){as.data.frame(Species_2.2.3_refs_3)}, if(exists("Species_2.2.3_refs_4")==T){as.data.frame(Species_2.2.3_refs_4)}, if(exists("Species_2.2.3_refs_5")==T){as.data.frame(Species_2.2.3_refs_5)}, if(exists("Species_2.2.3_refs_6")==T){as.data.frame(Species_2.2.3_refs_6)})
      Species_2.2.3$text <- gsub("%", "`%", Species_2.2.3$text)
      Species_2.2.3 <- data.frame(Species_2.2.3, Ameaca_cod, Estresse, Species_2.2.3_refs)
      rownames(Species_2.2.3) <- NULL
      Species_2.2.3 <- Species_2.2.3[,-1]
      how_many_2.2.3 <- as.numeric(count(Species_2.2.3))
      how_many_2.2.3_vect <- 1:how_many_2.2.3

    }

    Species_2.3.4 <- data_species %>% filter(threat=="2.3.4 Scale Unknown/Unrecorded")
    if(count(Species_2.3.4) == 0){rm(Species_2.3.4)} else{

      Ameaca_cod <- "19"
      Estresse <- "2"
      if(is.na(Species_2.3.4$reference[1])==T){} else{Species_2.3.4_refs_1 <- t(data.frame(strsplit(Species_2.3.4$reference[1],"  ")))}
      if(is.na(Species_2.3.4$reference[2])==T){} else{Species_2.3.4_refs_2 <- t(data.frame(strsplit(Species_2.3.4$reference[2],"  ")))}
      if(is.na(Species_2.3.4$reference[3])==T){} else{Species_2.3.4_refs_3 <- t(data.frame(strsplit(Species_2.3.4$reference[3],"  ")))}
      if(is.na(Species_2.3.4$reference[4])==T){} else{Species_2.3.4_refs_4 <- t(data.frame(strsplit(Species_2.3.4$reference[4],"  ")))}
      if(is.na(Species_2.3.4$reference[5])==T){} else{Species_2.3.4_refs_5 <- t(data.frame(strsplit(Species_2.3.4$reference[5],"  ")))}
      if(is.na(Species_2.3.4$reference[6])==T){} else{Species_2.3.4_refs_6 <- t(data.frame(strsplit(Species_2.3.4$reference[6],"  ")))}
      Species_2.3.4_refs <- bind_rows(if(exists("Species_2.3.4_refs_1")==T){as.data.frame(Species_2.3.4_refs_1)}, if(exists("Species_2.3.4_refs_2")==T){as.data.frame(Species_2.3.4_refs_2)}, if(exists("Species_2.3.4_refs_3")==T){as.data.frame(Species_2.3.4_refs_3)}, if(exists("Species_2.3.4_refs_4")==T){as.data.frame(Species_2.3.4_refs_4)}, if(exists("Species_2.3.4_refs_5")==T){as.data.frame(Species_2.3.4_refs_5)}, if(exists("Species_2.3.4_refs_6")==T){as.data.frame(Species_2.3.4_refs_6)})
      Species_2.3.4$text <- gsub("%", "`%", Species_2.3.4$text)
      Species_2.3.4 <- data.frame(Species_2.3.4, Ameaca_cod, Estresse, Species_2.3.4_refs)
      rownames(Species_2.3.4) <- NULL
      Species_2.3.4 <- Species_2.3.4[,-1]
      how_many_2.3.4 <- as.numeric(count(Species_2.3.4))
      how_many_2.3.4_vect <- 1:how_many_2.3.4

    }

    Species_3.2 <- data_species %>% filter(threat=="3.2 Mining & quarrying")
    if(count(Species_3.2) == 0){rm(Species_3.2)} else{

      Ameaca_cod <- "26"
      Estresse <- "3"
      if(is.na(Species_3.2$reference[1])==T){} else{Species_3.2_refs_1 <- t(data.frame(strsplit(Species_3.2$reference[1],"  ")))}
      if(is.na(Species_3.2$reference[2])==T){} else{Species_3.2_refs_2 <- t(data.frame(strsplit(Species_3.2$reference[2],"  ")))}
      if(is.na(Species_3.2$reference[3])==T){} else{Species_3.2_refs_3 <- t(data.frame(strsplit(Species_3.2$reference[3],"  ")))}
      if(is.na(Species_3.2$reference[4])==T){} else{Species_3.2_refs_4 <- t(data.frame(strsplit(Species_3.2$reference[4],"  ")))}
      if(is.na(Species_3.2$reference[5])==T){} else{Species_3.2_refs_5 <- t(data.frame(strsplit(Species_3.2$reference[5],"  ")))}
      if(is.na(Species_3.2$reference[6])==T){} else{Species_3.2_refs_6 <- t(data.frame(strsplit(Species_3.2$reference[6],"  ")))}
      Species_3.2_refs <- bind_rows(if(exists("Species_3.2_refs_1")==T){as.data.frame(Species_3.2_refs_1)}, if(exists("Species_3.2_refs_2")==T){as.data.frame(Species_3.2_refs_2)}, if(exists("Species_3.2_refs_3")==T){as.data.frame(Species_3.2_refs_3)}, if(exists("Species_3.2_refs_4")==T){as.data.frame(Species_3.2_refs_4)}, if(exists("Species_3.2_refs_5")==T){as.data.frame(Species_3.2_refs_5)}, if(exists("Species_3.2_refs_6")==T){as.data.frame(Species_3.2_refs_6)})
      Species_3.2$text <- gsub("%", "`%", Species_3.2$text)
      Species_3.2 <- data.frame(Species_3.2, Ameaca_cod, Estresse, Species_3.2_refs)
      rownames(Species_3.2) <- NULL
      Species_3.2 <- Species_3.2[,-1]
      how_many_3.2 <- as.numeric(count(Species_3.2))
      how_many_3.2_vect <- 1:how_many_3.2

    }

    Species_7.1.3 <- data_species %>% filter(threat=="7.1.3 Trend Unknown/Unrecorded")
    if(count(Species_7.1.3) == 0){rm(Species_7.1.3)} else{

      Ameaca_cod <- "65"
      Estresse <- "3"
      if(is.na(Species_7.1.3$reference[1])==T){} else{Species_7.1.3_refs_1 <- t(data.frame(strsplit(Species_7.1.3$reference[1],"  ")))}
      if(is.na(Species_7.1.3$reference[2])==T){} else{Species_7.1.3_refs_2 <- t(data.frame(strsplit(Species_7.1.3$reference[2],"  ")))}
      if(is.na(Species_7.1.3$reference[3])==T){} else{Species_7.1.3_refs_3 <- t(data.frame(strsplit(Species_7.1.3$reference[3],"  ")))}
      if(is.na(Species_7.1.3$reference[4])==T){} else{Species_7.1.3_refs_4 <- t(data.frame(strsplit(Species_7.1.3$reference[4],"  ")))}
      if(is.na(Species_7.1.3$reference[5])==T){} else{Species_7.1.3_refs_5 <- t(data.frame(strsplit(Species_7.1.3$reference[5],"  ")))}
      if(is.na(Species_7.1.3$reference[6])==T){} else{Species_7.1.3_refs_6 <- t(data.frame(strsplit(Species_7.1.3$reference[6],"  ")))}
      Species_7.1.3_refs <- bind_rows(if(exists("Species_7.1.3_refs_1")==T){as.data.frame(Species_7.1.3_refs_1)}, if(exists("Species_7.1.3_refs_2")==T){as.data.frame(Species_7.1.3_refs_2)}, if(exists("Species_7.1.3_refs_3")==T){as.data.frame(Species_7.1.3_refs_3)}, if(exists("Species_7.1.3_refs_4")==T){as.data.frame(Species_7.1.3_refs_4)}, if(exists("Species_7.1.3_refs_5")==T){as.data.frame(Species_7.1.3_refs_5)}, if(exists("Species_7.1.3_refs_6")==T){as.data.frame(Species_7.1.3_refs_6)})
      Species_7.1.3$text <- gsub("%", "`%", Species_7.1.3$text)
      Species_7.1.3 <- data.frame(Species_7.1.3, Ameaca_cod, Estresse, Species_7.1.3_refs)
      rownames(Species_7.1.3) <- NULL
      Species_7.1.3 <- Species_7.1.3[,-1]
      how_many_7.1.3 <- as.numeric(count(Species_7.1.3))
      how_many_7.1.3_vect <- 1:how_many_7.1.3

    }

    #Renderizando script de AHK

    sink(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/AHK_scripts/fill_threats-", Species, ".ahk"

      )

    )
    cat("F4::\n")
    cat("\n")
    cat("run opera.exe -new http://cncflora.jbrj.gov.br/profiles/")
    cat(recorte)
    cat("/profile/profile:")
    cat(profile_sp)
    cat("/edit\n")
    cat("Sleep 3000\n")
    cat("Click, 170, 590\n")
    cat("Sleep 1000\n")
    cat("Send {Tab}\n")
    cat("Sleep 1000\n")
    cat("Send {Enter}\n")
    cat("Sleep 1000\n")
    cat("Send +{Tab 4}\n")
    cat("Sleep 1000\n")

    if(exists("Species_1.1")){
      cat("Send {Down ")
      cat(Species_1.1$Ameaca_cod[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(Species_1.1$Estresse[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter 3}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 6}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      if(how_many_1.1==1){
        cat("Send {Text}")
        cat(Species_1.1$text[1])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_1.1==2){
        cat("Send {Text}")
        cat(Species_1.1$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[2])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_1.1==3){
        cat("Send {Text}")
        cat(Species_1.1$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[3])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_1.1==4){
        cat("Send {Text}")
        cat(Species_1.1$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[4])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_1.1==5){
        cat("Send {Text}")
        cat(Species_1.1$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[5])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_1.1==6){
        cat("Send {Text}")
        cat(Species_1.1$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[5])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$text[6])
        cat("\n")
        cat("Sleep 1000\n")
      }
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      #Referências
      if(1 %in% how_many_1.1_vect==T){
        cat("Send {Text}")
        cat(Species_1.1$V1[1])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_1.1$V2[1])==T){} else{if(is.na(Species_1.1$V2[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V2)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V3[1])==T){} else{if(is.na(Species_1.1$V3[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V3)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V4[1])==T){} else{if(is.na(Species_1.1$V4[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V4)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V5[1])==T){} else{if(is.na(Species_1.1$V5[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V5)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V6[1])==T){} else{if(is.na(Species_1.1$V6[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V6)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V7[1])==T){} else{if(is.na(Species_1.1$V7[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V7)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V8[1])==T){} else{if(is.na(Species_1.1$V8[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V8)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V9[1])==T){} else{if(is.na(Species_1.1$V9[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V9)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V10[1])==T){} else{if(is.na(Species_1.1$V10[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V10)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V11[1])==T){} else{if(is.na(Species_1.1$V11[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V11)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V12[1])==T){} else{if(is.na(Species_1.1$V12[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V12)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V13[1])==T){} else{if(is.na(Species_1.1$V13[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V13)
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
      if(2 %in% how_many_1.1_vect==T){
        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_1.1$V1[2])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_1.1$V2[2])==T){} else{if(is.na(Species_1.1$V2[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V2[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V3[2])==T){} else{if(is.na(Species_1.1$V3[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V3[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V4[2])==T){} else{if(is.na(Species_1.1$V4[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V4[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V5[2])==T){} else{if(is.na(Species_1.1$V5[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V5[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V6[2])==T){} else{if(is.na(Species_1.1$V6[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V6[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V7[2])==T){} else{if(is.na(Species_1.1$V7[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V7[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V8[2])==T){} else{if(is.na(Species_1.1$V8[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V8[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V9[2])==T){} else{if(is.na(Species_1.1$V9[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V9[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V10[2])==T){} else{if(is.na(Species_1.1$V10[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V10[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V11[2])==T){} else{if(is.na(Species_1.1$V11[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V11[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V12[2])==T){} else{if(is.na(Species_1.1$V12[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V12[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_1.1$V13[2])==T){} else{if(is.na(Species_1.1$V13[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_1.1$V13[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
    }

    #Verificar se ameaça anterior foi incluída ou não
    if(exists("Species_2.1.4")==F){} else{if(exists("Species_1.1")==T){
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 4}\n")
      cat("Sleep 1000\n")
    }}

    if(exists("Species_2.1.4")){
      cat("Send {Down ")
      cat(Species_2.1.4$Ameaca_cod[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(Species_2.1.4$Estresse[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter 3}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 6}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      if(how_many_2.1.4==1){
        cat("Send {Text}")
        cat(Species_2.1.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.1.4==2){
        cat("Send {Text}")
        cat(Species_2.1.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.1.4==3){
        cat("Send {Text}")
        cat(Species_2.1.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[3])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.1.4==4){
        cat("Send {Text}")
        cat(Species_2.1.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[4])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.1.4==5){
        cat("Send {Text}")
        cat(Species_2.1.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[5])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.1.4==6){
        cat("Send {Text}")
        cat(Species_2.1.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[5])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$text[6])
        cat("\n")
        cat("Sleep 1000\n")
      }
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      #Referências
      if(1 %in% how_many_2.1.4_vect==T){
        cat("Send {Text}")
        cat(Species_2.1.4$V1[1])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_2.1.4$V2[1])==T){} else{if(is.na(Species_2.1.4$V2[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V2)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V3[1])==T){} else{if(is.na(Species_2.1.4$V3[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V3)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V4[1])==T){} else{if(is.na(Species_2.1.4$V4[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V4)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V5[1])==T){} else{if(is.na(Species_2.1.4$V5[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V5)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V6[1])==T){} else{if(is.na(Species_2.1.4$V6[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V6)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V7[1])==T){} else{if(is.na(Species_2.1.4$V7[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V7)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V8[1])==T){} else{if(is.na(Species_2.1.4$V8[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V8)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V9[1])==T){} else{if(is.na(Species_2.1.4$V9[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V9)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V10[1])==T){} else{if(is.na(Species_2.1.4$V10[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V10)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V11[1])==T){} else{if(is.na(Species_2.1.4$V11[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V11)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V12[1])==T){} else{if(is.na(Species_2.1.4$V12[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V12)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V13[1])==T){} else{if(is.na(Species_2.1.4$V13[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V13)
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
      if(2 %in% how_many_2.1.4_vect==T){
        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.1.4$V1[2])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_2.1.4$V2[2])==T){} else{if(is.na(Species_2.1.4$V2[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V2[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V3[2])==T){} else{if(is.na(Species_2.1.4$V3[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V3[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V4[2])==T){} else{if(is.na(Species_2.1.4$V4[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V4[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V5[2])==T){} else{if(is.na(Species_2.1.4$V5[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V5[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V6[2])==T){} else{if(is.na(Species_2.1.4$V6[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V6[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V7[2])==T){} else{if(is.na(Species_2.1.4$V7[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V7[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V8[2])==T){} else{if(is.na(Species_2.1.4$V8[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V8[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V9[2])==T){} else{if(is.na(Species_2.1.4$V9[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V9[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V10[2])==T){} else{if(is.na(Species_2.1.4$V10[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V10[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V11[2])==T){} else{if(is.na(Species_2.1.4$V11[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V11[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V12[2])==T){} else{if(is.na(Species_2.1.4$V12[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V12[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.1.4$V13[2])==T){} else{if(is.na(Species_2.1.4$V13[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.1.4$V13[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
    }

    #Verificar se ameaça anterior foi incluída ou não
    if(exists("Species_2.2.3")==F){} else{if(exists("Species_1.1")==T | exists("Species_2.1.4")==T){
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 4}\n")
      cat("Sleep 1000\n")
    }}

    if(exists("Species_2.2.3")){
      cat("Send {Down ")
      cat(Species_2.2.3$Ameaca_cod[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(Species_2.2.3$Estresse[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter 3}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 6}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      if(how_many_2.2.3==1){
        cat("Send {Text}")
        cat(Species_2.2.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.2.3==2){
        cat("Send {Text}")
        cat(Species_2.2.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.2.3==3){
        cat("Send {Text}")
        cat(Species_2.2.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[3])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.2.3==4){
        cat("Send {Text}")
        cat(Species_2.2.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[4])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.2.3==5){
        cat("Send {Text}")
        cat(Species_2.2.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[5])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.2.3==6){
        cat("Send {Text}")
        cat(Species_2.2.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[5])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$text[6])
        cat("\n")
        cat("Sleep 1000\n")
      }
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      #Referências
      if(1 %in% how_many_2.2.3_vect==T){
        cat("Send {Text}")
        cat(Species_2.2.3$V1[1])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_2.2.3$V2[1])==T){} else{if(is.na(Species_2.2.3$V2[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V2)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V3[1])==T){} else{if(is.na(Species_2.2.3$V3[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V3)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V4[1])==T){} else{if(is.na(Species_2.2.3$V4[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V4)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V5[1])==T){} else{if(is.na(Species_2.2.3$V5[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V5)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V6[1])==T){} else{if(is.na(Species_2.2.3$V6[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V6)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V7[1])==T){} else{if(is.na(Species_2.2.3$V7[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V7)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V8[1])==T){} else{if(is.na(Species_2.2.3$V8[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V8)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V9[1])==T){} else{if(is.na(Species_2.2.3$V9[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V9)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V10[1])==T){} else{if(is.na(Species_2.2.3$V10[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V10)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V11[1])==T){} else{if(is.na(Species_2.2.3$V11[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V11)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V12[1])==T){} else{if(is.na(Species_2.2.3$V12[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V12)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V13[1])==T){} else{if(is.na(Species_2.2.3$V13[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V13)
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
      if(2 %in% how_many_2.2.3_vect==T){
        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.2.3$V1[2])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_2.2.3$V2[2])==T){} else{if(is.na(Species_2.2.3$V2[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V2[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V3[2])==T){} else{if(is.na(Species_2.2.3$V3[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V3[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V4[2])==T){} else{if(is.na(Species_2.2.3$V4[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V4[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V5[2])==T){} else{if(is.na(Species_2.2.3$V5[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V5[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V6[2])==T){} else{if(is.na(Species_2.2.3$V6[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V6[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V7[2])==T){} else{if(is.na(Species_2.2.3$V7[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V7[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V8[2])==T){} else{if(is.na(Species_2.2.3$V8[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V8[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V9[2])==T){} else{if(is.na(Species_2.2.3$V9[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V9[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V10[2])==T){} else{if(is.na(Species_2.2.3$V10[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V10[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V11[2])==T){} else{if(is.na(Species_2.2.3$V11[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V11[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V12[2])==T){} else{if(is.na(Species_2.2.3$V12[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V12[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.2.3$V13[2])==T){} else{if(is.na(Species_2.2.3$V13[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.2.3$V13[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
    }

    #Verificar se ameaça anterior foi incluída ou não
    if(exists("Species_2.3.4")==F){} else{if(exists("Species_1.1")==T | exists("Species_2.1.4")==T | exists("Species_2.2.3")==T){
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 4}\n")
      cat("Sleep 1000\n")
    }}

    if(exists("Species_2.3.4")){
      cat("Send {Down ")
      cat(Species_2.3.4$Ameaca_cod[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(Species_2.3.4$Estresse[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter 3}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 6}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      if(how_many_2.3.4==1){
        cat("Send {Text}")
        cat(Species_2.3.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.3.4==2){
        cat("Send {Text}")
        cat(Species_2.3.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.3.4==3){
        cat("Send {Text}")
        cat(Species_2.3.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[3])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.3.4==4){
        cat("Send {Text}")
        cat(Species_2.3.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[4])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.3.4==5){
        cat("Send {Text}")
        cat(Species_2.3.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[5])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_2.3.4==6){
        cat("Send {Text}")
        cat(Species_2.3.4$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[5])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$text[6])
        cat("\n")
        cat("Sleep 1000\n")
      }
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      #Referências
      if(1 %in% how_many_2.3.4_vect==T){
        cat("Send {Text}")
        cat(Species_2.3.4$V1[1])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_2.3.4$V2[1])==T){} else{if(is.na(Species_2.3.4$V2[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V2)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V3[1])==T){} else{if(is.na(Species_2.3.4$V3[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V3)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V4[1])==T){} else{if(is.na(Species_2.3.4$V4[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V4)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V5[1])==T){} else{if(is.na(Species_2.3.4$V5[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V5)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V6[1])==T){} else{if(is.na(Species_2.3.4$V6[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V6)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V7[1])==T){} else{if(is.na(Species_2.3.4$V7[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V7)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V8[1])==T){} else{if(is.na(Species_2.3.4$V8[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V8)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V9[1])==T){} else{if(is.na(Species_2.3.4$V9[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V9)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V10[1])==T){} else{if(is.na(Species_2.3.4$V10[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V10)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V11[1])==T){} else{if(is.na(Species_2.3.4$V11[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V11)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V12[1])==T){} else{if(is.na(Species_2.3.4$V12[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V12)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V13[1])==T){} else{if(is.na(Species_2.3.4$V13[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V13)
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
      if(2 %in% how_many_2.3.4_vect==T){
        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_2.3.4$V1[2])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_2.3.4$V2[2])==T){} else{if(is.na(Species_2.3.4$V2[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V2[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V3[2])==T){} else{if(is.na(Species_2.3.4$V3[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V3[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V4[2])==T){} else{if(is.na(Species_2.3.4$V4[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V4[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V5[2])==T){} else{if(is.na(Species_2.3.4$V5[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V5[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V6[2])==T){} else{if(is.na(Species_2.3.4$V6[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V6[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V7[2])==T){} else{if(is.na(Species_2.3.4$V7[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V7[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V8[2])==T){} else{if(is.na(Species_2.3.4$V8[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V8[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V9[2])==T){} else{if(is.na(Species_2.3.4$V9[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V9[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V10[2])==T){} else{if(is.na(Species_2.3.4$V10[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V10[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V11[2])==T){} else{if(is.na(Species_2.3.4$V11[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V11[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V12[2])==T){} else{if(is.na(Species_2.3.4$V12[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V12[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_2.3.4$V13[2])==T){} else{if(is.na(Species_2.3.4$V13[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_2.3.4$V13[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
    }

    #Verificar se ameaças anteriores foram incluídas ou não
    if(exists("Species_3.2")==F){} else{if(exists("Species_1.1")==T | exists("Species_2.1.4")==T | exists("Species_2.2.3")==T | exists("Species_2.3.4")==T){
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 4}\n")
      cat("Sleep 1000\n")
    }}

    if(exists("Species_3.2")){
      cat("Send {Down ")
      cat(Species_3.2$Ameaca_cod[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(Species_3.2$Estresse[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter 3}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 6}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      if(how_many_3.2==1){
        cat("Send {Text}")
        cat(Species_3.2$text[1])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_3.2==2){
        cat("Send {Text}")
        cat(Species_3.2$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[2])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_3.2==3){
        cat("Send {Text}")
        cat(Species_3.2$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[3])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_3.2==4){
        cat("Send {Text}")
        cat(Species_3.2$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[4])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_3.2==5){
        cat("Send {Text}")
        cat(Species_3.2$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[5])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_3.2==6){
        cat("Send {Text}")
        cat(Species_3.2$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[5])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$text[6])
        cat("\n")
        cat("Sleep 1000\n")
      }
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      #Referências
      if(1 %in% how_many_3.2_vect==T){
        cat("Send {Text}")
        cat(Species_3.2$V1[1])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_3.2$V2[1])==T){} else{if(is.na(Species_3.2$V2[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V2)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V3[1])==T){} else{if(is.na(Species_3.2$V3[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V3)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V4[1])==T){} else{if(is.na(Species_3.2$V4[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V4)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V5[1])==T){} else{if(is.na(Species_3.2$V5[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V5)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V6[1])==T){} else{if(is.na(Species_3.2$V6[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V6)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V7[1])==T){} else{if(is.na(Species_3.2$V7[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V7)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V8[1])==T){} else{if(is.na(Species_3.2$V8[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V8)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V9[1])==T){} else{if(is.na(Species_3.2$V9[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V9)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V10[1])==T){} else{if(is.na(Species_3.2$V10[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V10)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V11[1])==T){} else{if(is.na(Species_3.2$V11[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V11)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V12[1])==T){} else{if(is.na(Species_3.2$V12[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V12)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V13[1])==T){} else{if(is.na(Species_3.2$V13[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V13)
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
      if(2 %in% how_many_3.2_vect==T){
        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_3.2$V1[2])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_3.2$V2[2])==T){} else{if(is.na(Species_3.2$V2[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V2[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V3[2])==T){} else{if(is.na(Species_3.2$V3[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V3[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V4[2])==T){} else{if(is.na(Species_3.2$V4[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V4[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V5[2])==T){} else{if(is.na(Species_3.2$V5[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V5[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V6[2])==T){} else{if(is.na(Species_3.2$V6[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V6[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V7[2])==T){} else{if(is.na(Species_3.2$V7[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V7[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V8[2])==T){} else{if(is.na(Species_3.2$V8[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V8[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V9[2])==T){} else{if(is.na(Species_3.2$V9[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V9[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V10[2])==T){} else{if(is.na(Species_3.2$V10[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V10[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V11[2])==T){} else{if(is.na(Species_3.2$V11[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V11[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V12[2])==T){} else{if(is.na(Species_3.2$V12[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V12[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_3.2$V13[2])==T){} else{if(is.na(Species_3.2$V13[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_3.2$V13[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
    }

    #Verificar se ameaças anteriores foram incluídas ou não
    if(exists("Species_7.1.3")==F){} else{if(exists("Species_1.1")==T | exists("Species_2.1.4")==T | exists("Species_2.2.3")==T | exists("Species_2.3.4")==T | exists("Species_3.2")==T){
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 4}\n")
      cat("Sleep 1000\n")
    }}

    if(exists("Species_7.1.3")){
      cat("Send {Down ")
      cat(Species_7.1.3$Ameaca_cod[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Down ")
      cat(Species_7.1.3$Estresse[1])
      cat("}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter 3}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 6}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 4}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      cat("Send +{Tab 2}\n")
      cat("Sleep 1000\n")
      cat("Send {Down 3}\n")
      cat("Sleep 1000\n")
      cat("Send {Tab 3}\n")
      cat("Sleep 1000\n")
      if(how_many_7.1.3==1){
        cat("Send {Text}")
        cat(Species_7.1.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_7.1.3==2){
        cat("Send {Text}")
        cat(Species_7.1.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_7.1.3==3){
        cat("Send {Text}")
        cat(Species_7.1.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[3])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_7.1.3==4){
        cat("Send {Text}")
        cat(Species_7.1.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[4])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_7.1.3==5){
        cat("Send {Text}")
        cat(Species_7.1.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[5])
        cat("\n")
        cat("Sleep 1000\n")
      }
      if(how_many_7.1.3==6){
        cat("Send {Text}")
        cat(Species_7.1.3$text[1])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[2])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[3])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[4])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[5])
        cat("\n")
        cat("Sleep 1000\n")
        cat("Send {Space}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$text[6])
        cat("\n")
        cat("Sleep 1000\n")
      }
      cat("Send {Tab}\n")
      cat("Sleep 1000\n")
      cat("Send {Enter}\n")
      cat("Sleep 1000\n")
      #Referências
      if(1 %in% how_many_7.1.3_vect==T){
        cat("Send {Text}")
        cat(Species_7.1.3$V1[1])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_7.1.3$V2[1])==T){} else{if(is.na(Species_7.1.3$V2[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V2)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V3[1])==T){} else{if(is.na(Species_7.1.3$V3[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V3)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V4[1])==T){} else{if(is.na(Species_7.1.3$V4[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V4)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V5[1])==T){} else{if(is.na(Species_7.1.3$V5[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V5)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V6[1])==T){} else{if(is.na(Species_7.1.3$V6[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V6)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V7[1])==T){} else{if(is.na(Species_7.1.3$V7[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V7)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V8[1])==T){} else{if(is.na(Species_7.1.3$V8[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V8)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V9[1])==T){} else{if(is.na(Species_7.1.3$V9[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V9)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V10[1])==T){} else{if(is.na(Species_7.1.3$V10[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V10)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V11[1])==T){} else{if(is.na(Species_7.1.3$V11[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V11)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V12[1])==T){} else{if(is.na(Species_7.1.3$V12[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V12)
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V13[1])==T){} else{if(is.na(Species_7.1.3$V13[1])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V13)
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
      if(2 %in% how_many_7.1.3_vect==T){
        cat("Send {Tab 2}\n")
        cat("Sleep 1000\n")
        cat("Send {Enter}\n")
        cat("Sleep 1000\n")
        cat("Send {Text}")
        cat(Species_7.1.3$V1[2])
        cat("\n")
        cat("Sleep 1000\n")
        if(is.null(Species_7.1.3$V2[2])==T){} else{if(is.na(Species_7.1.3$V2[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V2[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V3[2])==T){} else{if(is.na(Species_7.1.3$V3[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V3[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V4[2])==T){} else{if(is.na(Species_7.1.3$V4[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V4[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V5[2])==T){} else{if(is.na(Species_7.1.3$V5[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V5[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V6[2])==T){} else{if(is.na(Species_7.1.3$V6[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V6[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V7[2])==T){} else{if(is.na(Species_7.1.3$V7[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V7[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V8[2])==T){} else{if(is.na(Species_7.1.3$V8[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V8[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V9[2])==T){} else{if(is.na(Species_7.1.3$V9[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V9[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V10[2])==T){} else{if(is.na(Species_7.1.3$V10[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V10[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V11[2])==T){} else{if(is.na(Species_7.1.3$V11[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V11[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V12[2])==T){} else{if(is.na(Species_7.1.3$V12[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V12[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
        if(is.null(Species_7.1.3$V13[2])==T){} else{if(is.na(Species_7.1.3$V13[2])==T){} else{
          cat("Send {Tab 2}\n")
          cat("Sleep 1000\n")
          cat("Send {Enter}\n")
          cat("Sleep 1000\n")
          cat("Send {Text}")
          cat(Species_7.1.3$V13[2])
          cat("\n")
          cat("Sleep 1000\n")
        }}
      }
    }

    cat("\n")
    cat("Send {Tab 4}\n")
    cat("Sleep 1000\n")
    cat("Send {Enter}\n")
    cat("Sleep 6000\n")
    cat("Send ^w\n")
    cat("\n")

    cat("\nreturn")
    sink()

    #Removendo as referências
    if(exists("Species_1.1_refs_1")){rm(Species_1.1_refs_1)}
    if(exists("Species_1.1_refs_2")){rm(Species_1.1_refs_2)}
    if(exists("Species_1.1_refs_3")){rm(Species_1.1_refs_3)}
    if(exists("Species_1.1_refs_4")){rm(Species_1.1_refs_4)}
    if(exists("Species_1.1_refs_5")){rm(Species_1.1_refs_5)}
    if(exists("Species_1.1_refs_6")){rm(Species_1.1_refs_6)}

    if(exists("Species_2.1.4_refs_1")){rm(Species_2.1.4_refs_1)}
    if(exists("Species_2.1.4_refs_2")){rm(Species_2.1.4_refs_2)}
    if(exists("Species_2.1.4_refs_3")){rm(Species_2.1.4_refs_3)}
    if(exists("Species_2.1.4_refs_4")){rm(Species_2.1.4_refs_4)}
    if(exists("Species_2.1.4_refs_5")){rm(Species_2.1.4_refs_5)}
    if(exists("Species_2.1.4_refs_6")){rm(Species_2.1.4_refs_6)}

    if(exists("Species_2.2.3_refs_1")){rm(Species_2.2.3_refs_1)}
    if(exists("Species_2.2.3_refs_2")){rm(Species_2.2.3_refs_2)}
    if(exists("Species_2.2.3_refs_3")){rm(Species_2.2.3_refs_3)}
    if(exists("Species_2.2.3_refs_4")){rm(Species_2.2.3_refs_4)}
    if(exists("Species_2.2.3_refs_5")){rm(Species_2.2.3_refs_5)}
    if(exists("Species_2.2.3_refs_6")){rm(Species_2.2.3_refs_6)}

    if(exists("Species_2.3.4_refs_1")){rm(Species_2.3.4_refs_1)}
    if(exists("Species_2.3.4_refs_2")){rm(Species_2.3.4_refs_2)}
    if(exists("Species_2.3.4_refs_3")){rm(Species_2.3.4_refs_3)}
    if(exists("Species_2.3.4_refs_4")){rm(Species_2.3.4_refs_4)}
    if(exists("Species_2.3.4_refs_5")){rm(Species_2.3.4_refs_5)}
    if(exists("Species_2.3.4_refs_6")){rm(Species_2.3.4_refs_6)}

    if(exists("Species_3.2_refs_1")){rm(Species_3.2_refs_1)}
    if(exists("Species_3.2_refs_2")){rm(Species_3.2_refs_2)}
    if(exists("Species_3.2_refs_3")){rm(Species_3.2_refs_3)}
    if(exists("Species_3.2_refs_4")){rm(Species_3.2_refs_4)}
    if(exists("Species_3.2_refs_5")){rm(Species_3.2_refs_5)}
    if(exists("Species_3.2_refs_6")){rm(Species_3.2_refs_6)}

    if(exists("Species_7.1.3_refs_1")){rm(Species_7.1.3_refs_1)}
    if(exists("Species_7.1.3_refs_2")){rm(Species_7.1.3_refs_2)}
    if(exists("Species_7.1.3_refs_3")){rm(Species_7.1.3_refs_3)}
    if(exists("Species_7.1.3_refs_4")){rm(Species_7.1.3_refs_4)}
    if(exists("Species_7.1.3_refs_5")){rm(Species_7.1.3_refs_5)}
    if(exists("Species_7.1.3_refs_6")){rm(Species_7.1.3_refs_6)}

  }

  #Renderizando o output final (All)

  sink(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/outputs/AHK_scripts/fill_threats.ahk"

    )

  )
  cat("F4::")
  cat("\n")

  for(i in listOfSpecies$species){

    Script <- readtext(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/AHK_scripts/fill_threats-", i, ".ahk"

      )

    )
    Script$text<-sub("return","Sleep 5000\n",Script$text)
    Script$text<-sub("F4\\:\\:","",Script$text)

    cat(Script$text)
    cat("\n")

    file.remove(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/outputs/AHK_scripts/fill_threats-", i, ".ahk"

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
