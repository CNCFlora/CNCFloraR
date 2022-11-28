prepare_listOfSpecies_files_to_build_profileOfSpeciesHTMLs <- function(onlyNonExistentProfile = F, ask_to_write_file = T){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species(ask_to_open_file = F)


  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }

  if(onlyNonExistentProfile == T){

    listOfSpecies_PTs <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["intersectPANs",] == T &
            df["intersectTERs",] == T &
            df["intersectUCs",] == T &
            df["overlayMapBiomasTodosOsAnos",] == T &
            df["overlayMapBiomasFire",] == T &
            df["HTMLprofile",] == F

        )

      ]

    )

  } else{

    if(onlyNonExistentProfile == F){

      listOfSpecies_PTs <- colnames(

        df[

          which(

            df["occurrenceRecords",] == T &
              df["intersectPANs",] == T &
              df["intersectTERs",] == T &
              df["intersectUCs",] == T &
              df["overlayMapBiomasTodosOsAnos",] == T &
              df["overlayMapBiomasFire",] == T

          )

        ]

      )

    }

  }


  if(onlyNonExistentProfile == T){

    listOfSpecies_PNTs_ <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["intersectPANs",] == T &
            df["intersectTERs",] == T &
            df["intersectUCs",] == T &
            df["HTMLprofile",] == F

        )

      ]

    )

  } else{

    if(onlyNonExistentProfile == F){

      listOfSpecies_PNTs_ <- colnames(

        df[

          which(

            df["occurrenceRecords",] == T &
              df["intersectPANs",] == T &
              df["intersectTERs",] == T &
              df["intersectUCs",] == T

          )

        ]

      )

    }

  }

  # Load follow-up table from GoogleSheets ####

  ss <- gs4_get(ss_followUpTable_URL)
  followUpTable <- read_sheet(ss, sheet = which(ss$sheets$name == "List_for_HTML_profile"))

  listOfSpecies_PNTs <- followUpTable %>% dplyr::filter(`PA/PNA` == "PNA") %>% dplyr::select(Espécie)

  listOfSpecies_PNTs <- listOfSpecies_PNTs %>% dplyr::filter(Espécie %in% listOfSpecies_PNTs_)


  listOfSpecies <- c(listOfSpecies_PTs, listOfSpecies_PNTs$Espécie) |> unique()


  followUpTable.filtered <- followUpTable %>% dplyr::filter(Espécie %in% listOfSpecies)

  output <- data.frame(

    species = followUpTable.filtered$Espécie,
    flow = followUpTable.filtered$`PA/PNA`,
    records = followUpTable.filtered$Registros


  )

  if(T %in% duplicated(output)){

    output <- output[-which(duplicated(output)),]

  }



  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme="dark"

    )

  )


  if(ask_to_write_file == T){

    # Ask to write the `species_profileOfSpeciesHTML.csv` file

    answer <- ""

    while(

      answer != "Y" |
      answer != "N"

    ){

      answer <-
        toupper(readline("Write the list of species file (species_profileOfSpeciesHTML.csv)? (y/n): "))

      if(answer == "Y"){

        write.table(

          output,
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_profileOfSpeciesHTML.csv"

          ),
          col.names = F,
          row.names = F,
          sep = ";"

        )

        message("File created.")

        break

      }

      if(answer == "N"){

        break

      }

    }

  } else {

    write.table(

      output,
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_profileOfSpeciesHTML.csv"

      ),
      col.names = F,
      row.names = F,
      sep = ";"

    )

  }

  #Done

}
