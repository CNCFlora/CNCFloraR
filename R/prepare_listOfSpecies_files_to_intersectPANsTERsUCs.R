prepare_listOfSpecies_files_to_intersectPANsTERsUCs <- function(onlyNonExistentProfile = F){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species()


  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }


  if(onlyNonExistentProfile == T){

    listOfSpecies <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["intersectPANs",] == F &
            df["intersectTERs",] == F &
            df["intersectUCs",] == F

        )

      ]

    )

  } else{

    if(onlyNonExistentProfile == F){

      listOfSpecies <- colnames(

        df[

          which(

            df["occurrenceRecords",] == T

          )

        ]

      )

    }

  }

  # Load follow-up table from GoogleSheets ####

  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")
  followUpTable <- read_sheet(ss, sheet = 6)

  listOfSpecies_PNTs <- followUpTable %>% filter(`PA/PNA` == "PNA") %>% select(Espécie)

  listOfSpecies_PNTs <- listOfSpecies_PNTs %>% filter(Espécie %in% listOfSpecies_PNTs_)


  listOfSpecies <- c(listOfSpecies_PTs, listOfSpecies_PNTs$Espécie) |> unique()


  followUpTable.filtered <- followUpTable %>% dplyr::filter(Espécie %in% listOfSpecies)

  output <- data.frame(

    species = followUpTable.filtered$Espécie,
    flow = followUpTable.filtered$`PA/PNA`,
    records = followUpTable.filtered$Registros


  )


  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme="dark"

    )

  )


  # Ask to write the `species_intersect_UCs_PANs_TERs` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (species_intersect_UCs_PANs_TERs)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_intersect_UCs_PANs_TERs"

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

  #Done

}
