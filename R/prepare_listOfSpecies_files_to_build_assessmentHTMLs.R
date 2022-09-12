prepare_listOfSpecies_files_to_build_assessmentHTMLs <- function(onlyNonExistentAssessment = F){

  library(stringr)
  library(googlesheets4)
  library(colorDF)

  df <- check_all_files_of_species()


  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }

  if(onlyNonExistentAssessment == T){

    listOfSpecies <- colnames(

      df[

        which(

          df["filledProfile_from_oldSystem",] == T &
            df["overlayMapBiomasQuadOfGrid",] == T &
            df["overlayMapBiomasAOOinEOObuffer",] == T &
            df["trendQuadOfGrid",] == T &
            df["shapefilePoints",] == T &
            df["shapefileAOO",] == T &
            df["shapefileAOO_MapBiomas",] == T &
            df["shapefileEOO",] == T &
            df["HTMLassessment",] == F

        )

      ]

    )

  } else{

    if(onlyNonExistentAssessment == F){

      listOfSpecies <- colnames(

        df[

          which(

            df["filledProfile_from_oldSystem",] == T &
              df["overlayMapBiomasQuadOfGrid",] == T &
              df["overlayMapBiomasAOOinEOObuffer",] == T &
              df["trendQuadOfGrid",] == T &
              df["shapefilePoints",] == T &
              df["shapefileAOO",] == T &
              df["shapefileAOO_MapBiomas",] == T &
              df["shapefileEOO",] == T

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


  # Ask to write the `species_assessmentHTML.csv` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (species_assessmentHTML.csv)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_assessmentHTML.csv"

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
