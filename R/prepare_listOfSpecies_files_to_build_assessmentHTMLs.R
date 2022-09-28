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

  # Load follow-up table from GoogleSheets ####

  followUpTable <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=674274536")
  followUpTable_sheet7 <- read_sheet(followUpTable, sheet = which(ss$sheets$name == "List_for_HTML_assessment"))

  followUpTable_sheet7.filtered <-
    followUpTable_sheet7 %>%
    dplyr::filter(is.na(as.character(Espécie)) == F) %>%
    dplyr::filter(is.na(as.character(Feito)) == T) %>%
    unique()

  followUpTable_sheet7.listOfSpecies <- data.frame(

    species = followUpTable_sheet7.filtered$Espécie

  )

  if(T %in% duplicated(followUpTable_sheet7.listOfSpecies)){

    followUpTable_sheet7.listOfSpecies <-
      followUpTable_sheet7.listOfSpecies[-which(duplicated(followUpTable_sheet7.listOfSpecies)),]

  }

  listOfSpecies <- listOfSpecies[listOfSpecies %in% followUpTable_sheet7.listOfSpecies]


  output <- data.frame(

    species = listOfSpecies

  )


  # Print the results

  options(colorDF_n = Inf)

  print(

    colorDF(

      output,
      theme="dark"

    )

  )


  # Ask to write the `species_assessment.csv` file

  answer <- ""

  while(

    answer != "Y" |
    answer != "N"

  ){

    answer <-
      toupper(readline("Write the list of species file (species_assessment.csv)? (y/n): "))

    if(answer == "Y"){

      write.table(

        output,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_assessment.csv"

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
