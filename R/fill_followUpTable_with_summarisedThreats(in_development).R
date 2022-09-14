fill_followUpTable_with_summarisedThreats <- function(){

  library(googledrive)
  library(googlesheets4)
  library(stringr)


  # Get list of species file (species_fill_followUpTable_with_summarisedThreats.csv) ####

  ## Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_fill_followUpTable_with_summarisedThreats.csv"

  )

  ## Import the list of species file from local path ####

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = F,
    sep = ";",
    encoding = "UTF-8"

  )

  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")
  read_ss_sheet1 <- read_sheet(ss, sheet = 1)

  species_without_summarisedThreats_in_followUpTable_sheet1 <-
    read_ss_sheet1 %>%
    dplyr::filter(NameFB_semAutor %in% listOfSpecies) %>%
    dplyr::filter(is.na(`Continuing decline in extent of occurrence (EOO)`) == T &
                    is.na(`Continuing decline in area of occupancy (AOO)`) == T) %>%
    dplyr::select(NameFB_semAutor)

  if(nrow(species_without_summarisedThreats_in_followUpTable_sheet1) == 0){

    rm(species_without_summarisedThreats_in_followUpTable_sheet1)

  } else {

    species_without_summarisedThreats_in_followUpTable_sheet1 <- data.frame(

      species = species_without_summarisedThreats_in_followUpTable_sheet1$NameFB_semAutor,
      summarisedThreats = paste0(

        ""

      )

    )

  }

  for(species_without_summarisedThreats in species_without_summarisedThreats_in_followUpTable_sheet1$NameFB_semAutor){

    species <- species_without_summarisedThreats

    species.input <- read.csv(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/species.inputs/overlayAnalysis results/TodosOsAnos/",
        species,
        ".csv"

      ),
      header = T,
      sep = ","

    )

    species.input_AOO_diff <- species.input %>% select(Classe, Porcentagem_AOO_diff)
    species.input_AOO_diff <- species.input_AOO_diff %>% filter(Ameaca != "Total")
    species.input_AOO_diff <- species.input_AOO_diff %>% arrange(desc(as.numeric(Porcentagem_AOO_diff)))
    species.input_AOO_diff$Porcentagem_AOO_diff <- round(species.input_AOO_diff$Porcentagem_AOO_diff, 2)


  }


  if(exists("species_without_summarisedThreats_in_followUpTable_sheet1") == T){

    for(i in 1:nrow(species_without_summarisedThreats_in_followUpTable_sheet1)){

      celula_HTML <- which(read_ss_sheet1$NameFB_semAutor == species_without_summarisedThreats_in_followUpTable_sheet1$species[i])
      celula_HTML <- paste("BA", celula_HTML + 1, sep="")
      URL <- gs4_formula(species_without_summarisedThreats_in_followUpTable_sheet1$URL[i])

      range_write(

        ss,
        data = data.frame((URL)),
        range = celula_HTML,
        col_names = F,
        sheet = 1

      )

      Sys.sleep(2)

    }

  }

}

