fill_followUpTable_with_completed_filling <- function(){

  library(googledrive)
  library(googlesheets4)
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

  listOfSpecies <- listOfSpecies$V2


  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")
  read_ss_sheet1 <- read_sheet(ss, sheet = 1)

  row_to_fill <- read_ss_sheet1 %>%
    dplyr::filter(is.na(HTMLs) == F) %>%
    dplyr::filter(is.na(`Preenc. Autom.`) == T) %>%
    dplyr::select(NameFB_semAutor)

  row_to_fill <- as.character(row_to_fill$NameFB_semAutor)

  row_to_fill <- row_to_fill[row_to_fill %in% listOfSpecies]

  if(length(row_to_fill) == 0){} else{

    for(i in 1:length(row_to_fill)){

      celula_HTML <- which(read_ss_sheet1$NameFB_semAutor == row_to_fill[i])
      celula_HTML <- paste("BB", celula_HTML + 1, sep="")

      range_write(

        ss,
        data = data.frame(as.character("x")),
        range = celula_HTML,
        col_names = F,
        sheet = 1

      )

      Sys.sleep(2)

    }

  }


}

