#' Read the conservation actions from profile of species HTML
#'
#' Ler as ações de conservação do perfil das espécies


read_actions_from_profileOfSpeciesHTML <- function(){

  library(XML)
  library(data.table)
  library(dplyr)
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

  htmls <- listOfSpecies

  htmls$V4 <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/outputs/profileOfSpeciesHTML results/", htmls$V2,".html"

  )

  htmls_n <- 1:nrow(htmls)

  output <- NULL
  for(i in htmls_n){

    species <- as.character(htmls[i, 2])
    flow <- htmls$V3[i]
    species_html <- readHTMLTable(as.character(htmls[i, 4]))

    if(flow == "PA"){species_html <- species_html[[3]]}

    if(flow == "PNA"){species_html <- species_html[[2]]}

    species_html <- data.frame(species = species, species_html)

    colnames(species_html) <- c(

      "species",
      "action",
      "situation",
      "text",
      "reference"

    )

    output_ <- species_html

    output <- rbind(output, output_)

  }

  write.csv2(

    output,
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/oldSystem_fill_tables/actions_table.csv"

    ),
    row.names = F,
    fileEncoding = "UTF-8"

  )

  message("Done.")

}

