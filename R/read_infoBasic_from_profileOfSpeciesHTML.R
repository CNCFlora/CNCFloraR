#' Read the basic informations from profile of species HTML
#'
#' Ler as informações básicas do perfil das espécies


read_infoBasic_from_profileOfSpeciesHTML <- function(){

  library(XML)
  library(dplyr)


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
    species_html <- readHTMLTable(as.character(htmls[i, 4]))
    species_html <- species_html[[1]]
    n_species_html <- as.numeric(count(species_html) + 1)
    n2_species_html <- as.numeric(count(species_html) + 2)
    species_html[n_species_html,] <- sub("\\s\\s\\s\\s.*", "", species_html[6,])
    species_html[n2_species_html,] <- sub(".*\\s\\s\\s\\s", "", species_html[6,])
    species_html[n2_species_html, 1]<-"Ref_FB2020"
    species_html <- rbind(

      species_html[2,],
      species_html[3,],
      species_html[5,],
      species_html[n_species_html,],
      species_html[n2_species_html,],
      species_html[7,],
      species_html[8,],
      species_html[9,],
      species_html[10,],
      species_html[11,],
      species_html[12,]

    )

    output_ <- species_html

    output_ <- data.frame(species = species, output_)

    colnames(output_) <- c("species", "variable", "info")

    output <- rbind(output, output_)

  }

  write.csv2(

    output,
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/oldSystem_fill_tables/infobasic_table.csv"

    ),
    row.names = F,
    fileEncoding = "UTF-8"

  )

  message("Done.")

}

