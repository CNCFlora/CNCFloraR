#' Read the Threats from profile of species HTML
#'
#' Ler as ameaças do perfil das espécies


read_threats_from_profileOfSpeciesHTML <- function(){

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
    species_html <- readHTMLTable(as.character(htmls[i, 4]))
    species_html <- species_html[[2]]

    species_html <- data.frame(species = species, species_html)

    output_ <- species_html

    colnames(output_) <- c(

      "species",
      "threat",
      "biome",
      "vegetation",
      "state",
      "municipality",
      "UC",
      "text",
      "reference"

    )

    output <- rbind(output, output_)

  }



  # Seleção de ameaças para preenchimento automático

  Ame_output <- NULL


  ## 1.1

  ### MapBiomas

  Ame_output_ <- output %>%
    dplyr::filter(threat == "1.1 Housing & urban areas") %>%
    dplyr::filter(

      str_detect(

        reference,
        "MapBiomas, 2022(a\\.|\\.) Projeto MapBiomas - Coleção 6 da Série Anual de Mapas de Cobertura e Uso de Solo do Brasil"

      )

    )

  if(count(Ame_output_) == 0){} else{

    Ame_output_ <- data.frame(Ame_output_, source = "MapBiomas")
    Ame_output <- rbind(Ame_output, Ame_output_)

  }


  ## 2.1.4

  ### MapBiomas

  Ame_output_ <- output %>%
    dplyr::filter(threat == "2.1.4 Scale Unknown/Unrecorded") %>%
    dplyr::filter(

      str_detect(

        reference,
        "MapBiomas, 2022(a\\.|\\.) Projeto MapBiomas - Coleção 6 da Série Anual de Mapas de Cobertura e Uso de Solo do Brasil"

      )

    )

  if(count(Ame_output_) == 0){} else{

    Ame_output_ <- data.frame(Ame_output_, source = "MapBiomas")
    Ame_output <- rbind(Ame_output, Ame_output_)

  }


  ### IBGE

  Ame_output_ <- output %>%
    dplyr::filter(threat == "2.1.4 Scale Unknown/Unrecorded") %>%
    dplyr::filter(

      str_detect(

        text,
        "De acordo com o IBGE, "

      )

    )

  if(count(Ame_output_) == 0){} else{

    Ame_output_ <- data.frame(Ame_output_, source = "IBGE")
    Ame_output <- rbind(Ame_output, Ame_output_)

  }


  ## 2.2.3

  ### MapBiomas

  Ame_output_ <- output %>%
    dplyr::filter(threat == "2.2.3 Scale Unknown/Unrecorded") %>%
    dplyr::filter(

      str_detect(

        reference,
        "MapBiomas, 2022(a\\.|\\.) Projeto MapBiomas - Coleção 6 da Série Anual de Mapas de Cobertura e Uso de Solo do Brasil"

      )

    )

  if(count(Ame_output_) == 0){} else{

    Ame_output_ <- data.frame(Ame_output_, source = "MapBiomas")
    Ame_output <- rbind(Ame_output, Ame_output_)

  }

  ### IBGE

  Ame_output_ <- output %>%
    dplyr::filter(threat == "2.2.3 Scale Unknown/Unrecorded") %>%
    dplyr::filter(

      str_detect(

        text,
        "De acordo com o IBGE, "

      )

    )

  if(count(Ame_output_) == 0){} else{

    Ame_output_ <- data.frame(Ame_output_, source = "IBGE")
    Ame_output <- rbind(Ame_output, Ame_output_)

  }


  ## 2.3.4

  ### Lapig Pastagens

  Ame_output_ <- output %>%
    dplyr::filter(threat == "2.3.4 Scale Unknown/Unrecorded") %>%
    dplyr::filter(

      str_detect(

        text,
        "De acordo com o Atlas das Pastagens Brasileiras, "

      )

    )

  if(count(Ame_output_) == 0){} else{

    Ame_output_ <- data.frame(Ame_output_, source = "Lapig_Pastagens")
    Ame_output <- rbind(Ame_output, Ame_output_)

  }

  ### MapBiomas

  Ame_output_ <- output %>%
    dplyr::filter(threat == "2.3.4 Scale Unknown/Unrecorded") %>%
    dplyr::filter(

      str_detect(

        reference,
        "MapBiomas, 2022(a\\.|\\.) Projeto MapBiomas - Coleção 6 da Série Anual de Mapas de Cobertura e Uso de Solo do Brasil"

      )

    )

  if(count(Ame_output_) == 0){} else{

    Ame_output_ <- data.frame(Ame_output_, source = "MapBiomas")
    Ame_output <- rbind(Ame_output, Ame_output_)

  }


  ## 3.2

  ### MapBiomas

  Ame_output_ <- output %>%
    dplyr::filter(threat == "3.2 Mining & quarrying") %>%
    dplyr::filter(

      str_detect(

        reference,
        "MapBiomas, 2022(a\\.|\\.) Projeto MapBiomas - Coleção 6 da Série Anual de Mapas de Cobertura e Uso de Solo do Brasil"

      )

    )

  if(count(Ame_output_) == 0){} else{

    Ame_output_ <- data.frame(Ame_output_, source = "MapBiomas")
    Ame_output <- rbind(Ame_output, Ame_output_)

  }


  ## 7.1.3

  ### MapBiomas

  Ame_output_ <- output %>%
    dplyr::filter(threat == "7.1.3 Trend Unknown/Unrecorded") %>%
    dplyr::filter(

      str_detect(

        reference,
        "MapBiomas-Fogo, 2022"

      )

    )

  if(count(Ame_output_) == 0){} else{

    Ame_output_ <- data.frame(Ame_output_, source = "MapBiomas")
    Ame_output <- rbind(Ame_output, Ame_output_)

  }

  output <- Ame_output

  write.csv2(

    output,
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/oldSystem_fill_tables/threats_table.csv"

    ),
    row.names = F,
    fileEncoding = "UTF-8"

  )

  message("Done.")

}

