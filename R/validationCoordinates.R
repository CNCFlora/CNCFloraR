#' Verify the validation of Coordinates and fix errors
#'
#' Verifica se há erros tipográficos nas coordenadas e os corrigede

validationCoordinates <- function(list = "", ask_to_open_file = F) {

  # Load libraries ####

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(sf)
        library(rvest)
        library(rgeos)
        library(dplyr)
        library(stringr)
        library(tidyr)
        library(rlist)
        library(units)
        library(googledrive)
        library(data.table)

      })
    })
  })


  if(list[1] == ""){

    # List of Species file (validationCoordinates.csv) ####

    ## Get local path of the downloaded list of species file ####

    listOfSpecies_localPath <- paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/validationCoordinates.csv"

    )

    ## Ask to open the list of species file ####
    if(ask_to_open_file == T){

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
      sep = ",",
      encoding = "UTF-8"

    )

  }


  listOfSpecies_n <- 1:as.numeric(length(listOfSpecies$V1))


  # Create an empty output variable for loop ####

  output <- NULL


  # Start loop ####

  for(i in listOfSpecies_n){

    rm(list = setdiff(ls(), c("i", "listOfSpecies", "listOfSpecies_n", "output")))

    Species <- listOfSpecies[i,]

    # Obtain data of occurrence records from old System ####

    input <- get_occurrenceRecords_from_oldSystem(Species)


    # Obtenção do status de validação dos registros ####

    registros<-input %>% html_nodes(".label-valid") %>% html_text()
    registros<-str_extract(registros, "\\w+")


    # Obtenção dos códigos URN ####

    URNs<-input %>% html_nodes("a") %>% html_attr("name")
    URNs<-as.data.frame(URNs)
    URNs<-URNs %>% drop_na()
    URNs<-as.data.frame(URNs[-1,])
    toDelete <- seq(2, nrow(URNs), 2)
    URNs<-as.data.frame(URNs[toDelete,])


    # Obtenção das coordenadas geográficas ####

    # Adicionar na linha 84 do script do HTML
    coords<-input %>% html_elements(".col-md-6") %>% html_elements("input") %>% html_attr("value")
    coords2 <- matrix(coords, ncol = 6, byrow = TRUE)
    coords_df <- as.data.frame(coords2)
    coords_df<-coords_df[,-6]
    coords_df<-coords_df[,-5]
    coords_df<-coords_df %>% dplyr::select(V2, V1, V3, V4)
    coords_df<-data.frame(coords_df,URNs,registros)
    colnames(coords_df) <- c("lon","lat","precision","protocol","URNs","Validade")


    # Não triar registros válidos ####

    coords_df_valid <- coords_df


    # Verificação do grau da coordenada ####

    coords_df_valid$Degree_zone <- sub("\\..*", "", coords_df_valid$lon)
    coords_df_valid$Degree_zone <- sub("^$", "EMPTY", coords_df_valid$Degree_zone)

    coords_df_valid$UTMzone <- grepl("-",coords_df_valid$lat)
    coords_df_valid$UTMzone <- sub(TRUE, "S", coords_df_valid$UTMzone)
    coords_df_valid$UTMzone <- sub(FALSE, "N", coords_df_valid$UTMzone)

    coords_df_valid$Degree_zone <- sub("-", "", coords_df_valid$Degree_zone)

    EMPTY_row<-grep("EMPTY", coords_df_valid$Degree_zone)

    coords_df_valid$UTMzone[EMPTY_row]<-""
    coords_df_valid$Degree_zone[EMPTY_row]<-""


    # Verificação da Zona UTM ####

    coords_df_valid_n_row <- as.numeric(1:(length(coords_df_valid$Degree_zone)))


    for(i in coords_df_valid_n_row){

      coords_df_valid$UTMzone[i]<-paste(ceiling((-as.numeric(coords_df_valid$Degree_zone[i])+180)/6),coords_df_valid$UTMzone[i], sep = "")

    }
    coords_df_valid$UTMzone<-sub("NA","",coords_df_valid$UTMzone)


    # Apagamento das coordenadas vazias ####

    coords_df_valid <- coords_df_valid %>% mutate_all(na_if,"")
    for(i in coords_df_valid_n_row){

      if(is.na(coords_df_valid$precision[i])){

        coords_df_valid <- coords_df_valid[-i,]

      }

    }

    output_ <- data.frame(Species = Species, coords_df_valid)

    output <- rbind(output, output_)

  }


  ### List of errors ####

  cat("\014")

  check <- suppressWarnings(leaflet::validateCoords(as.numeric(output$lon), as.numeric(output$lat), warn = TRUE, mode = "point"))

  cat("Longitude errors: ")

  lon_errors <- output[is.na(check$lng),]

  print(

    lon_errors %>% dplyr::select(Species, URNs, Validade, lon, lat, precision, protocol)

  )

  cat("\n\nLatitude errors: ")

  lat_errors <- output[is.na(check$lat),]

  print(

    lat_errors %>% select(Species, URNs, Validade, lon, lat, precision, protocol)

  )

  invisible(return(list(

    "lon_errors" = lon_errors,
    "lat_errors" = lat_errors

    )))

}
