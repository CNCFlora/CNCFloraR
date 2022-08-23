#' Intersection analysis between occurrence records with UCs, PANs and TERs shapefiles
#'
#' Análise de interseção entre os registros de ocorrências e os shapefiles das UCS, PANs e TERs


intersect_occurrenceRecords_with_UCs_PANs_TERs <- function(){

  # Load packages ####

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(dplyr)
        library(sf)
        library(rvest)
        library(tidyr)
        library(googledrive)
        library(purrr)
        library(filesstrings)
        library(gtools)
        library(data.table)

      })
    })
  })


  # List of Species file (species_intersect_UCs_PANs_TERs.csv) ####

  ## Download the list of species file from GoogleDrive ####

  message("Downloading the list of species file from Google Drive API...")

  with_drive_quiet(
    drive_download(

      drive_get("species_intersect_UCs_PANs_TERs.csv"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/listOfSpecies_for_processing/species_intersect_UCs_PANs_TERs.csv"),
      overwrite = TRUE

    )
  )

  message("Download of the list of species file successful.")


  ## Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <- paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/listOfSpecies_for_processing/species_intersect_UCs_PANs_TERs.csv")


  ## Import the list of species file from local path ####

  message("Importing the list of species file from local path...")

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = F,
    sep = ";",
    encoding = "UTF-8"

  )

  message("List of species file imported.")


  # Load shapefiles ####

  message("Loading shapefiles...")

  ## UCs ####

  ## Get local path of UCs shapefile ####

  shapefile_UCs_localPath <- paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.shp")

  ## Import the shapefile of UCs from local path ####

  shapefile_UCs <- read_sf(

    shapefile_UCs_localPath

  )

  shapefile_UCs <- st_make_valid(st_transform(shapefile_UCs, "+proj=eqc +datum=WGS84"))


  ## PANs ####

  ## Get local path of PANs shapefile ####

  shapefile_PANs_localPath <- paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/PANs/PANs.shp")

  ## Import the shapefile of PANs from local path ####

  shapefile_PANs <- read_sf(

    shapefile_PANs_localPath

  )

  shapefile_PANs <- st_make_valid(st_transform(shapefile_PANs, "+proj=eqc +datum=WGS84"))


  ## TERs ####

  ## Get local path of TERs shapefile ####

  shapefile_TERs_localPath <- paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/TERs/TERs.shp")

  ## Import the shapefile of TERs from local path ####

  shapefile_TERs <- read_sf(

    shapefile_TERs_localPath

  )

  shapefile_TERs <- st_make_valid(st_transform(shapefile_TERs, "+proj=eqc +datum=WGS84"))

  message("Shapefiles loaded.")


  # Create empty variables ####

  UCs <- NULL
  PANs <- NULL
  TERs <- NULL


  # Loop start ####

  for(species in listOfSpecies$V1){

    Species <- species

    message(paste0("Analyzing ", Species, " ..."))

    ## Obtain data of occurrence records from old System ####

    input <- get_occurrenceRecords_from_oldSystem(Species)


    ## Info records ####

    STATES <-
      input %>%
      html_nodes("#stateProvince") %>%
      html_attr("value")
    STATES <- str_trim(STATES, side = c("both"))

    URNs <-
      input %>%
      html_nodes("a") %>%
      html_attr("name")
    URNs <- as.data.frame(URNs)
    URNs <- URNs %>% drop_na()
    URNs <- as.data.frame(URNs[-1,])
    URNs_n <- 1:nrow(URNs)
    toKeep <- which(even(URNs_n))
    URNs <- as.data.frame(URNs[toKeep,])

    validation <-
      input %>%
      html_nodes(".label-valid") %>%
      html_text()
    validation <- str_extract(validation, "\\w+")

    validation_SIG <-
      input %>%
      html_nodes(".label-sig") %>%
      html_text()
    validation_SIG <- str_extract(validation_SIG, "\\w+\\s\\w+")

    coords <-
      input %>%
      html_elements(".col-md-6") %>%
      html_elements("input") %>%
      html_attr("value")
    coords2 <- matrix(coords, ncol = 6, byrow = TRUE)
    coords_df <- as.data.frame(coords2)
    coords_df <- coords_df[,-6]
    coords_df <- coords_df[,-5]
    coords_df <- data.frame(STATES, URNs, validation, validation_SIG, coords_df)
    colnames(coords_df) <-
      c("Estado", "URN", "validation", "validation_SIG", "lat", "lon", "precision", "protocol")


    ## Validation of Occurrence records  ####

    ### Possibly Threatened ####

    if(as.character(listOfSpecies[listOfSpecies$V1 == Species, 2]) == "PA"){

      coords_df_valid <-
        coords_df %>%
        dplyr::filter(validation == "Válido" & validation_SIG == "SIG OK")

    }


    ### Possibly Not Threatened ####

    if(as.character(listOfSpecies[listOfSpecies$V1 == Species, 2]) == "PNA"){


      ### Possibly Not Threatened with records NOT validated ####

      if(as.character(listOfSpecies[listOfSpecies$V1 == Species, 3]) == "0"){

        coords_df_valid <- coords_df

      }


      ### Possibly Not Threatened with records validated and SIG revised ####

      if(listOfSpecies[listOfSpecies$V1 == Species, 3] == "x" &
         listOfSpecies[listOfSpecies$V1 == Species, 4] == "x"){

        coords_df_valid <-
          coords_df %>%
          dplyr::filter(validation == "Válido" & validation_SIG == "SIG OK")

      }


      ### Possibly Not Threatened with records validated and SIG NOT revised ####

      if(listOfSpecies[listOfSpecies$V1 == Species, 3] == "x" &
         listOfSpecies[listOfSpecies$V1 == Species, 4] == "0"){

        coords_df_valid <-
          coords_df %>%
          dplyr::filter(validation == "Válido")

      }

    }

    coords_df_valid$lat <- sub("^$", "EMPTY", coords_df_valid$lat)
    EMPTY_row <- grep("EMPTY", coords_df_valid$lat)

    if(length(EMPTY_row) == 0){} else{

      coords_df_valid <- coords_df_valid[-EMPTY_row,]
      URNs <- URNs[-EMPTY_row,]

    }

    coords_df_valid_for_UCs <-
      coords_df_valid %>%
      dplyr::filter (!str_detect(precision, "município|municipio|Município|Municipio"))

    dots <- data.frame(

      longitude = coords_df_valid$lon,
      latitude = coords_df_valid$lat,
      URNs = coords_df_valid$URN,
      STATES = coords_df_valid$Estado

    )

    dots_UCs <- data.frame(

      longitude = coords_df_valid_for_UCs$lon,
      latitude = coords_df_valid_for_UCs$lat,
      URNs = coords_df_valid_for_UCs$URN,
      STATES = coords_df_valid_for_UCs$Estado

    )

    sites <- st_as_sf(

      dots,
      coords = c("longitude", "latitude"),
      crs = 4326,
      agr = "constant"

    )
    sites <- st_make_valid(

      st_transform(sites, "+proj=eqc +datum=WGS84")

    )

    sites_for_UCs <- st_as_sf(

      dots,
      coords = c("longitude", "latitude"),
      crs = 4326,
      agr = "constant"

    )
    sites_for_UCs <- st_make_valid(

      st_transform(sites_for_UCs, "+proj=eqc +datum=WGS84")

    )


    ## UCs ####

    UCs_out <- st_intersection(sites_for_UCs, shapefile_UCs)
    UCs_out <- as.data.frame(UCs_out)

    if(as.numeric(nrow(UCs_out)) > 0){

      UCs_out <- data.frame(

        nome_aceit = Species,
        id_da_ocor = UCs_out$URN,
        Nome_Uc1 = UCs_out$NOME_UC1

      )
      UCs <- rbind(UCs, UCs_out)

    }


    ## PANs ####

    PANs_out <- st_intersection(sites, shapefile_PANs)
    PANs_out <- as.data.frame(PANs_out)

    if(as.numeric(nrow(PANs_out))>0){

      PANs_out <- data.frame(

        nome_aceit = Species,
        id_da_ocor = PANs_out$URNs,
        PAN = PANs_out$PAN

      )
      PANs <- rbind(PANs, PANs_out)

    }


    ## TERs ####

    TERs_out <- st_intersection(sites, shapefile_TERs)
    TERs_out <- as.data.frame(TERs_out)

    if(as.numeric(nrow(TERs_out)) > 0){

      TERs_out <- data.frame(

        species = Species,
        id_da_ocor = TERs_out$URNs,
        territorio = TERs_out$territorio,
        Nome_compl = TERs_out$Nome_compl,
        state = TERs_out$STATES

      )
      TERs <- rbind(TERs, TERs_out)

    }

  }

  message("All done!")

  ## Export files ####

  write.csv(

    PANs,
    paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/outputs/intersect_analyses/PANs/PANs_from_intersection.csv")

  )

  write.csv(

    TERs,
    paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/outputs/intersect_analyses/TERs/TERs_from_intersection.csv")

  )

  write.csv(

    UCs,
    paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/outputs/intersect_analyses/UCs/UCs_from_intersection.csv")

  )

  message("Files generated.")

}

