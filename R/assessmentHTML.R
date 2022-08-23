#' Generate the HTML panel of assessment
#'
#' Gera o HTML do painel de avaliação


assessmentHTML <- function(){

  OverlayAnalyses_ID <- readline("ID of overlay analyses: ")

  message("Starting...")

  # Load packages ####

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(data.table)
        library(dismo)
        library(sp)
        library(trendsegmentR)
        library(basicTrendline)
        library(htmlwidgets)
        library(rvest)
        library(leaflet)
        library(sf)
        library(readtext)
        library(dplyr)
        library(stringr)
        library(stringi)
        library(tidyr)
        library(serpstatr)
        library(googledrive)
        library(readr)
        library(knitr)
        library(kableExtra)
        library(raster)
        library(knitr)
        library(ggplot2)
        library(ggrepel)
        library(forcats)

      })
    })
  })

  message("Downloading the list of species file from Google Drive...")

  # Load list of species file (species_assessment.csv) ####

  ## Download the list of species file from GoogleDrive ####

  with_drive_quiet(
    drive_download(

      drive_get(id = "1TBum3WAeEM2aMPiUrG3t765DH1FLQvbd"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/listOfSpecies_for_processing/species_assessment.csv"),
      overwrite = TRUE

    )
  )

  message("Download the list of species file from Google Drive successful.")


  ## Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <- paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/listOfSpecies_for_processing/species_assessment.csv")


  ## Import the list of species file from local path ####

  message("Importing the list of species file from local path...")

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = F,
    sep = ",",
    encoding = "UTF-8"

  )

  message("Import list of species file from local path successful.")

  # Load shapefiles ####

  message("Loading shapefiles...")

  ## UCs ####

  ### Download shapefile of UCs from Google Drive ####

  getShapefileUCs_from_GoogleDrive()


  ### Import shapefile of UCs from local path ####

  message("Importing shapefile of UCs from local path...")

  shapefile_UC <-
    read_sf(

    paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.shp")

  )

  shapefile_UC <-
    st_make_valid(

      st_transform(

        shapefile_UC,
        "+proj=eqc +datum=WGS84"

      )

    )

  UCs_shape <-
    st_read(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "CNCFlora_data/inputs/shapefiles/UCs/UCs.shp"

      ),
      quiet = T

    ) %>%
    st_transform(4326)

  UCs_shape <- st_make_valid(UCs_shape)


  #### Adjust names of UCs ####

  message("Fixing UCs names of the shapefile...")

  UCs_shape <- fixUCsNames(UCs_shape)


  # Load overlay analyses ####

  ## Check files of overlay analyses (by output ID) ####

  message("Checking if all files of overlay analyses are found...")

  OverlayAnalysesOutputs_byOuputID <- checkOverlayAnalysesOutputs(OverlayAnalyses_ID)

  if(OverlayAnalysesOutputs_byOuputID == F){

    message("Aborting... not all files of overlay analyses were found.")

    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    break
    opt <- options(show.error.messages = TRUE)
    on.exit(options(opt))

  }

  message("All files of overlay analyses were found.")


  ## Check files of overlay analyses (by species) ####

  message("Checking if all species are found in the overlay analyses files...")

  OverlayAnalysesOutputs_bySpecies <- check_if_all_species_are_in_OverlayAnalysesOutputs(listOfSpecies$V1, OverlayAnalyses_ID)

  if(OverlayAnalysesOutputs_bySpecies == F){

    message("Aborting... not all species are in the overlay analyses files.")

    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    break
    opt <- options(show.error.messages = TRUE)
    on.exit(options(opt))

  }

  message("All species were found in the overlay analyses files.")


  ## Import overlay analyses ####

  message("Importing overlay analyses files from local path...")

  options("encoding" = "UTF-8")


  ### TodosOsAnos_output ####

  MapBiomas_coverLand_results <-
    fread(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "CNCFlora_data/outputs/overlay_analyses/",
        "SIG_land_use_TodosOsAnos_output(", OverlayAnalyses_ID, ").csv"

      ),
      header = T
      #, encoding = "UTF-8"

    )


  ### TodosOsAnos_AOOinEOObuffer_output ####

  MapBiomas_coverLand_AOOinEOObuffer_results <-
    fread(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "CNCFlora_data/outputs/overlay_analyses/",
        "SIG_land_use_TodosOsAnos_AOOinEOObuffer_output(", OverlayAnalyses_ID, ").csv"

      ),
      header = T
      #, encoding = "UTF-8"
    )


  ### TodosOsAnos_AOObyQuadOfGrid_output ####

  AOObyQuadOfGrid_results <-
    fread(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "CNCFlora_data/outputs/overlay_analyses/",
        "SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output(", OverlayAnalyses_ID, ").csv"

      ),
      header = T
      #, encoding = "UTF-8"

    )

  message("Import overlay analyses files from local path successful.")


  # Loop Start ####

  for(

    i in listOfSpecies[1, 1]

  ){

    ## Clean all objects of workspace ####

    rm(

      list = setdiff(

        ls(),
        c(

          "OverlayAnalyses_ID",
          "shapefile_UC",
          "UCs_shape",
          "MapBiomas_coverLand_results",
          "MapBiomas_coverLand_AOOinEOObuffer_results",
          "i",
          "listOfSpecies",
          "j",
          "output",
          "cond_addPolygons",
          "cond_row_spec"

        )

      )

    )


    ## Create a variable with the name of species ####

    SPECIES <- i


    ## Load the filled profile of species ####

    ### Get the local path of the filled profile of species ####

    filledSpeciesProfile <-
      paste0(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/speciesProfile_filled/oldSystem/"

        ),
        SPECIES,
        ".html"

      )


    ## Load the occurrence records of the species ####

    ### Get the local path of the occurrence records of species from old System ####

    occurrenceRecords <-
      paste0(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/occurrences/oldSystem/"

        ),
        SPECIES,
        ".html"

      )


    ### Load the occurrence records of species from old system ####

    occurrenceRecords <- read_html(occurrenceRecords)


    ### Scrap the collection years of occurrence records from old system ####

    collectionYear <- yearOfCollections_from_oldSystem(occurrenceRecords)


    ### Get the number of occurrence records from old system ####

    collectionsNumber <- howManyCollections_from_oldSystem(occurrenceRecords)


    ## Get data of the species profile ####

    ### Scrap data of the species profile from old system ####

    speciesProfileEnviron <- scrap_data_of_filledSpeciesProfile_from_oldSystem(filledSpeciesProfile)


    ### Creating objects of species profile from 'speciesProfileEnviron' ####

    habit <- speciesProfileEnviron$habit

    endemicToBrazil <- speciesProfileEnviron$endemicToBrazil

    EOO <- speciesProfileEnviron$EOO
    AOO <- speciesProfileEnviron$AOO

    biomes <- speciesProfileEnviron$biomes
    biomes_n <- speciesProfileEnviron$biomes_n

    vegetations <- speciesProfileEnviron$vegetations

    distribution_in_BrazilianMunicipalities_n <- speciesProfileEnviron$distribution_in_BrazilianMunicipalities_n
    distribution_in_BrazilianStates <- speciesProfileEnviron$distribution_in_BrazilianStates
    distribution_in_BrazilianStates_n <- speciesProfileEnviron$distribution_in_BrazilianStates_n
    distribution_in_BrazilianStatesAndMunicipalities <- speciesProfileEnviron$distribution_in_BrazilianStatesAndMunicipalities

    UCs <- speciesProfileEnviron$UCs


    if(

      identical(UCs[1,], "character(0)") == T

    ){

      UCs_n <- 0

    } else {

      UCs_n <- length(UCs[1,])

    }

    Texto <- paste0(

      habit,
      "; ",
      "com ocorrência",
      if(

        biomes_n > 1

      ){

        " nos biomas "

      } else {

        " no bioma "

      },
      stri_replace_last(capture.output(cat(biomes, sep = ", ")), fixed = ", ", " e"),
      ", em ",
      stri_replace_last(capture.output(cat(vegetations, sep = ", ")), fixed = ", ", " e"),
      if(

        endemicToBrazil == T

      ){

        paste0(

          "; endêmica do Brasil, com distribuição ",
          if(

            distribution_in_BrazilianMunicipalities_n > 8

          ){

            if(

              distribution_in_BrazilianStates_n > 6

            ){

              "em diversos Estados"

            } else {

              if(distribution_in_BrazilianStates[1] == "Acre"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Alagoas"){distribution_in_BrazilianStates[1] <- paste0("de ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Amapá"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Amazonas"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Bahia"){distribution_in_BrazilianStates[1] <- paste0("da ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Ceará"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Distrito Federal"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Espírito Santo"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Goiás"){distribution_in_BrazilianStates[1] <- paste0("de ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Maranhão"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Mato Grosso do Sul"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Mato Grosso"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Minas Gerais"){distribution_in_BrazilianStates[1] <- paste0("de ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Pará"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Paraíba"){distribution_in_BrazilianStates[1] <- paste0("da ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Paraná"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Pernambuco"){distribution_in_BrazilianStates[1] <- paste0("de ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Piauí"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Rio de Janeiro"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Rio Grande do Norte"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Rio Grande do Sul"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Rondônia"){distribution_in_BrazilianStates[1] <- paste0("de ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Roraima"){distribution_in_BrazilianStates[1] <- paste0("de ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "São Paulo"){distribution_in_BrazilianStates[1] <- paste0("de ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Sergipe"){distribution_in_BrazilianStates[1] <- paste0("de ", distribution_in_BrazilianStates[1])}
              if(distribution_in_BrazilianStates[1] == "Tocantins"){distribution_in_BrazilianStates[1] <- paste0("do ", distribution_in_BrazilianStates[1])}

              paste0("em diversos municípios dos Estados ",
                     stri_replace_last(capture.output(cat(distribution_in_BrazilianStates, sep = ", ")), fixed = ", ", " e")
              )

            }

          } else {

            if(

              distribution_in_BrazilianMunicipalities_n == 1

            ){

              paste0(

                "no município ",
                distribution_in_BrazilianStatesAndMunicipalities

              )

            } else {

              paste0(

                "nos municípios ",
                stri_replace_last(

                  capture.output(

                    cat(

                      distribution_in_BrazilianStatesAndMunicipalities,
                      sep = ", "

                    )

                  ),
                  fixed = ", ",
                  " e "

                )

              )

            }

          }
        )

      } else {

        if(

          distribution_out_of_Brazil == "A espécie não é endêmica do Brasil"

        ){

          distribution_out_of_Brazil <- sub(

            "A espécie não é endêmica do Brasil",
            "fora do Brasil",
            distribution_out_of_Brazil

          )

        }

        paste0(

          "; com distribuição ",
          distribution_out_of_Brazil,
          ", inclusive no Brasil, onde é registrada ",

          if(

            distribution_in_BrazilianMunicipalities_n > 8

          ){

            if(

              distribution_in_BrazilianStates_n > 6

            ){

              "em diversos Estados"

            } else {

              paste0(

                "em diversos municípios dos Estados ",
                stri_replace_last(

                  capture.output(

                    cat(

                      distribution_in_BrazilianStates,
                      sep = ", "

                    )

                  ),
                  fixed = ", ",
                  " e"

                )

              )

            }

          } else {

            paste0(

              "nos municípios ",
              stri_replace_last(

                capture.output(

                  cat(

                    distribution_in_BrazilianStatesAndMunicipalities, sep = ", "

                  )

                ),
                fixed = ", ",
                " e"

              )

            )

          }
        )
      },
      "; representada por ",
      collectionsNumber,
      " coletas ",
      if(

        collectionsNumber > 8

      ){

        paste0(

          "entre os anos de ",
          min(collectionYear),
          " e ",
          max(collectionYear)

        )

      } else {

        paste0(

          "nos anos de ",
          stri_replace_last(

            capture.output(

              cat(

                collectionYear,
                sep = ", "

              )

            ),
            fixed = ", ",
            " e "

          )

        )

      },
      "; e sem dados populacionais disponíveis. ",
      "Considerando: "
    )

    if(

      AOO == EOO

    ){

      AOO_EOO <- paste0(

        "seu valor de AOO e EOO igual a ",
        AOO,
        " km²"

      )

    } else {

      AOO_EOO <- paste0(

        "seu valor de AOO igual a ",
        AOO,
        " km² e de EOO igual a ",
        EOO,
        " km²"

      )

    }

    if(

      UCs_n > 5

    ){

      UCs_text <-
        "sua ocorrência em diversas Unidades de Conservação"

    } else {

      if(

        UCs_n <= 5 &
        UCs_n > 0 &
        UCs_n != 1

      ){

        UCs_text <-
          "sua ocorrência em Unidades de Conservação"

      } else {

        if(

          UCs_n == 1

        ){

          UCs_text <-
            "sua ocorrência em Unidade de Conservação"

        } else {

          if(

            UCs_n == 0

          ){

            UCs_text <-
              "que não é registrada em Unidades de Conservação"

          }

        }

      }

    }

    if(

      collectionsNumber > 8

    ){

      yearsElapsedSinceLastCollections <-
        paste0(

          "que suas coletas foram realizadas entre ca. ",
          as.numeric(

            sub(

              "\\-.*",
              "",
              Sys.Date()

            )

          ) - min(collectionYear),
          " e ",
          as.numeric(

            sub(

              "\\-.*",
              "",
              Sys.Date()

            )

          ) - max(collectionYear),
          " atrás"

        )

    } else {

      if(

        length(collectionYear) == 1

      ){

        yearsElapsedSinceLastCollections <-
          paste0(

            "que sua última coleta foi realizada há ",
            stri_replace_last(

              capture.output(

                cat(

                  as.numeric(

                    sub(

                      "\\-.*",
                      "",
                      Sys.Date()

                    )

                  ) - collectionYear,
                  sep = ", "

                )

              ),
              fixed = ", ",
              " e"

            ),
            " anos"

          )

      } else {

        yearsElapsedSinceLastCollections <-
          paste0(

            "que suas coletas foram realizadas há ",
            stri_replace_last(

              capture.output(

                cat(

                  as.numeric(

                    sub(

                      "\\-.*",
                      "",
                      Sys.Date()

                    )

                  ) - collectionYear,
                  sep = ", "

                )

              ),
              fixed = ", ",
              " e"

            ),
            " anos"

          )

      }

    }

    considerations <-
      data.frame(

        AOO_EOO = AOO_EOO,
        UCs = UCs_text,
        yearsElapsedSinceLastCollections = yearsElapsedSinceLastCollections

      )

    if(

      EOO < 100

    ){

      EOO_category <- "CR"

    } else {

      if(

        EOO < 5000

      ){

        EOO_category <- "EN"

      } else {

        if(

          EOO < 20000

        ){

          EOO_category <- "VU"

        } else {

          EOO_category <- "Não ameaçada"

        }

      }

    }

    if(

      AOO < 10

    ){

      AOO_category <- "CR"

    } else {

      if(

        AOO < 500

      ){

        AOO_category <- "EN"

      } else {

        if(

          AOO < 2000

        ){

          AOO_category <- "VU"

        } else {

          EOO_category <- "Não ameaçada"

        }

      }

    }

    if(

      AOO_category == "Não ameaçada" &
      EOO_category == "Não ameaçada"

    ){

      Category <- "LC"

    } else {

      if(

        AOO_category != "Não ameaçada"

      ){

        Category_AOO <- paste("AOO:", AOO_category)

      }

      if(

        EOO_category != "Não ameaçada"

      ){

        Category_EOO <- paste("EOO:", EOO_category)

      }

    }

    AOO_EOO_dt <- t(data.frame(EOO, AOO))

    Category <- t(data.frame(EOO_category, AOO_category))

    AOO_EOO_dt <- data.frame(Valor = AOO_EOO_dt, Category)

    if(

      AOO_category == "CR"

    ){

      AOOthreatned_rowCR <- 2

    } else {

      AOOthreatned_rowCR <-0

    }

    if(

      AOO_category == "EN"

    ){

      AOOthreatned_rowEN <- 2

    } else {

      AOOthreatned_rowEN <- 0

    }

    if(

      AOO_category == "VU"

    ){

      AOOthreatned_rowVU <- 2

    } else {

      AOOthreatned_rowVU <- 0

    }

    if(

      EOO_category == "CR"

    ){

      EOOthreatned_rowCR <- 1

    } else {

      EOOthreatned_rowCR <- 0

    }

    if(

      EOO_category == "EN"

    ){

      EOOthreatned_rowEN <- 1

    } else {

      EOOthreatned_rowEN <- 0

    }

    if(

      EOO_category == "VU"

    ){

      EOOthreatned_rowVU <- 1

    } else {

      EOOthreatned_rowVU <- 0

    }

    threatned_row <- data.frame(

      Category = c("CR", "EN", "VU"),

      AOOthreatned_row = c(

        AOOthreatned_rowCR,

        AOOthreatned_rowEN,

        AOOthreatned_rowVU

      ),

      EOOthreatned_row = c(

        EOOthreatned_rowCR,

        EOOthreatned_rowEN,

        EOOthreatned_rowVU

      )

    )

    if(

      threatned_row[1,2] == 2 |
      threatned_row[1,3] == 1

    ){

      if(

        threatned_row[1,2] == 2 &&
        threatned_row[1,3] == 1

      ){

        threatned_rowCR <- c(1, 2)

      } else {

        if(

          threatned_row[1,2] == 0 &&
          threatned_row[1,3] == 1

        ){

          threatned_rowCR <- 1

        } else {

          if(

            threatned_row[1,2] == 2 &&
            threatned_row[1,3] == 0

          ){

            threatned_rowCR <- 2

          }

        }

      }

    }

    if(

      threatned_row[2,2] == 2 |
      threatned_row[2,3] == 1

    ){

      if(

        threatned_row[2,2] == 2 &&
        threatned_row[2,3] == 1

      ){

        threatned_rowEN <- c(1, 2)

      } else {

        if(

          threatned_row[2,2] == 0 &&
          threatned_row[2,3] == 1

        ){

          threatned_rowEN <- 1

        } else {

          if(

            threatned_row[2,2] == 2 &&
            threatned_row[2,3] == 0

          ){

            threatned_rowEN <- 2

          }

        }

      }

    }

    if(

      threatned_row[3,2] == 2 |
      threatned_row[3,3] == 1

    ){

      if(

        threatned_row[3,2] == 2 &&
        threatned_row[3,3] == 1

      ){

        threatned_rowVU <- c(1, 2)

      } else {

        if(

          threatned_row[3,2] == 0 &&
          threatned_row[3,3] == 1

        ){

          threatned_rowVU <- 1

        } else {

          if(

            threatned_row[3,2] == 2 &&
            threatned_row[3,3] == 0

          ){

            threatned_rowVU <- 2

          }

        }

      }

    }

    sp_in_MapBiomas_coverLand <-
      MapBiomas_coverLand_results %>%
      dplyr::filter(Species == i)

    sp_in_MapBiomas_coverLand <- sp_in_MapBiomas_coverLand %>%
      dplyr::filter(Ameaca %in%
                      c(

                        "Mosaico Agricultura e Pastagem",
                        "Silvicultura",
                        "Pastagem",
                        "Cana-de-açúcar",
                        "Infraestrutura Urbana",
                        "Mineração",
                        "Lavouras perenes",
                        "Soja",
                        "Outras lavouras temporárias",
                        "Outras Lavouras Perenes",
                        "Citrus",
                        "Arroz",
                        "Café",
                        "Total"

                      )

      )

    if(

      nrow(sp_in_MapBiomas_coverLand) > 0

    ) {

      sp_in_MapBiomas_coverLand <- sp_in_MapBiomas_coverLand %>%
        dplyr::select(

          Ameaca,
          AOO_util_km2,
          Area_AOO_sites_km2_2020,
          Porcentagem_AOO_2020,
          Area_EOO_km2,
          Area_EOO_sites_km2_2020,
          Porcentagem_EOO_2020

        )

      sp_in_MapBiomas_coverLand$Area_AOO_sites_km2_2020 <-
        round(

          as.numeric(

            sp_in_MapBiomas_coverLand$Area_AOO_sites_km2_2020

          ),
          2

        )

      sp_in_MapBiomas_coverLand$Area_EOO_sites_km2_2020 <-
        round(

          as.numeric(

            sp_in_MapBiomas_coverLand$Area_EOO_sites_km2_2020

          ),
          2

        )

      sp_in_MapBiomas_coverLand$AOO_util_km2[1:length(sp_in_MapBiomas_coverLand$AOO_util_km2)-1] <- ""

      sp_in_MapBiomas_coverLand$Area_EOO_km2 <-
        round(

          as.numeric(sp_in_MapBiomas_coverLand$Area_EOO_km2),
          2

        )

      sp_in_MapBiomas_coverLand$Area_EOO_km2[1:length(sp_in_MapBiomas_coverLand$Area_EOO_km2)-1] <- ""

      dataPie <-
        sp_in_MapBiomas_coverLand %>%
        dplyr::filter(Ameaca != "Total")


      # Pizza da proporção de ameaças na AOO

      dataPieAOO <- dataPie %>%

        dplyr::select(

          Ameaca,
          Area_AOO_sites_km2_2020,
          Porcentagem_AOO_2020

        )

      dataPieAOO <- dataPieAOO %>%

        dplyr::filter(Porcentagem_AOO_2020 != 0)

      dataPieAOO <- rbind(

        data.frame(

          Ameaca = "Sem ameaça",
          Area_AOO_sites_km2_2020 = "",
          Porcentagem_AOO_2020 = 100 - sum(dataPieAOO$Porcentagem_AOO_2020)

        ),
        dataPieAOO

      )

      dataPieAOOCores <- dataPieAOO$Ameaca
      dataPieAOOCores <- sub("Sem ameaça", "white", dataPieAOOCores)
      dataPieAOOCores <- sub("Mosaico Agricultura e Pastagem", "#fff3bf", dataPieAOOCores)
      dataPieAOOCores <- sub("Silvicultura", "#ad4413", dataPieAOOCores)
      dataPieAOOCores <- sub("Pastagem", "#FFD966", dataPieAOOCores)
      dataPieAOOCores <- sub("Cana-de-açúcar", "#C27BA0", dataPieAOOCores)
      dataPieAOOCores <- sub("Infraestrutura Urbana", "#aa0000", dataPieAOOCores)
      dataPieAOOCores <- sub("Mineração", "#af2a2a", dataPieAOOCores)
      dataPieAOOCores <- sub("Lavouras perenes", "#f3b4f1", dataPieAOOCores)
      dataPieAOOCores <- sub("Soja", "#e075ad", dataPieAOOCores)
      dataPieAOOCores <- sub("Outras lavouras temporárias", "#e787f8", dataPieAOOCores)
      dataPieAOOCores <- sub("Citrus", "#d082de", dataPieAOOCores)
      dataPieAOOCores <- sub("Arroz", "#982c9e", dataPieAOOCores)
      dataPieAOOCores <- sub("Café", "#cca0d4", dataPieAOOCores)
      dataPieAOOCores <- sub("Outras Lavouras Perenes", "#cd49e4", dataPieAOOCores)

      cols_AOO <- dataPieAOOCores

      names(cols_AOO) <- dataPieAOO$Ameaca

      dataPie_AOO2 <- dataPieAOO %>%

        mutate(

          csum = rev(cumsum(rev(Porcentagem_AOO_2020))),
          pos = Porcentagem_AOO_2020 / 2 + lead(csum, 1),
          pos = if_else(is.na(pos), Porcentagem_AOO_2020 / 2, pos)

        )

      ggplot(

        dataPieAOO,
        aes(

          x = "" , y = Porcentagem_AOO_2020, fill = fct_inorder(Ameaca)

        )

      ) +
        geom_col(width = 1, color = 1) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols_AOO) +
        geom_label_repel(

          data = dataPie_AOO2,
          aes(y = pos, label = paste0(Porcentagem_AOO_2020, "%")),
          size = 4.5,
          nudge_x = 1,
          show.legend = FALSE

        ) +
        guides(fill = guide_legend(title = "Usos alternativos do solo")) +
        theme_void()

      ggsave(paste0("PieAOO-", i, ".png"), width=6, height=5)


      # Pizza da proporção de ameaças nas quadrículas de AOO localizadas no buffer da EOO

      sp_in_MapBiomas_coverLand_AOOinEOObuffer_results <-
        MapBiomas_coverLand_AOOinEOObuffer_results %>%
        dplyr::filter(Species == i)

      if(

        nrow(sp_in_MapBiomas_coverLand_AOOinEOObuffer_results) > 0

      ){

        sp_in_MapBiomas_coverLand_AOOinEOObuffer_results <-
          sp_in_MapBiomas_coverLand_AOOinEOObuffer_results %>%
          dplyr::select(

            Ameaca,
            Area_AOO_sites_km2_2020,
            Porcentagem_AOO_2020

          )

        sp_in_MapBiomas_coverLand_AOOinEOObuffer_results <-
          sp_in_MapBiomas_coverLand_AOOinEOObuffer_results %>%
          dplyr::filter(

            Ameaca %in%

              c(

                "Mosaico Agricultura e Pastagem",
                "Silvicultura",
                "Pastagem",
                "Cana-de-açúcar",
                "Infraestrutura Urbana",
                "Mineração",
                "Lavouras perenes",
                "Soja",
                "Outras lavouras temporárias",
                "Total"

              )

          )

        sp_in_MapBiomas_coverLand_AOOinEOObuffer_results$Area_AOO_sites_km2_2020 <-
          round(

            sp_in_MapBiomas_coverLand_AOOinEOObuffer_results$Area_AOO_sites_km2_2020,
            2

          )

        dataPie_AOOinEOObuffer <-
          sp_in_MapBiomas_coverLand_AOOinEOObuffer_results %>%
          dplyr::filter(Ameaca != "Total")

        dataPie_AOOinEOObuffer <- dataPie_AOOinEOObuffer %>%
          dplyr::filter(Porcentagem_AOO_2020 != 0)

        dataPie_AOOinEOObuffer <- rbind(

          data.frame(

            Ameaca = "Sem ameaça",
            Area_AOO_sites_km2_2020 = "",
            Porcentagem_AOO_2020 = 100 - sum(dataPie_AOOinEOObuffer$Porcentagem_AOO_2020)

          ),

          dataPie_AOOinEOObuffer

        )

        dataPie_AOOinEOObufferCores <- dataPie_AOOinEOObuffer$Ameaca
        dataPie_AOOinEOObufferCores <- sub("Sem ameaça", "white", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Mosaico Agricultura e Pastagem", "#fff3bf", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Silvicultura", "#ad4413", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Pastagem", "#FFD966", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Cana-de-açúcar", "#C27BA0", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Infraestrutura Urbana", "#aa0000", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Mineração", "#af2a2a", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Lavouras perenes", "#f3b4f1", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Soja", "#e075ad", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Outras lavouras temporárias", "#e787f8", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Citrus", "#d082de", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Arroz", "#982c9e", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Café", "#cca0d4", dataPie_AOOinEOObufferCores)
        dataPie_AOOinEOObufferCores <- sub("Outras Lavouras Perenes", "#cd49e4", dataPie_AOOinEOObufferCores)


        cols_AOOinEOObuffer <- dataPie_AOOinEOObufferCores

        names(cols_AOOinEOObuffer) <-
          dataPie_AOOinEOObuffer$Ameaca

        dataPie_AOOinEOObuffer2 <-
          dataPie_AOOinEOObuffer %>%
          mutate(

            csum = rev(cumsum(rev(Porcentagem_AOO_2020))),
            pos = Porcentagem_AOO_2020/2 + lead(csum, 1),
            pos = if_else(is.na(pos), Porcentagem_AOO_2020/2, pos)

          )

        ggplot(

          dataPie_AOOinEOObuffer,
          aes(

            x = "" ,
            y = Porcentagem_AOO_2020,
            fill = fct_inorder(Ameaca)

          )

        ) +
          geom_col(width = 1, color = 1) +
          coord_polar(theta = "y") +
          scale_fill_manual(values = cols_AOOinEOObuffer) +
          geom_label_repel(

            data = dataPie_AOOinEOObuffer2,
            aes(

              y = pos,
              label = paste0(Porcentagem_AOO_2020, "%")

            ),
            size = 4.5, nudge_x = 1, show.legend = FALSE

          ) +
          guides(fill = guide_legend(title = "Ameaca")) +
          theme_void()

        ggsave(paste0("PieAOOinEOObuffer-", i, ".png"))

      }

      if(

        is.na(dataPie$Area_EOO_sites_km2[1]) == T

      ){

      } else {

        if(

          dataPie$Area_EOO_sites_km2[1] != "Análise dispensada"

        ){
          # Pizza da proporção de ameaças na EOO

          dataPieEOO <- dataPie %>%
            dplyr::select(

              Ameaca,
              Area_EOO_sites_km2_2020,
              Porcentagem_EOO_2020

            )

          dataPieEOO <- dataPieEOO %>%
            dplyr::filter(Porcentagem_EOO_2020 != 0)

          dataPieEOO <- rbind(
            data.frame(

              Ameaca = "Sem ameaça",
              Area_EOO_sites_km2_2020 = "",
              Porcentagem_EOO_2020 = 100-sum(dataPieEOO$Porcentagem_EOO_2020)

            ),
            dataPieEOO

          )

          dataPieEOOCores <- dataPieEOO$Ameaca
          dataPieEOOCores <- sub("Sem ameaça", "white", dataPieEOOCores)
          dataPieEOOCores <- sub("Mosaico Agricultura e Pastagem", "#fff3bf", dataPieEOOCores)
          dataPieEOOCores <- sub("Silvicultura", "#ad4413", dataPieEOOCores)
          dataPieEOOCores <- sub("Pastagem", "#FFD966", dataPieEOOCores)
          dataPieEOOCores <- sub("Cana-de-açúcar", "#C27BA0", dataPieEOOCores)
          dataPieEOOCores <- sub("Infraestrutura Urbana", "#aa0000", dataPieEOOCores)
          dataPieEOOCores <- sub("Mineração", "#af2a2a", dataPieEOOCores)
          dataPieEOOCores <- sub("Lavouras perenes", "#f3b4f1", dataPieEOOCores)
          dataPieEOOCores <- sub("Soja", "#e075ad", dataPieEOOCores)
          dataPieEOOCores <- sub("Outras lavouras temporárias", "#e787f8", dataPieEOOCores)
          dataPieEOOCores <- sub("Citrus", "#d082de", dataPieEOOCores)
          dataPieEOOCores <- sub("Arroz", "#982c9e", dataPieEOOCores)
          dataPieEOOCores <- sub("Café", "#cca0d4", dataPieEOOCores)
          dataPieEOOCores <- sub("Outras Lavouras Perenes", "#cd49e4", dataPieEOOCores)

          cols_EOO <- dataPieEOOCores

          names(cols_EOO) <- dataPieEOO$Ameaca

          dataPie_EOO2 <- dataPieEOO %>%
            mutate(

              csum = rev(cumsum(rev(Porcentagem_EOO_2020))),
              pos = Porcentagem_EOO_2020 / 2 + lead(csum, 1),
              pos = if_else(is.na(pos), Porcentagem_EOO_2020 / 2, pos)

            )

          ggplot(

            dataPieEOO,
            aes(

              x = "" ,
              y = Porcentagem_EOO_2020,
              fill = fct_inorder(Ameaca)

            )

          ) +
            geom_col(width = 1, color = 1) +
            coord_polar(theta = "y") +
            scale_fill_manual(values = cols_EOO) +
            geom_label_repel(

              data = dataPie_EOO2,
              aes(y = pos, label = paste0(Porcentagem_EOO_2020, "%")),
              size = 4.5,
              nudge_x = 2,
              show.legend = FALSE

            ) +
            guides(fill = guide_legend(title = "Usos alternativos do solo")) +
            theme_void()

          ggsave(paste0("PieEOO-", i, ".png"), width=6, height=5)

        }

      }

    }


    # Gráfico de área empilhada: AOO ####

    input_StackedAreaChart <- MapBiomas_coverLand_results %>%
      dplyr::filter(Species == SPECIES) %>%
      dplyr::filter(Ameaca != "Total") %>%
      dplyr::select(Ameaca, contains("Porcentagem_AOO"))

    ### Floresta
    input_StackedAreaChart_floresta <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Floresta")
    input_StackedAreaChart_floresta <- t(input_StackedAreaChart_floresta)
    input_StackedAreaChart_floresta <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_floresta)[2:37]), Porc = as.numeric(input_StackedAreaChart_floresta[2:37]), Ameaca = "Floresta")

    ### Pastagem
    input_StackedAreaChart_pastagem <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Pastagem")
    input_StackedAreaChart_pastagem <- t(input_StackedAreaChart_pastagem)
    input_StackedAreaChart_pastagem <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_pastagem)[2:37]), Porc = as.numeric(input_StackedAreaChart_pastagem[2:37]), Ameaca = "Pastagem")

    ### Mineração
    input_StackedAreaChart_mineracao <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Mineração")
    input_StackedAreaChart_mineracao <- t(input_StackedAreaChart_mineracao)
    input_StackedAreaChart_mineracao <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_mineracao)[2:37]), Porc = as.numeric(input_StackedAreaChart_mineracao[2:37]), Ameaca = "Mineração")

    ### Soja
    input_StackedAreaChart_soja <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Soja")
    input_StackedAreaChart_soja <- t(input_StackedAreaChart_soja)
    input_StackedAreaChart_soja <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_soja)[2:37]), Porc = as.numeric(input_StackedAreaChart_soja[2:37]), Ameaca = "Soja")

    ### Cana-de-açúcar
    input_StackedAreaChart_cana <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Cana-de-açúcar")
    input_StackedAreaChart_cana <- t(input_StackedAreaChart_cana)
    input_StackedAreaChart_cana <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_cana)[2:37]), Porc = as.numeric(input_StackedAreaChart_cana[2:37]), Ameaca = "Cana-de-açúcar")

    ### Lavouras perenes
    input_StackedAreaChart_lavouras_perenes <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Lavouras perenes")
    input_StackedAreaChart_lavouras_perenes <- t(input_StackedAreaChart_lavouras_perenes)
    input_StackedAreaChart_lavouras_perenes <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_lavouras_perenes)[2:37]), Porc = as.numeric(input_StackedAreaChart_lavouras_perenes[2:37]), Ameaca = "Lavouras perenes")

    ### Outras lavouras temporárias
    input_StackedAreaChart_outras_lavouras_temporarias <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Outras lavouras temporárias")
    input_StackedAreaChart_outras_lavouras_temporarias <- t(input_StackedAreaChart_outras_lavouras_temporarias)
    input_StackedAreaChart_outras_lavouras_temporarias <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_outras_lavouras_temporarias)[2:37]), Porc = as.numeric(input_StackedAreaChart_outras_lavouras_temporarias[2:37]), Ameaca = "Outras lavouras temporárias")

    ### Mosaico Agricultura e Pastagem
    input_StackedAreaChart_mosaico_agricultura_pastagem <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Mosaico Agricultura e Pastagem")
    input_StackedAreaChart_mosaico_agricultura_pastagem <- t(input_StackedAreaChart_mosaico_agricultura_pastagem)
    input_StackedAreaChart_mosaico_agricultura_pastagem <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_mosaico_agricultura_pastagem)[2:37]), Porc = as.numeric(input_StackedAreaChart_mosaico_agricultura_pastagem[2:37]), Ameaca = "Mosaico Agricultura e Pastagem")

    ### Infraestrutura Urbana
    input_StackedAreaChart_infraestrutura_urbana <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Infraestrutura Urbana")
    input_StackedAreaChart_infraestrutura_urbana <- t(input_StackedAreaChart_infraestrutura_urbana)
    input_StackedAreaChart_infraestrutura_urbana <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_infraestrutura_urbana)[2:37]), Porc = as.numeric(input_StackedAreaChart_infraestrutura_urbana[2:37]), Ameaca = "Infraestrutura Urbana")

    ### Silvicultura
    input_StackedAreaChart_silvicultura <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Silvicultura")
    input_StackedAreaChart_silvicultura <- t(input_StackedAreaChart_silvicultura)
    input_StackedAreaChart_silvicultura <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_silvicultura)[2:37]), Porc = as.numeric(input_StackedAreaChart_silvicultura[2:37]), Ameaca = "Silvicultura")

    ### Savana
    input_StackedAreaChart_savana <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Savana")
    input_StackedAreaChart_savana <- t(input_StackedAreaChart_savana)
    input_StackedAreaChart_savana <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_savana)[2:37]), Porc = as.numeric(input_StackedAreaChart_savana[2:37]), Ameaca = "Savana")

    ### Mangue
    input_StackedAreaChart_mangue <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Mangue")
    input_StackedAreaChart_mangue <- t(input_StackedAreaChart_mangue)
    input_StackedAreaChart_mangue <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_mangue)[2:37]), Porc = as.numeric(input_StackedAreaChart_mangue[2:37]), Ameaca = "Mangue")

    ### Campo Alagado e Área Pantanosa
    input_StackedAreaChart_alagado_pantanosa <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Campo Alagado e Área Pantanosa")
    input_StackedAreaChart_alagado_pantanosa <- t(input_StackedAreaChart_alagado_pantanosa)
    input_StackedAreaChart_alagado_pantanosa <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_alagado_pantanosa)[2:37]), Porc = as.numeric(input_StackedAreaChart_alagado_pantanosa[2:37]), Ameaca = "Campo Alagado e Área Pantanosa")

    ### Formação Campestre
    input_StackedAreaChart_campos <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Formação Campestre")
    input_StackedAreaChart_campos <- t(input_StackedAreaChart_campos)
    input_StackedAreaChart_campos <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_campos)[2:37]), Porc = as.numeric(input_StackedAreaChart_campos[2:37]), Ameaca = "Formação Campestre")

    ### Outras Formações não Florestais
    input_StackedAreaChart_outras_nao_florestais <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Outras Formações não Florestais")
    input_StackedAreaChart_outras_nao_florestais <- t(input_StackedAreaChart_outras_nao_florestais)
    input_StackedAreaChart_outras_nao_florestais <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_outras_nao_florestais)[2:37]), Porc = as.numeric(input_StackedAreaChart_outras_nao_florestais[2:37]), Ameaca = "Outras Formações não Florestais")

    ### Lavoura Temporária
    input_StackedAreaChart_lavouras_temporarias <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Lavoura Temporária")
    input_StackedAreaChart_lavouras_temporarias <- t(input_StackedAreaChart_lavouras_temporarias)
    input_StackedAreaChart_lavouras_temporarias <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_lavouras_temporarias)[2:37]), Porc = as.numeric(input_StackedAreaChart_lavouras_temporarias[2:37]), Ameaca = "Lavoura Temporária")

    ### Praia, Duna e Areal
    input_StackedAreaChart_praia_duna_areal <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Praia, Duna e Areal")
    input_StackedAreaChart_praia_duna_areal <- t(input_StackedAreaChart_praia_duna_areal)
    input_StackedAreaChart_praia_duna_areal <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_praia_duna_areal)[2:37]), Porc = as.numeric(input_StackedAreaChart_praia_duna_areal[2:37]), Ameaca = "Praia, Duna e Areal")

    ### Outras Áreas não Vegetadas
    input_StackedAreaChart_outras_nao_vegetadas <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Outras Áreas não Vegetadas")
    input_StackedAreaChart_outras_nao_vegetadas <- t(input_StackedAreaChart_outras_nao_vegetadas)
    input_StackedAreaChart_outras_nao_vegetadas <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_outras_nao_vegetadas)[2:37]), Porc = as.numeric(input_StackedAreaChart_outras_nao_vegetadas[2:37]), Ameaca = "Outras Áreas não Vegetadas")

    ### Não Observado
    input_StackedAreaChart_nao_observado <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Não Observado")
    input_StackedAreaChart_nao_observado <- t(input_StackedAreaChart_nao_observado)
    input_StackedAreaChart_nao_observado <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_nao_observado)[2:37]), Porc = as.numeric(input_StackedAreaChart_nao_observado[2:37]), Ameaca = "Não Observado")

    ### Afloramento Rochoso
    input_StackedAreaChart_afloramento_rochoso <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Afloramento Rochoso")
    input_StackedAreaChart_afloramento_rochoso <- t(input_StackedAreaChart_afloramento_rochoso)
    input_StackedAreaChart_afloramento_rochoso <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_afloramento_rochoso)[2:37]), Porc = as.numeric(input_StackedAreaChart_afloramento_rochoso[2:37]), Ameaca = "Afloramento Rochoso")

    ### Aquicultura
    input_StackedAreaChart_aquicultura <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Aquicultura")
    input_StackedAreaChart_aquicultura <- t(input_StackedAreaChart_aquicultura)
    input_StackedAreaChart_aquicultura <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_aquicultura)[2:37]), Porc = as.numeric(input_StackedAreaChart_aquicultura[2:37]), Ameaca = "Aquicultura")

    ### Apicum
    input_StackedAreaChart_apicum <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Apicum")
    input_StackedAreaChart_apicum <- t(input_StackedAreaChart_apicum)
    input_StackedAreaChart_apicum <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_apicum)[2:37]), Porc = as.numeric(input_StackedAreaChart_apicum[2:37]), Ameaca = "Apicum")

    ### Rio, Lago e Oceano
    input_StackedAreaChart_rio_lago_oceano <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Rio, Lago e Oceano")
    input_StackedAreaChart_rio_lago_oceano <- t(input_StackedAreaChart_rio_lago_oceano)
    input_StackedAreaChart_rio_lago_oceano <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_rio_lago_oceano)[2:37]), Porc = as.numeric(input_StackedAreaChart_rio_lago_oceano[2:37]), Ameaca = "Rio, Lago e Oceano")

    ### Arroz
    input_StackedAreaChart_arroz <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Arroz")
    input_StackedAreaChart_arroz <- t(input_StackedAreaChart_arroz)
    input_StackedAreaChart_arroz <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_arroz)[2:37]), Porc = as.numeric(input_StackedAreaChart_arroz[2:37]), Ameaca = "Arroz")

    ### Café
    input_StackedAreaChart_cafe <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Café")
    input_StackedAreaChart_cafe <- t(input_StackedAreaChart_cafe)
    input_StackedAreaChart_cafe <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_cafe)[2:37]), Porc = as.numeric(input_StackedAreaChart_cafe[2:37]), Ameaca = "Café")

    ### Citrus
    input_StackedAreaChart_citrus <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Citrus")
    input_StackedAreaChart_citrus <- t(input_StackedAreaChart_citrus)
    input_StackedAreaChart_citrus <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_citrus)[2:37]), Porc = as.numeric(input_StackedAreaChart_citrus[2:37]), Ameaca = "Citrus")

    ### Outras Lavouras Perenes
    input_StackedAreaChart_outras_lavouras_perenes <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Outras Lavouras Perenes")
    input_StackedAreaChart_outras_lavouras_perenes <- t(input_StackedAreaChart_outras_lavouras_perenes)
    input_StackedAreaChart_outras_lavouras_perenes <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_outras_lavouras_perenes)[2:37]), Porc = as.numeric(input_StackedAreaChart_outras_lavouras_perenes[2:37]), Ameaca = "Outras Lavouras Perenes")

    ### Restinga Arborizada
    input_StackedAreaChart_restinga_arborizada <- input_StackedAreaChart %>% dplyr::filter(Ameaca == "Restinga Arborizada")
    input_StackedAreaChart_restinga_arborizada <- t(input_StackedAreaChart_restinga_arborizada)
    input_StackedAreaChart_restinga_arborizada <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_restinga_arborizada)[2:37]), Porc = as.numeric(input_StackedAreaChart_restinga_arborizada[2:37]), Ameaca = "Restinga Arborizada")

    ## Data.frame das Feições
    StackedAreaChart_df <- rbind (

      input_StackedAreaChart_floresta,
      input_StackedAreaChart_pastagem,
      input_StackedAreaChart_mineracao,
      input_StackedAreaChart_soja,
      input_StackedAreaChart_cana,
      input_StackedAreaChart_lavouras_perenes,
      input_StackedAreaChart_outras_lavouras_temporarias,
      input_StackedAreaChart_mosaico_agricultura_pastagem,
      input_StackedAreaChart_infraestrutura_urbana,
      input_StackedAreaChart_silvicultura,
      input_StackedAreaChart_savana,
      input_StackedAreaChart_mangue,
      input_StackedAreaChart_alagado_pantanosa,
      input_StackedAreaChart_campos,
      input_StackedAreaChart_outras_nao_florestais,
      input_StackedAreaChart_lavouras_temporarias,
      input_StackedAreaChart_praia_duna_areal,
      input_StackedAreaChart_outras_nao_vegetadas,
      input_StackedAreaChart_nao_observado,
      input_StackedAreaChart_afloramento_rochoso,
      input_StackedAreaChart_aquicultura,
      input_StackedAreaChart_apicum,
      input_StackedAreaChart_rio_lago_oceano,
      input_StackedAreaChart_arroz,
      input_StackedAreaChart_cafe,
      input_StackedAreaChart_citrus,
      input_StackedAreaChart_outras_lavouras_perenes,
      input_StackedAreaChart_restinga_arborizada

    )

    ## Substituir NA para 0
    StackedAreaChart_df$Porc[is.na(StackedAreaChart_df$Porc)] <- 0

    ## Organizar por Ano
    StackedAreaChart_df <- StackedAreaChart_df %>% arrange(Ano)
    StackedAreaChart_df$Ano <- as.numeric(StackedAreaChart_df$Ano)

    ## Converter as feições como fatores
    StackedAreaChart_df$Ameaca <-
      factor(

        StackedAreaChart_df$Ameaca,
        levels = c(
          "Floresta",
          "Savana",
          "Mangue",
          "Restinga Arborizada",
          "Campo Alagado e Área Pantanosa",
          "Formação Campestre",
          "Apicum",
          "Afloramento Rochoso",
          "Outras Formações não Florestais",
          "Pastagem",
          "Lavoura Temporária",
          "Soja",
          "Cana-de-açúcar",
          "Arroz",
          "Outras lavouras temporárias",
          "Lavouras perenes",
          "Café",
          "Citrus",
          "Outras Lavouras Perenes",
          "Silvicultura",
          "Mosaico Agricultura e Pastagem",
          "Praia, Duna e Areal",
          "Infraestrutura Urbana",
          "Mineração",
          "Outras Áreas não Vegetadas",
          "Rio, Lago e Oceano",
          "Aquicultura",
          "Não Observado"

        )

      )

    ## Salvar imagem
    g <- ggplot(

      StackedAreaChart_df,
      aes(

        x = Ano,
        y = Porc,
        fill = Ameaca

      )

    ) +
      geom_area() +
      scale_fill_manual(

        values=

          c(

            "#129912",
            "#00ff00",
            "#687537",
            "#6b9932",
            "#45C2A5",
            "#B8AF4F",
            "#968c46",
            "#665a3a",
            "#f1c232",
            "#FFD966",
            "#D5A6BD",
            "#e075ad",
            "#C27BA0",
            "#982c9e",
            "#e787f8",
            "#f3b4f1",
            "#cca0d4",
            "#d082de",
            "#cd49e4",
            "#ad4413",
            "#fff3bf",
            "#DD7E6B",
            "#aa0000",
            "#af2a2a",
            "#ff3d3d",
            "#0000FF",
            "#02106f",
            "#D5D5E5"

          )

      ) +
      theme(legend.text = element_text(size = 12))

    ggsave(

      paste0(

        "StackedArea-",
        i,
        ".png"

      ),
      width = 12,
      height = 10,
      dpi = 500

    )


    ## Salvar imagem Natural vs. Ameaça
    StackedAreaChartNatural_df <-
      StackedAreaChart_df %>%
      dplyr::filter(Ameaca %in%
                      c(

                        "Floresta",
                        "Savana",
                        "Mangue",
                        "Restinga Arborizada",
                        "Campo Alagado",
                        "Formação Campestre",
                        "Apicum",
                        "Afloramento Rochoso",
                        "Outras Formações não Florestais",
                        "Praia, Duna e Areal",
                        "Rio, Lago e Oceano"

                      )

      )

    StackedAreaChartThreat_df <-
      StackedAreaChart_df %>%
      dplyr::filter(Ameaca %in%
                      c(

                        "Pastagem",
                        "Lavoura Temporária",
                        "Soja",
                        "Cana-de-açúcar",
                        "Arroz",
                        "Outras lavouras temporárias",
                        "Lavouras perens",
                        "Café",
                        "Citrus",
                        "Outras Lavouras Perenes",
                        "Silvicultura",
                        "Mosaico Agricultura e Pastagem",
                        "Infraestrutura Urbana",
                        "Mineração",
                        "Outras Áreas não Vegetadas",
                        "Aquicultura"

                      )

      )

    StackedAreaChartNatural_df <-
      StackedAreaChartNatural_df %>%
      group_by(Ano) %>%
      summarise(Porc = sum(Porc), Feicao = "Natural")

    StackedAreaChartThreat_df <-
      StackedAreaChartThreat_df %>%
      group_by(Ano) %>%
      summarise(Porc = sum(Porc), Feicao = "Uso Alternativo")

    StackedAreaChartNaturalThreatdf <-
      rbind(

        StackedAreaChartNatural_df,
        StackedAreaChartThreat_df

      )

    StackedAreaChartNaturalThreatdf$Feicao <-
      factor(

        StackedAreaChartNaturalThreatdf$Feicao,
        levels = c("Natural", "Uso Alternativo")

      )

    g <- ggplot(

      StackedAreaChartNaturalThreatdf,
      aes(

        x = Ano,
        y = Porc,
        fill = Feicao

      )

    ) +
      geom_area() +
      scale_fill_manual(values = c("#32CD32", "red")) +
      theme(

        legend.position="none",
        axis.text.x=element_text(size=26),
        axis.text.y=element_text(size=26),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()

      )

    ggsave(paste0("StackedArea_NaturalvsThreat-", i, ".png"), width = 12, height = 10, dpi = 500)

    output<-data.frame(Especie=i, Texto=Texto, Considerações=considerations)


    # Análise de tendência: AOO ####

    output_trendline <- NULL

    input_MapBiomas <- MapBiomas_coverLand_results %>%
      dplyr::filter(Species == SPECIES)

    input_Ameaca <- unique(input_MapBiomas$Ameaca[-29])


    ## Feições isoladas ####

    for(

      feicao in input_Ameaca

    ){

      input2 <- input_MapBiomas %>%
        dplyr::filter(Ameaca == feicao)

      input2 <- input2 %>%
        dplyr::select(contains("Porcentagem_AOO"))

      if(

        sum(as.numeric(input2)) == 0

      ){

      } else {

        FEICAO <- feicao

        input3 <- t(input2)
        rownames(input3) <- 1:length(input3[,1])

        input4 <- data.frame(

          Ano = 1985:2020,
          Porcentagem_AOO = input3[,1]

        )

        png(

          file = paste0("TrendAOO-", SPECIES, "_" , feicao, ".png"),
          height = 500,
          width = 1000

        )

        trendline(

          input4$Ano,
          as.numeric(input4$Porcentagem_AOO),
          model = "line2P",
          eDigit = 4,
          main = paste0("AOO ", SPECIES,": ", FEICAO),
          xlab = "",
          ylab = "%"

        )

        dev.off()

        trendline_summary <-
          trendline_summary(

            input4$Ano,
            as.numeric(input4$Porcentagem_AOO),
            model = "line2P",
            eDigit = 4

          )

        output_trendline_ <-
          data.frame(

            Especie = SPECIES,
            Feição = FEICAO,
            Taxa_anual = round(

              trendline_summary$parameter$a *
                100 /
                length(input4$Ano),
              2

            )

          )

        tsfit <- trendsegment(

          x = as.numeric(input4$Porcentagem_AOO),
          minsegL = 10

        ) # Mudar para 3

        png(

          file = paste0("TrendAOO_detectTrends-", SPECIES, "_" , FEICAO, ".png"),
          height = 500,
          width = 1000

        )

        plot(

          input4,
          type = "b",
          ylim = range(as.numeric(input4$Porcentagem_AOO), tsfit$est),
          main = paste0("AOO ", SPECIES,": ", FEICAO),
          xlab = "",
          ylab = "%"

        )

        lines(input4$Ano, tsfit$est, col=2, lwd=2)

        dev.off()

        if(

          length(tsfit$cpt) == 0

        ){

          no_trend_AOO <- T
          png(

            file = paste0("TrendAOO_LastTrend-", SPECIES, "_" , FEICAO, ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            input4$Ano[1:36],
            as.numeric(input4$Porcentagem_AOO[1:36]),
            model = "line2P",
            eDigit = 4,
            main = paste0("AOO ", SPECIES,": ", FEICAO),
            sub = paste0("Última Tendência", " (1985 a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

          trendline_summary_last_trend <-
            trendline_summary(

              input4$Ano[1:36],
              as.numeric(input4$Porcentagem_AOO[1:36]),
              model = "line2P",
              eDigit = 4

            )

        } else {

          no_trend_AOO <- F
          png(

            file = paste0("TrendAOO_LastTrend-", SPECIES, "_" , FEICAO, ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            input4$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
            as.numeric(input4$Porcentagem_AOO[tsfit$cpt[length(tsfit$cpt)]:36]),
            model = "line2P",
            eDigit = 4,
            main = paste0("AOO ", SPECIES,": ", FEICAO),
            sub = paste0("Última Tendência", " (", input4$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

          trendline_summary_last_trend <-
            trendline_summary(

              input4$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
              as.numeric(input4$Porcentagem_AOO[tsfit$cpt[length(tsfit$cpt)]:36]),
              model ="line2P",
              eDigit = 4

            )

        }

        if(

          no_trend_AOO == T

        ){

          output_trendline__ <- data.frame(Inicio_ultima_tendencia = "Sem reversão de tendência", Taxa_ultima_tendencia = "Sem reversão de tendência")

        } else {

          output_trendline__ <- data.frame(Inicio_ultima_tendencia = input4$Ano[tsfit$cpt[length(tsfit$cpt)]], Taxa_ultima_tendencia = round(trendline_summary_last_trend$parameter$a*100/trendline_summary_last_trend$N, 2))

        }

        output_trendline_ <- cbind(output_trendline_, output_trendline__)

        output_trendline <- rbind(output_trendline, output_trendline_)

        output_trendline_n <- nrow(output_trendline)

        rm(no_trend_AOO)

      }

    }


    ## Feições aglutinadas: Natural ####
    png(file = paste0("TrendAOO-", SPECIES, "_Natural", ".png"), height = 500, width = 1000)
    trendline(StackedAreaChartNatural_df$Ano, StackedAreaChartNatural_df$Porc, model = "line2P", eDigit = 4, main = paste0("AOO ", SPECIES,": Natural"), xlab = "", ylab = "%")
    dev.off()

    trendline_summary_Natural <-
      trendline_summary(StackedAreaChartNatural_df$Ano, StackedAreaChartNatural_df$Porc, model = "line2P", eDigit = 4)

    output_trendline_Natural_Taxa_anual <-
      data.frame(Taxa_anual = round(trendline_summary_Natural$parameter$a*100/length(StackedAreaChartNatural_df$Ano), 2))

    tsfit <- trendsegment(

      x = as.numeric(StackedAreaChartNatural_df$Porc),
      minsegL = 10

    ) # Mudar para 3

    png(file = paste0("TrendAOO_detectTrends-", SPECIES, "_Natural", ".png"), height = 500, width = 1000)
    plot(

      StackedAreaChartNatural_df[,1:2],
      type = "b",
      ylim = range(as.numeric(StackedAreaChartNatural_df$Porc), tsfit$est),
      main = paste0("AOO ", SPECIES,": Natural"),
      xlab = "",
      ylab = "%"

    )

    lines(StackedAreaChartNatural_df$Ano, tsfit$est, col=2, lwd=2)
    dev.off()

    if(

      output_trendline_Natural_Taxa_anual == 0

    ){

      png(file = paste0("TrendAOO_LastTrend-", SPECIES, "_Natural", ".png"), height = 500, width = 1000)
      trendline(StackedAreaChartNatural_df$Ano, StackedAreaChartNatural_df$Porc, model = "line2P", eDigit = 4, main = paste0("AOO ", SPECIES,": Natural"), sub = paste0("Última Tendência", " (1985 a 2020)"), xlab = "", ylab = "%")
      dev.off()

    } else {

      if(

        is.null(tsfit$cpt) == T

      ){

        png(file = paste0("TrendAOO_LastTrend-", SPECIES, "_Natural", ".png"), height = 500, width = 1000)
        trendline(StackedAreaChartNatural_df$Ano, StackedAreaChartNatural_df$Porc, model = "line2P", eDigit = 4, main = paste0("AOO ", SPECIES,": Natural"), sub = paste0("Última Tendência", " (1985 a 2020)"), xlab = "", ylab = "%")
        dev.off()

      } else {

        png(file = paste0("TrendAOO_LastTrend-", SPECIES, "_Natural", ".png"), height = 500, width = 1000)
        trendline(StackedAreaChartNatural_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36], StackedAreaChartNatural_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36], model = "line2P", eDigit = 4, main = paste0("AOO ", SPECIES,": Natural"), sub = paste0("Última Tendência", " (", input4$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"), xlab = "", ylab = "%")
        dev.off()

      }

    }

    if(

      output_trendline_Natural_Taxa_anual == 0

    ){

      trendline_summary_Natural_LastTrend <-
        trendline_summary(

          StackedAreaChartNatural_df$Ano,
          StackedAreaChartNatural_df$Porc,
          model = "line2P",
          eDigit = 4

        )

      output_trendline_Natural_LastTrend <-
        data.frame(

          Inicio_ultima_tendencia = "Sem reversão de tendência",
          Taxa_ultima_tendencia = "Sem reversão de tendência"

        )

    } else {

      if(

        is.null(tsfit$cpt) == T

      ){

        trendline_summary_Natural_LastTrend <-
          trendline_summary(

            StackedAreaChartNatural_df$Ano,
            StackedAreaChartNatural_df$Porc,
            model = "line2P",
            eDigit = 4

          )

        output_trendline_Natural_LastTrend <-
          data.frame(

            Inicio_ultima_tendencia = as.numeric(1985),
            Taxa_ultima_tendencia = round(trendline_summary_Natural_LastTrend$parameter$a * 100 / length(StackedAreaChartNatural_df$Ano), 2)

          )

      } else {

        trendline_summary_Natural_LastTrend <-
          trendline_summary(

            StackedAreaChartNatural_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
            StackedAreaChartNatural_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36],
            model = "line2P",
            eDigit = 4

          )

        output_trendline_Natural_LastTrend <-
          data.frame(

            Inicio_ultima_tendencia = StackedAreaChartNatural_df$Ano[tsfit$cpt[length(tsfit$cpt)]],
            Taxa_ultima_tendencia = round(trendline_summary_Natural_LastTrend$parameter$a*100/length(StackedAreaChartNatural_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36]), 2)

          )

      }

    }

    output_trendline_Natural <-
      cbind(

        output_trendline_Natural_Taxa_anual,
        output_trendline_Natural_LastTrend

      )


    ## Feições Aglutinadas: Uso Alternativo ####

    trendline_summary_Threat <-
      trendline_summary(

        StackedAreaChartThreat_df$Ano,
        StackedAreaChartThreat_df$Porc,
        model = "line2P",
        eDigit = 4

      )

    output_trendline_Threat_Taxa_anual <-
      data.frame(

        Taxa_anual = round(

          trendline_summary_Threat$parameter$a *
            100 /
            length(StackedAreaChartThreat_df$Ano),
          2

        )

      )

    if(

      output_trendline_Threat_Taxa_anual == 0

    ){

      png(

        file = paste0("TrendAOO-", SPECIES, "_Threat", ".png"),
        height = 500,
        width = 1000

      )

      plot(

        StackedAreaChartThreat_df[,1:2],
        main = paste0("AOO ", SPECIES,": Uso Alternativo"),
        xlab = "",
        ylab = "%"

      )

      dev.off()

    } else {

      png(

        file = paste0("TrendAOO-", SPECIES, "_Threat", ".png"),
        height = 500,
        width = 1000

      )

      trendline(

        StackedAreaChartThreat_df$Ano,
        StackedAreaChartThreat_df$Porc,
        # main = paste0("AOO ", SPECIES,": Uso Alternativo"),
        xlab = "",
        ylab = "",
        cex.axis = 2,
        eSize = 2

      )

      dev.off()

    }

    tsfit <-
      trendsegment(

        x = as.numeric(StackedAreaChartThreat_df$Porc),
        minsegL = 10

      ) # Mudar para 3

    png(

      file = paste0("TrendAOO_detectTrends-", SPECIES, "_Threat", ".png"),
      height = 500,
      width = 1000

    )

    plot(

      StackedAreaChartThreat_df[,1:2],
      type = "b",
      ylim = range(as.numeric(StackedAreaChartThreat_df$Porc), tsfit$est),
      main = paste0("AOO ", SPECIES,": Uso Alternativo"),
      xlab = "",
      ylab = "%"

    )

    lines(StackedAreaChartThreat_df$Ano, tsfit$est, col=2, lwd=2)

    dev.off()

    if(

      output_trendline_Threat_Taxa_anual == 0

    ){

      png(

        file = paste0("TrendAOO_LastTrend-", SPECIES, "_Threat", ".png"),
        height = 500,
        width = 1000

      )

      plot(

        StackedAreaChartThreat_df[,1:2],
        model = "line2P",
        eDigit = 4,
        main = paste0("AOO ", SPECIES,": Uso Alternativo"),
        sub = paste0("Última Tendência", " (1985 a 2020)"),
        xlab = "",
        ylab = "%"

      )

      dev.off()

    } else {

      if(

        output_trendline_Threat_Taxa_anual < 1

      ){

        png(

          file = paste0("TrendAOO_LastTrend-", SPECIES, "_Threat", ".png"),
          height = 500,
          width = 1000

        )

        trendline(

          StackedAreaChartThreat_df$Ano[1:36],
          StackedAreaChartThreat_df$Porc[1:36],
          model = "line2P",
          eDigit = 4,
          main = paste0("AOO ", SPECIES,": Uso Alternativo"),
          sub = paste0("Última Tendência", " (", input4$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"),
          xlab = "",
          ylab = "%"

        )

        dev.off()

      } else {

        if(

          is.null(tsfit$cpt) == T

        ){

          png(

            file = paste0("TrendAOO_LastTrend-", SPECIES, "_Threat", ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            StackedAreaChartThreat_df$Ano[1:36],
            StackedAreaChartThreat_df$Porc[1:36],
            model = "line2P",
            eDigit = 4,
            main = paste0("AOO ", SPECIES,": Uso Alternativo"),
            sub = paste0("Última Tendência", " (1985 a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

        } else {

          png(

            file = paste0("TrendAOO_LastTrend-", SPECIES, "_Threat", ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            StackedAreaChartThreat_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
            StackedAreaChartThreat_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36],
            model = "line2P",
            eDigit = 4,
            main = paste0("AOO ", SPECIES,": Uso Alternativo"),
            sub = paste0("Última Tendência", " (", input4$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

        }

      }

    }

    if(

      output_trendline_Threat_Taxa_anual == 0

    ){

      output_trendline_Threat_LastTrend <- data.frame(Inicio_ultima_tendencia = "Sem reversão de tendência", Taxa_ultima_tendencia = "Sem reversão de tendência")

    } else {

      if(

        output_trendline_Threat_Taxa_anual < 1

      ){

        trendline_summary_Threat_LastTrend <-
          trendline_summary(StackedAreaChartThreat_df$Ano[1:36], StackedAreaChartThreat_df$Porc[1:36], model = "line2P", eDigit = 4)

        output_trendline_Threat_LastTrend <-
          data.frame(Inicio_ultima_tendencia = StackedAreaChartThreat_df$Ano[1], Taxa_ultima_tendencia = round(trendline_summary_Threat_LastTrend$parameter$a*100/length(StackedAreaChartThreat_df$Ano[1:36]), 2))

      } else {

        if(

          is.null(tsfit$cpt) == T

        ){

          trendline_summary_Threat_LastTrend <-
            trendline_summary(StackedAreaChartThreat_df$Ano[1:36], StackedAreaChartThreat_df$Porc[1:36], model = "line2P", eDigit = 4)

          output_trendline_Threat_LastTrend <-
            data.frame(Inicio_ultima_tendencia = StackedAreaChartThreat_df$Ano[1], Taxa_ultima_tendencia = round(trendline_summary_Threat_LastTrend$parameter$a*100/length(StackedAreaChartThreat_df$Ano[1:36]), 2))

        } else {

          trendline_summary_Threat_LastTrend <-
            trendline_summary(StackedAreaChartThreat_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36], StackedAreaChartThreat_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36], model = "line2P", eDigit = 4)

          output_trendline_Threat_LastTrend <-
            data.frame(Inicio_ultima_tendencia = StackedAreaChartThreat_df$Ano[tsfit$cpt[length(tsfit$cpt)]], Taxa_ultima_tendencia = round(trendline_summary_Threat_LastTrend$parameter$a*100/length(StackedAreaChartThreat_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36]), 2))

        }

      }

    }

    output_trendline_Threat <- cbind(output_trendline_Threat_Taxa_anual, output_trendline_Threat_LastTrend)


    ## Reunir Outputs Natural e Uso Alternativo ####

    output_trendline_NaturalANDThreat <- rbind(output_trendline_Natural, output_trendline_Threat)
    output_trendline_NaturalANDThreat <- data.frame(Feições_Aglutinadas = c("Natural", "Uso Alternativo"), output_trendline_NaturalANDThreat)


    # Gráfico de área empilhada: EOO ####

    if(

      is.na(dataPie$Area_EOO_sites_km2[1]) == T

    ){

    } else {

      if(

        dataPie$Area_EOO_sites_km2[1] != "Análise dispensada"

      ){

        input_StackedAreaChart_EOO <- MapBiomas_coverLand_results %>% dplyr::filter(Species == SPECIES) %>% dplyr::filter(Ameaca != "Total") %>% dplyr::select(Ameaca, contains("Porcentagem_EOO"))

        ### Floresta
        input_StackedAreaChart_EOO_floresta <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Floresta")
        input_StackedAreaChart_EOO_floresta <- t(input_StackedAreaChart_EOO_floresta)
        input_StackedAreaChart_EOO_floresta <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_floresta)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_floresta[2:37]), Ameaca = "Floresta")

        ### Pastagem
        input_StackedAreaChart_EOO_pastagem <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Pastagem")
        input_StackedAreaChart_EOO_pastagem <- t(input_StackedAreaChart_EOO_pastagem)
        input_StackedAreaChart_EOO_pastagem <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_pastagem)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_pastagem[2:37]), Ameaca = "Pastagem")

        ### Mineração
        input_StackedAreaChart_EOO_mineracao <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Mineração")
        input_StackedAreaChart_EOO_mineracao <- t(input_StackedAreaChart_EOO_mineracao)
        input_StackedAreaChart_EOO_mineracao <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_mineracao)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_mineracao[2:37]), Ameaca = "Mineração")

        ### Soja
        input_StackedAreaChart_EOO_soja <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Soja")
        input_StackedAreaChart_EOO_soja <- t(input_StackedAreaChart_EOO_soja)
        input_StackedAreaChart_EOO_soja <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_soja)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_soja[2:37]), Ameaca = "Soja")

        ### Cana-de-açúcar
        input_StackedAreaChart_EOO_cana <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Cana-de-açúcar")
        input_StackedAreaChart_EOO_cana <- t(input_StackedAreaChart_EOO_cana)
        input_StackedAreaChart_EOO_cana <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_cana)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_cana[2:37]), Ameaca = "Cana-de-açúcar")

        ### Lavouras perenes
        input_StackedAreaChart_EOO_lavouras_perenes <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Lavouras perenes")
        input_StackedAreaChart_EOO_lavouras_perenes <- t(input_StackedAreaChart_EOO_lavouras_perenes)
        input_StackedAreaChart_EOO_lavouras_perenes <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_lavouras_perenes)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_lavouras_perenes[2:37]), Ameaca = "Lavouras perenes")

        ### Outras lavouras temporárias
        input_StackedAreaChart_EOO_outras_lavouras_temporarias <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Outras lavouras temporárias")
        input_StackedAreaChart_EOO_outras_lavouras_temporarias <- t(input_StackedAreaChart_EOO_outras_lavouras_temporarias)
        input_StackedAreaChart_EOO_outras_lavouras_temporarias <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_outras_lavouras_temporarias)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_outras_lavouras_temporarias[2:37]), Ameaca = "Outras lavouras temporárias")

        ### Mosaico Agricultura e Pastagem
        input_StackedAreaChart_EOO_mosaico_agricultura_pastagem <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Mosaico Agricultura e Pastagem")
        input_StackedAreaChart_EOO_mosaico_agricultura_pastagem <- t(input_StackedAreaChart_EOO_mosaico_agricultura_pastagem)
        input_StackedAreaChart_EOO_mosaico_agricultura_pastagem <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_mosaico_agricultura_pastagem)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_mosaico_agricultura_pastagem[2:37]), Ameaca = "Mosaico Agricultura e Pastagem")

        ### Infraestrutura Urbana
        input_StackedAreaChart_EOO_infraestrutura_urbana <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Infraestrutura Urbana")
        input_StackedAreaChart_EOO_infraestrutura_urbana <- t(input_StackedAreaChart_EOO_infraestrutura_urbana)
        input_StackedAreaChart_EOO_infraestrutura_urbana <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_infraestrutura_urbana)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_infraestrutura_urbana[2:37]), Ameaca = "Infraestrutura Urbana")

        ### Silvicultura
        input_StackedAreaChart_EOO_silvicultura <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Silvicultura")
        input_StackedAreaChart_EOO_silvicultura <- t(input_StackedAreaChart_EOO_silvicultura)
        input_StackedAreaChart_EOO_silvicultura <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_silvicultura)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_silvicultura[2:37]), Ameaca = "Silvicultura")

        ### Savana
        input_StackedAreaChart_EOO_savana <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Savana")
        input_StackedAreaChart_EOO_savana <- t(input_StackedAreaChart_EOO_savana)
        input_StackedAreaChart_EOO_savana <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_savana)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_savana[2:37]), Ameaca = "Savana")

        ### Mangue
        input_StackedAreaChart_EOO_mangue <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Mangue")
        input_StackedAreaChart_EOO_mangue <- t(input_StackedAreaChart_EOO_mangue)
        input_StackedAreaChart_EOO_mangue <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_mangue)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_mangue[2:37]), Ameaca = "Mangue")

        ### Campo Alagado e Área Pantanosa
        input_StackedAreaChart_EOO_alagado_pantanosa <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Campo Alagado e Área Pantanosa")
        input_StackedAreaChart_EOO_alagado_pantanosa <- t(input_StackedAreaChart_EOO_alagado_pantanosa)
        input_StackedAreaChart_EOO_alagado_pantanosa <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_alagado_pantanosa)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_alagado_pantanosa[2:37]), Ameaca = "Campo Alagado e Área Pantanosa")

        ### Formação Campestre
        input_StackedAreaChart_EOO_campos <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Formação Campestre")
        input_StackedAreaChart_EOO_campos <- t(input_StackedAreaChart_EOO_campos)
        input_StackedAreaChart_EOO_campos <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_campos)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_campos[2:37]), Ameaca = "Formação Campestre")

        ### Outras Formações não Florestais
        input_StackedAreaChart_EOO_outras_nao_florestais <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Outras Formações não Florestais")
        input_StackedAreaChart_EOO_outras_nao_florestais <- t(input_StackedAreaChart_EOO_outras_nao_florestais)
        input_StackedAreaChart_EOO_outras_nao_florestais <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_outras_nao_florestais)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_outras_nao_florestais[2:37]), Ameaca = "Outras Formações não Florestais")

        ### Lavoura Temporária
        input_StackedAreaChart_EOO_lavouras_temporarias <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Lavoura Temporária")
        input_StackedAreaChart_EOO_lavouras_temporarias <- t(input_StackedAreaChart_EOO_lavouras_temporarias)
        input_StackedAreaChart_EOO_lavouras_temporarias <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_lavouras_temporarias)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_lavouras_temporarias[2:37]), Ameaca = "Lavoura Temporária")

        ### Praia, Duna e Areal
        input_StackedAreaChart_EOO_praia_duna_areal <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Praia, Duna e Areal")
        input_StackedAreaChart_EOO_praia_duna_areal <- t(input_StackedAreaChart_EOO_praia_duna_areal)
        input_StackedAreaChart_EOO_praia_duna_areal <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_praia_duna_areal)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_praia_duna_areal[2:37]), Ameaca = "Praia, Duna e Areal")

        ### Outras Áreas não Vegetadas
        input_StackedAreaChart_EOO_outras_nao_vegetadas <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Outras Áreas não Vegetadas")
        input_StackedAreaChart_EOO_outras_nao_vegetadas <- t(input_StackedAreaChart_EOO_outras_nao_vegetadas)
        input_StackedAreaChart_EOO_outras_nao_vegetadas <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_outras_nao_vegetadas)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_outras_nao_vegetadas[2:37]), Ameaca = "Outras Áreas não Vegetadas")

        ### Não Observado
        input_StackedAreaChart_EOO_nao_observado <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Não Observado")
        input_StackedAreaChart_EOO_nao_observado <- t(input_StackedAreaChart_EOO_nao_observado)
        input_StackedAreaChart_EOO_nao_observado <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_nao_observado)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_nao_observado[2:37]), Ameaca = "Não Observado")

        ### Afloramento Rochoso
        input_StackedAreaChart_EOO_afloramento_rochoso <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Afloramento Rochoso")
        input_StackedAreaChart_EOO_afloramento_rochoso <- t(input_StackedAreaChart_EOO_afloramento_rochoso)
        input_StackedAreaChart_EOO_afloramento_rochoso <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_afloramento_rochoso)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_afloramento_rochoso[2:37]), Ameaca = "Afloramento Rochoso")

        ### Aquicultura
        input_StackedAreaChart_EOO_aquicultura <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Aquicultura")
        input_StackedAreaChart_EOO_aquicultura <- t(input_StackedAreaChart_EOO_aquicultura)
        input_StackedAreaChart_EOO_aquicultura <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_aquicultura)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_aquicultura[2:37]), Ameaca = "Aquicultura")

        ### Apicum
        input_StackedAreaChart_EOO_apicum <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Apicum")
        input_StackedAreaChart_EOO_apicum <- t(input_StackedAreaChart_EOO_apicum)
        input_StackedAreaChart_EOO_apicum <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_apicum)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_apicum[2:37]), Ameaca = "Apicum")

        ### Rio, Lago e Oceano
        input_StackedAreaChart_EOO_rio_lago_oceano <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Rio, Lago e Oceano")
        input_StackedAreaChart_EOO_rio_lago_oceano <- t(input_StackedAreaChart_EOO_rio_lago_oceano)
        input_StackedAreaChart_EOO_rio_lago_oceano <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_rio_lago_oceano)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_rio_lago_oceano[2:37]), Ameaca = "Rio, Lago e Oceano")

        ### Arroz
        input_StackedAreaChart_EOO_arroz <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Arroz")
        input_StackedAreaChart_EOO_arroz <- t(input_StackedAreaChart_EOO_arroz)
        input_StackedAreaChart_EOO_arroz <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_arroz)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_arroz[2:37]), Ameaca = "Arroz")

        ### Café
        input_StackedAreaChart_EOO_cafe <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Café")
        input_StackedAreaChart_EOO_cafe <- t(input_StackedAreaChart_EOO_cafe)
        input_StackedAreaChart_EOO_cafe <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_cafe)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_cafe[2:37]), Ameaca = "Café")

        ### Citrus
        input_StackedAreaChart_EOO_citrus <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Citrus")
        input_StackedAreaChart_EOO_citrus <- t(input_StackedAreaChart_EOO_citrus)
        input_StackedAreaChart_EOO_citrus <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_citrus)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_citrus[2:37]), Ameaca = "Citrus")

        ### Outras Lavouras Perenes
        input_StackedAreaChart_EOO_outras_lavouras_perenes <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Outras Lavouras Perenes")
        input_StackedAreaChart_EOO_outras_lavouras_perenes <- t(input_StackedAreaChart_EOO_outras_lavouras_perenes)
        input_StackedAreaChart_EOO_outras_lavouras_perenes <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_outras_lavouras_perenes)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_outras_lavouras_perenes[2:37]), Ameaca = "Outras Lavouras Perenes")

        ### Restinga Arborizada
        input_StackedAreaChart_EOO_restinga_arborizada <- input_StackedAreaChart_EOO %>% dplyr::filter(Ameaca == "Restinga Arborizada")
        input_StackedAreaChart_EOO_restinga_arborizada <- t(input_StackedAreaChart_EOO_restinga_arborizada)
        input_StackedAreaChart_EOO_restinga_arborizada <- data.frame(Ano = sub("Porcentagem_EOO_", "", row.names(input_StackedAreaChart_EOO_restinga_arborizada)[2:37]), Porc = as.numeric(input_StackedAreaChart_EOO_restinga_arborizada[2:37]), Ameaca = "Restinga Arborizada")

        ## Data.frame das Feições
        StackedAreaChart_EOO_df <-
          rbind (

            input_StackedAreaChart_EOO_floresta,
            input_StackedAreaChart_EOO_pastagem,
            input_StackedAreaChart_EOO_mineracao,
            input_StackedAreaChart_EOO_soja,
            input_StackedAreaChart_EOO_cana,
            input_StackedAreaChart_EOO_lavouras_perenes,
            input_StackedAreaChart_EOO_outras_lavouras_temporarias,
            input_StackedAreaChart_EOO_mosaico_agricultura_pastagem,
            input_StackedAreaChart_EOO_infraestrutura_urbana,
            input_StackedAreaChart_EOO_silvicultura,
            input_StackedAreaChart_EOO_savana,
            input_StackedAreaChart_EOO_mangue,
            input_StackedAreaChart_EOO_alagado_pantanosa,
            input_StackedAreaChart_EOO_campos,
            input_StackedAreaChart_EOO_outras_nao_florestais,
            input_StackedAreaChart_EOO_lavouras_temporarias,
            input_StackedAreaChart_EOO_praia_duna_areal,
            input_StackedAreaChart_EOO_outras_nao_vegetadas,
            input_StackedAreaChart_EOO_nao_observado,
            input_StackedAreaChart_EOO_afloramento_rochoso,
            input_StackedAreaChart_EOO_aquicultura,
            input_StackedAreaChart_EOO_apicum,
            input_StackedAreaChart_EOO_rio_lago_oceano,
            input_StackedAreaChart_EOO_arroz,
            input_StackedAreaChart_EOO_cafe,
            input_StackedAreaChart_EOO_citrus,
            input_StackedAreaChart_EOO_outras_lavouras_perenes,
            input_StackedAreaChart_EOO_restinga_arborizada

          )

        ## Organizar por Ano
        StackedAreaChart_EOO_df <-
          StackedAreaChart_EOO_df %>%
          arrange(Ano)

        StackedAreaChart_EOO_df$Ano <-
          as.numeric(StackedAreaChart_EOO_df$Ano)

        ## Converter as feições como fatores
        StackedAreaChart_EOO_df$Ameaca <-
          factor(

            StackedAreaChart_EOO_df$Ameaca,
            levels = c(

              "Floresta",
              "Savana",
              "Mangue",
              "Restinga Arborizada",
              "Campo Alagado e Área Pantanosa",
              "Formação Campestre",
              "Apicum",
              "Afloramento Rochoso",
              "Outras Formações não Florestais",
              "Pastagem",
              "Lavoura Temporária",
              "Soja",
              "Cana-de-açúcar",
              "Arroz",
              "Outras lavouras temporárias",
              "Lavouras perenes",
              "Café",
              "Citrus",
              "Outras Lavouras Perenes",
              "Silvicultura",
              "Mosaico Agricultura e Pastagem",
              "Praia, Duna e Areal",
              "Infraestrutura Urbana",
              "Mineração",
              "Outras Áreas não Vegetadas",
              "Rio, Lago e Oceano",
              "Aquicultura",
              "Não Observado"

            )

          )

        ## Salvar imagem
        g <- ggplot(

          StackedAreaChart_EOO_df,
          aes(

            x = Ano,
            y = Porc,
            fill = Ameaca

          )

        ) +
          geom_area() +
          scale_fill_manual(

            values = c(

              "#129912",
              "#00ff00",
              "#687537",
              "#6b9932",
              "#45C2A5",
              "#B8AF4F",
              "#968c46",
              "#665a3a",
              "#f1c232",
              "#FFD966",
              "#D5A6BD",
              "#e075ad",
              "#C27BA0",
              "#982c9e",
              "#e787f8",
              "#f3b4f1",
              "#cca0d4",
              "#d082de",
              "#cd49e4",
              "#ad4413",
              "#fff3bf",
              "#DD7E6B",
              "#aa0000",
              "#af2a2a",
              "#ff3d3d",
              "#0000FF",
              "#02106f",
              "#D5D5E5"

            )

          ) +
          theme(legend.text = element_text(size=12))

        ggsave(

          paste0("StackedArea_EOO-", i, ".png"),
          width = 12,
          height = 10,
          dpi = 500

        )


        ## Salvar imagem Natural vs. Ameaça
        StackedAreaChartNatural_EOO_df <-
          StackedAreaChart_EOO_df %>%
          dplyr::filter(Ameaca %in%
                          c(

                            "Floresta",
                            "Savana",
                            "Mangue",
                            "Restinga Arborizada",
                            "Campo Alagado",
                            "Formação Campestre",
                            "Apicum",
                            "Afloramento Rochoso",
                            "Outras Formações não Florestais",
                            "Praia, Duna e Areal",
                            "Rio, Lago e Oceano"

                          )

          )

        StackedAreaChartThreat_EOO_df <-
          StackedAreaChart_EOO_df %>%
          dplyr::filter(Ameaca %in%
                          c(

                            "Pastagem",
                            "Lavoura Temporária",
                            "Soja",
                            "Cana-de-açúcar",
                            "Arroz",
                            "Outras lavouras temporárias",
                            "Lavouras perens",
                            "Café",
                            "Citrus",
                            "Outras Lavouras Perenes",
                            "Silvicultura",
                            "Mosaico Agricultura e Pastagem",
                            "Infraestrutura Urbana",
                            "Mineração",
                            "Outras Áreas não Vegetadas",
                            "Aquicultura"

                          )

          )

        StackedAreaChartNatural_EOO_df <-
          StackedAreaChartNatural_EOO_df %>%
          group_by(Ano) %>%
          summarise(Porc = sum(Porc), Feicao = "Natural")

        StackedAreaChartThreat_EOO_df <-
          StackedAreaChartThreat_EOO_df %>%
          group_by(Ano) %>%
          summarise(Porc = sum(Porc), Feicao = "Uso Alternativo")

        StackedAreaChartNaturalThreatdf_EOO <-
          rbind(

            StackedAreaChartNatural_EOO_df,
            StackedAreaChartThreat_EOO_df

          )

        StackedAreaChartNaturalThreatdf_EOO$Feicao <-
          factor(

            StackedAreaChartNaturalThreatdf_EOO$Feicao,
            levels = c("Natural", "Uso Alternativo")

          )

        g <- ggplot(

          StackedAreaChartNaturalThreatdf_EOO,
          aes(

            x = Ano,
            y = Porc,
            fill=Feicao

          )

        ) +
          geom_area() +
          scale_fill_manual(values = c("#32CD32", "red")) +
          theme(

            legend.position="none",
            axis.text.x=element_text(size=26),
            axis.text.y=element_text(size=26),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()

          )

        ggsave(

          paste0("StackedArea_NaturalvsThreat_EOO-", i, ".png"),
          width = 12,
          height = 10,
          dpi = 500

        )

      }

    }


    # Análise de tendência: EOO ####

    output_EOO_trendline <- NULL

    input_MapBiomas <-
      MapBiomas_coverLand_results %>%
      dplyr::filter(Species == SPECIES)

    input_Ameaca <- unique(input_MapBiomas$Ameaca[-29])

    if(input_MapBiomas$Area_EOO_sites_km2_2020[1] == "Análise dispensada"){

      rm(output_EOO_trendline)
      trendEOONatural_total <- 0
      trendEOONatural_total_NOTREND <- F
      trendEOONatural_total_DOWN <- F
      trendEOONatural_total_UP <- F

      trendEOONatural_LastTrend_NOTREND <- F
      trendEOONatural_LastTrend_DOWN <- F
      trendEOONatural_LastTrend_UP <- F

      trendEOOThreat_total <- 0
      trendEOOThreat_total_NOTREND <- F
      trendEOOThreat_total_DOWN <- F
      trendEOOThreat_total_UP <- F

      trendEOOThreat_LastTrend_NOTREND <- F
      trendEOOThreat_LastTrend_DOWN <- F
      trendEOOThreat_LastTrend_UP <- F

    } else {

      ## Feições isoladas ####

      for(

        feicao in input_Ameaca

      ){

        input2 <- input_MapBiomas %>%
          dplyr::filter(Ameaca == feicao)

        input2 <- input2 %>%
          dplyr::select(contains("Porcentagem_EOO"))

        if(

          sum(input2) == 0

        ){

        } else {

          FEICAO <- feicao
          input3 <- t(input2)
          rownames(input3) <- 1:length(input3[,1])
          input4 <-
            data.frame(

              Ano = 1985:2020,
              Porcentagem_EOO = input3[,1]

            )

          png(

            file = paste0("TrendEOO-", SPECIES, "_" , feicao, ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            input4$Ano,
            input4$Porcentagem_EOO,
            model = "line2P",
            eDigit = 4,
            # main = paste0("AOO ", SPECIES,": Uso Alternativo"),
            xlab = "",
            ylab = "",
            cex.axis = 2,
            eSize = 2

          )

          dev.off()

          trendline_EOO_summary <-
            trendline_summary(

              input4$Ano,
              input4$Porcentagem_EOO,
              model = "line2P", eDigit = 4

            )

          output_EOO_trendline_ <-
            data.frame(

              Especie = SPECIES,
              Feição = FEICAO,
              Taxa_anual = round(trendline_EOO_summary$parameter$a*100/length(input4$Ano), 2)

            )

          tsfit <- trendsegment(

            x = as.numeric(input4$Porcentagem_EOO),
            minsegL = 10

          ) # Mudar para 3

          png(

            file = paste0("TrendEOO_detectTrends-", SPECIES, "_" , FEICAO, ".png"),
            height = 500,
            width = 1000

          )

          plot(

            input4,
            type = "b",
            ylim = range(as.numeric(input4$Porcentagem_EOO), tsfit$est),
            main = paste0("EOO ", SPECIES,": ", FEICAO),
            xlab = "",
            ylab = "%"

          )

          lines(input4$Ano, tsfit$est, col=2, lwd=2)

          dev.off()

          if(

            length(tsfit$cpt) == 0

          ){

            no_trend_EOO <- T
            png(

              file = paste0("TrendEOO_LastTrend-", SPECIES, "_" , FEICAO, ".png"),
              height = 500,
              width = 1000

            )

            trendline(

              input4$Ano[1:36],
              input4$Porcentagem_EOO[1:36],
              model = "line2P",
              eDigit = 4,
              main = paste0("EOO ", SPECIES,": ", FEICAO),
              sub = paste0("Última Tendência", " (1985 a 2020)"),
              xlab = "",
              ylab = "%"

            )

            dev.off()

            trendline_EOO_summary_last_trend <-
              trendline_summary(

                input4$Ano[1:36],
                input4$Porcentagem_EOO[1:36],
                model = "line2P", eDigit = 4

              )

          } else {

            no_trend_EOO <- F
            png(

              file = paste0("TrendEOO_LastTrend-", SPECIES, "_" , FEICAO, ".png"),
              height = 500,
              width = 1000

            )

            trendline(

              input4$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
              input4$Porcentagem_EOO[tsfit$cpt[length(tsfit$cpt)]:36],
              model = "line2P",
              eDigit = 4,
              main = paste0("EOO ", SPECIES,": ", FEICAO),
              sub = paste0("Última Tendência", " (", input4$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"),
              xlab = "",
              ylab = "%"

            )

            dev.off()

            trendline_EOO_summary_last_trend <-
              trendline_summary(

                input4$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
                input4$Porcentagem_EOO[tsfit$cpt[length(tsfit$cpt)]:36],
                model = "line2P",
                eDigit = 4
              )

          }

          if(

            no_trend_EOO == T

          ){

            output_EOO_trendline__ <-
              data.frame(

                Inicio_ultima_tendencia = "Sem reversão de tendência",
                Taxa_ultima_tendencia = "Sem reversão de tendência"

              )

          } else {

            output_EOO_trendline__ <-
              data.frame(

                Inicio_ultima_tendencia = input4$Ano[tsfit$cpt[length(tsfit$cpt)]],

                Taxa_ultima_tendencia = round(trendline_EOO_summary_last_trend$parameter$a*100/trendline_EOO_summary_last_trend$N,
                                              2

                )

              )

          }

          output_EOO_trendline_ <-
            cbind(output_EOO_trendline_, output_EOO_trendline__)

          output_EOO_trendline <-
            rbind(output_EOO_trendline, output_EOO_trendline_)

          output_EOO_trendline_n <- nrow(output_EOO_trendline)

          rm(no_trend_EOO)

        }

      }


      ## Feições aglutinadas: Natural ####

      png(

        file = paste0("TrendEOO-", SPECIES, "_Natural", ".png"),
        height = 500,
        width = 1000

      )

      trendline(

        StackedAreaChartNatural_EOO_df$Ano,
        StackedAreaChartNatural_EOO_df$Porc,
        model = "line2P",
        eDigit = 4,
        main = paste0("EOO ", SPECIES,": Natural"),
        xlab = "",
        ylab = "%"

      )

      dev.off()

      trendline_EOO_summary_Natural <-
        trendline_summary(
          StackedAreaChartNatural_EOO_df$Ano,
          StackedAreaChartNatural_EOO_df$Porc,
          model = "line2P",
          eDigit = 4

        )

      output_EOO_trendline_Natural_Taxa_anual <-
        data.frame(
          Taxa_anual = round(trendline_EOO_summary_Natural$parameter$a*100/length(StackedAreaChartNatural_EOO_df$Ano),
                             2

          )

        )

      tsfit <- trendsegment(

        x = as.numeric(StackedAreaChartNatural_EOO_df$Porc),
        minsegL = 10

      ) # Mudar para 3

      png(

        file = paste0("TrendEOO_detectTrends-", SPECIES, "_Natural", ".png"),
        height = 500,
        width = 1000

      )

      plot(

        StackedAreaChartNatural_EOO_df[,1:2],
        type = "b",
        ylim = range(as.numeric(StackedAreaChartNatural_EOO_df$Porc), tsfit$est),
        main = paste0("EOO ", SPECIES,": Natural"),
        xlab = "",
        ylab = "%"

      )

      lines(StackedAreaChartNatural_EOO_df$Ano, tsfit$est, col=2, lwd=2)

      dev.off()

      if(

        output_EOO_trendline_Natural_Taxa_anual == 0

      ){

        png(

          file = paste0("TrendEOO_LastTrend-", SPECIES, "_Natural", ".png"),
          height = 500,
          width = 1000

        )

        trendline(

          StackedAreaChartNatural_EOO_df$Ano,
          StackedAreaChartNatural_EOO_df$Porc,
          model = "line2P",
          eDigit = 4,
          main = paste0("EOO ", SPECIES,": Natural"),
          sub = paste0("Última Tendência", " (1985 a 2020)"),
          xlab = "",
          ylab = "%"

        )

        dev.off()

      } else {

        if(

          is.null(tsfit$cpt) == T

        ){

          png(

            file = paste0("TrendEOO_LastTrend-", SPECIES, "_Natural", ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            StackedAreaChartNatural_EOO_df$Ano,
            StackedAreaChartNatural_EOO_df$Porc,
            model = "line2P",
            eDigit = 4,
            main = paste0("EOO ", SPECIES,": Natural"),
            sub = paste0("Última Tendência", " (1985 a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

        } else {

          png(

            file = paste0("TrendEOO_LastTrend-", SPECIES, "_Natural", ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            StackedAreaChartNatural_EOO_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
            StackedAreaChartNatural_EOO_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36],
            model = "line2P",
            eDigit = 4,
            main = paste0("EOO ", SPECIES,": Natural"),
            sub = paste0("Última Tendência", " (", input4$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

        }

      }

      if(

        output_EOO_trendline_Natural_Taxa_anual == 0

      ){

        trendline_EOO_summary_Natural_LastTrend <-
          trendline_summary(

            StackedAreaChartNatural_EOO_df$Ano,
            StackedAreaChartNatural_EOO_df$Porc,
            model = "line2P",
            eDigit = 4

          )

        output_EOO_trendline_Natural_LastTrend <-
          data.frame(

            Inicio_ultima_tendencia = "Sem reversão de tendência",
            Taxa_ultima_tendencia = "Sem reversão de tendência"

          )

      } else {

        if(

          is.null(tsfit$cpt) == T

        ){

          trendline_EOO_summary_Natural_LastTrend <-
            trendline_summary(

              StackedAreaChartNatural_EOO_df$Ano,
              StackedAreaChartNatural_EOO_df$Porc,
              model = "line2P",
              eDigit = 4

            )

          output_EOO_trendline_Natural_LastTrend <-
            data.frame(

              Inicio_ultima_tendencia = as.numeric(1985),
              Taxa_ultima_tendencia = round(trendline_EOO_summary_Natural_LastTrend$parameter$a *
                                              100 /
                                              length(StackedAreaChartNatural_EOO_df$Ano),
                                            2

              )

            )

        } else {

          trendline_EOO_summary_Natural_LastTrend <-
            trendline_summary(StackedAreaChartNatural_EOO_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36], StackedAreaChartNatural_EOO_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36], model = "line2P", eDigit = 4)

          output_EOO_trendline_Natural_LastTrend <-
            data.frame(Inicio_ultima_tendencia = StackedAreaChartNatural_EOO_df$Ano[tsfit$cpt[length(tsfit$cpt)]], Taxa_ultima_tendencia = round(trendline_EOO_summary_Natural_LastTrend$parameter$a*100/length(StackedAreaChartNatural_EOO_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36]), 2))

        }

      }

      output_EOO_trendline_Natural <-
        cbind(

          output_EOO_trendline_Natural_Taxa_anual,
          output_EOO_trendline_Natural_LastTrend

        )

      ## Feições Aglutinadas: Uso Alternativo ####

      trendline_EOO_summary_Threat <-
        trendline_summary(StackedAreaChartThreat_EOO_df$Ano, StackedAreaChartThreat_EOO_df$Porc, model = "line2P", eDigit = 4)

      output_EOO_trendline_Threat_Taxa_anual <-
        data.frame(Taxa_anual = round(trendline_EOO_summary_Threat$parameter$a*100/length(StackedAreaChartThreat_EOO_df$Ano), 2))

      if(

        output_EOO_trendline_Threat_Taxa_anual == 0

      ){

        png(

          file = paste0("TrendEOO-", SPECIES, "_Threat", ".png"),
          height = 500,
          width = 1000

        )

        plot(

          StackedAreaChartThreat_EOO_df[,1:2],
          main = paste0("EOO ", SPECIES,": Uso Alternativo"),
          xlab = "",
          ylab = "%"

        )

        dev.off()

      } else {

        png(

          file = paste0("TrendEOO-", SPECIES, "_Threat", ".png"),
          height = 500,
          width = 1000

        )

        trendline(

          StackedAreaChartThreat_EOO_df$Ano,
          StackedAreaChartThreat_EOO_df$Porc,
          # main = paste0("EOO ", SPECIES,": Uso Alternativo"),
          xlab = "",
          ylab = "",
          cex.axis = 2,
          eSize = 2

        )

        dev.off()

      }

      tsfit <-
        trendsegment(

          x = as.numeric(StackedAreaChartThreat_EOO_df$Porc),
          minsegL = 10

        ) # Mudar para 3

      png(

        file = paste0("TrendEOO_detectTrends-", SPECIES, "_Threat", ".png"),
        height = 500,
        width = 1000

      )

      plot(

        StackedAreaChartThreat_EOO_df[,1:2],
        type = "b",
        ylim = range(as.numeric(StackedAreaChartThreat_EOO_df$Porc), tsfit$est),
        main = paste0("EOO ", SPECIES,": Uso Alternativo"),
        xlab = "",
        ylab = "%"

      )

      lines(StackedAreaChartThreat_EOO_df$Ano, tsfit$est, col=2, lwd=2)

      dev.off()

      if(

        output_EOO_trendline_Threat_Taxa_anual == 0

      ){

        png(

          file = paste0("TrendEOO_LastTrend-", SPECIES, "_Threat", ".png"),
          height = 500,
          width = 1000

        )

        plot(

          StackedAreaChartThreat_EOO_df[,1:2],
          model = "line2P",
          eDigit = 4,
          main = paste0("EOO ", SPECIES,": Uso Alternativo"),
          sub = paste0("Última Tendência", " (1985 a 2020)"),
          xlab = "",
          ylab = "%"

        )

        dev.off()

      } else {

        if(

          is.null(tsfit$cpt) == T

        ){

          png(

            file = paste0("TrendEOO_LastTrend-", SPECIES, "_Threat", ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            StackedAreaChartThreat_EOO_df$Ano[1:36],
            StackedAreaChartThreat_EOO_df$Porc[1:36],
            model = "line2P",
            eDigit = 4,
            main = paste0("EOO ", SPECIES,": Uso Alternativo"),
            sub = paste0("Última Tendência", " (1985 a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

        } else {

          png(

            file = paste0("TrendEOO_LastTrend-", SPECIES, "_Threat", ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            StackedAreaChartThreat_EOO_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
            StackedAreaChartThreat_EOO_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36],
            model = "line2P",
            eDigit = 4,
            main = paste0("EOO ", SPECIES,": Uso Alternativo"),
            sub = paste0("Última Tendência", " (", input4$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

        }

      }

      if(

        output_EOO_trendline_Threat_Taxa_anual == 0

      ){

        output_EOO_trendline_Threat_LastTrend <-
          data.frame(

            Inicio_ultima_tendencia = "Sem reversão de tendência",
            Taxa_ultima_tendencia = "Sem reversão de tendência"

          )

      } else {

        if(

          is.null(tsfit$cpt) == T

        ) {

          trendline_EOO_summary_Threat_LastTrend <-
            trendline_summary(

              StackedAreaChartThreat_EOO_df$Ano[1:36],
              StackedAreaChartThreat_EOO_df$Porc[1:36],
              model = "line2P",
              eDigit = 4

            )

          output_EOO_trendline_Threat_LastTrend <-
            data.frame(

              Inicio_ultima_tendencia = as.numeric(1985),
              Taxa_ultima_tendencia = round(trendline_EOO_summary_Threat_LastTrend$parameter$a*100/length(StackedAreaChartThreat_EOO_df$Ano[1:36]),
                                            2

              )

            )

        } else {

          trendline_EOO_summary_Threat_LastTrend <-
            trendline_summary(

              StackedAreaChartThreat_EOO_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
              StackedAreaChartThreat_EOO_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36],
              model = "line2P",
              eDigit = 4

            )

          output_EOO_trendline_Threat_LastTrend <-
            data.frame(

              Inicio_ultima_tendencia = StackedAreaChartThreat_EOO_df$Ano[tsfit$cpt[length(tsfit$cpt)]],
              Taxa_ultima_tendencia = round(trendline_EOO_summary_Threat_LastTrend$parameter$a*100/length(StackedAreaChartThreat_EOO_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36]),
                                            2

              )

            )

        }

      }

      output_EOO_trendline_Threat <-
        cbind(

          output_EOO_trendline_Threat_Taxa_anual,
          output_EOO_trendline_Threat_LastTrend

        )

      ## Reunir Outputs Natural e Uso Alternativo ####
      output_EOO_trendline_NaturalANDThreat <-
        rbind(

          output_EOO_trendline_Natural,
          output_EOO_trendline_Threat
        )

      output_EOO_trendline_NaturalANDThreat <-
        data.frame(

          Feições_Aglutinadas =
            c(

              "Natural",
              "Uso Alternativo"

            ),
          output_EOO_trendline_NaturalANDThreat

        )

    }


    # Gráfico de área empilhada: AOOinEOObuffer ####

    input_StackedAreaChart_AOOinEOObuffer <-
      MapBiomas_coverLand_AOOinEOObuffer_results %>%
      dplyr::filter(Species == SPECIES) %>%
      dplyr::filter(Ameaca != "Total") %>%
      dplyr::select(Ameaca, contains("Porcentagem_AOO"))

    ### Floresta
    input_StackedAreaChart_AOOinEOObuffer_floresta <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Floresta")
    input_StackedAreaChart_AOOinEOObuffer_floresta <- t(input_StackedAreaChart_AOOinEOObuffer_floresta)
    input_StackedAreaChart_AOOinEOObuffer_floresta <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_floresta)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_floresta[2:37]), Ameaca = "Floresta")

    ### Pastagem
    input_StackedAreaChart_AOOinEOObuffer_pastagem <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Pastagem")
    input_StackedAreaChart_AOOinEOObuffer_pastagem <- t(input_StackedAreaChart_AOOinEOObuffer_pastagem)
    input_StackedAreaChart_AOOinEOObuffer_pastagem <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_pastagem)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_pastagem[2:37]), Ameaca = "Pastagem")

    ### Mineração
    input_StackedAreaChart_AOOinEOObuffer_mineracao <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Mineração")
    input_StackedAreaChart_AOOinEOObuffer_mineracao <- t(input_StackedAreaChart_AOOinEOObuffer_mineracao)
    input_StackedAreaChart_AOOinEOObuffer_mineracao <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_mineracao)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_mineracao[2:37]), Ameaca = "Mineração")

    ### Soja
    input_StackedAreaChart_AOOinEOObuffer_soja <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Soja")
    input_StackedAreaChart_AOOinEOObuffer_soja <- t(input_StackedAreaChart_AOOinEOObuffer_soja)
    input_StackedAreaChart_AOOinEOObuffer_soja <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_soja)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_soja[2:37]), Ameaca = "Soja")

    ### Cana-de-açúcar
    input_StackedAreaChart_AOOinEOObuffer_cana <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Cana-de-açúcar")
    input_StackedAreaChart_AOOinEOObuffer_cana <- t(input_StackedAreaChart_AOOinEOObuffer_cana)
    input_StackedAreaChart_AOOinEOObuffer_cana <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_cana)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_cana[2:37]), Ameaca = "Cana-de-açúcar")

    ### Lavouras perenes
    input_StackedAreaChart_AOOinEOObuffer_lavouras_perenes <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Lavouras perenes")
    input_StackedAreaChart_AOOinEOObuffer_lavouras_perenes <- t(input_StackedAreaChart_AOOinEOObuffer_lavouras_perenes)
    input_StackedAreaChart_AOOinEOObuffer_lavouras_perenes <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_lavouras_perenes)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_lavouras_perenes[2:37]), Ameaca = "Lavouras perenes")

    ### Outras lavouras temporárias
    input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_temporarias <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Outras lavouras temporárias")
    input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_temporarias <- t(input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_temporarias)
    input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_temporarias <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_temporarias)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_temporarias[2:37]), Ameaca = "Outras lavouras temporárias")

    ### Mosaico Agricultura e Pastagem
    input_StackedAreaChart_AOOinEOObuffer_mosaico_agricultura_pastagem <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Mosaico Agricultura e Pastagem")
    input_StackedAreaChart_AOOinEOObuffer_mosaico_agricultura_pastagem <- t(input_StackedAreaChart_AOOinEOObuffer_mosaico_agricultura_pastagem)
    input_StackedAreaChart_AOOinEOObuffer_mosaico_agricultura_pastagem <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_mosaico_agricultura_pastagem)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_mosaico_agricultura_pastagem[2:37]), Ameaca = "Mosaico Agricultura e Pastagem")

    ### Infraestrutura Urbana
    input_StackedAreaChart_AOOinEOObuffer_infraestrutura_urbana <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Infraestrutura Urbana")
    input_StackedAreaChart_AOOinEOObuffer_infraestrutura_urbana <- t(input_StackedAreaChart_AOOinEOObuffer_infraestrutura_urbana)
    input_StackedAreaChart_AOOinEOObuffer_infraestrutura_urbana <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_infraestrutura_urbana)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_infraestrutura_urbana[2:37]), Ameaca = "Infraestrutura Urbana")

    ### Silvicultura
    input_StackedAreaChart_AOOinEOObuffer_silvicultura <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Silvicultura")
    input_StackedAreaChart_AOOinEOObuffer_silvicultura <- t(input_StackedAreaChart_AOOinEOObuffer_silvicultura)
    input_StackedAreaChart_AOOinEOObuffer_silvicultura <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_silvicultura)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_silvicultura[2:37]), Ameaca = "Silvicultura")

    ### Savana
    input_StackedAreaChart_AOOinEOObuffer_savana <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Savana")
    input_StackedAreaChart_AOOinEOObuffer_savana <- t(input_StackedAreaChart_AOOinEOObuffer_savana)
    input_StackedAreaChart_AOOinEOObuffer_savana <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_savana)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_savana[2:37]), Ameaca = "Savana")

    ### Mangue
    input_StackedAreaChart_AOOinEOObuffer_mangue <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Mangue")
    input_StackedAreaChart_AOOinEOObuffer_mangue <- t(input_StackedAreaChart_AOOinEOObuffer_mangue)
    input_StackedAreaChart_AOOinEOObuffer_mangue <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_mangue)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_mangue[2:37]), Ameaca = "Mangue")

    ### Campo Alagado e Área Pantanosa
    input_StackedAreaChart_AOOinEOObuffer_alagado_pantanosa <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Campo Alagado e Área Pantanosa")
    input_StackedAreaChart_AOOinEOObuffer_alagado_pantanosa <- t(input_StackedAreaChart_AOOinEOObuffer_alagado_pantanosa)
    input_StackedAreaChart_AOOinEOObuffer_alagado_pantanosa <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_alagado_pantanosa)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_alagado_pantanosa[2:37]), Ameaca = "Campo Alagado e Área Pantanosa")

    ### Formação Campestre
    input_StackedAreaChart_AOOinEOObuffer_campos <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Formação Campestre")
    input_StackedAreaChart_AOOinEOObuffer_campos <- t(input_StackedAreaChart_AOOinEOObuffer_campos)
    input_StackedAreaChart_AOOinEOObuffer_campos <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_campos)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_campos[2:37]), Ameaca = "Formação Campestre")

    ### Outras Formações não Florestais
    input_StackedAreaChart_AOOinEOObuffer_outras_nao_florestais <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Outras Formações não Florestais")
    input_StackedAreaChart_AOOinEOObuffer_outras_nao_florestais <- t(input_StackedAreaChart_AOOinEOObuffer_outras_nao_florestais)
    input_StackedAreaChart_AOOinEOObuffer_outras_nao_florestais <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_outras_nao_florestais)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_outras_nao_florestais[2:37]), Ameaca = "Outras Formações não Florestais")

    ### Lavoura Temporária
    input_StackedAreaChart_AOOinEOObuffer_lavouras_temporarias <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Lavoura Temporária")
    input_StackedAreaChart_AOOinEOObuffer_lavouras_temporarias <- t(input_StackedAreaChart_AOOinEOObuffer_lavouras_temporarias)
    input_StackedAreaChart_AOOinEOObuffer_lavouras_temporarias <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_lavouras_temporarias)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_lavouras_temporarias[2:37]), Ameaca = "Lavoura Temporária")

    ### Praia, Duna e Areal
    input_StackedAreaChart_AOOinEOObuffer_praia_duna_areal <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Praia, Duna e Areal")
    input_StackedAreaChart_AOOinEOObuffer_praia_duna_areal <- t(input_StackedAreaChart_AOOinEOObuffer_praia_duna_areal)
    input_StackedAreaChart_AOOinEOObuffer_praia_duna_areal <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_praia_duna_areal)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_praia_duna_areal[2:37]), Ameaca = "Praia, Duna e Areal")

    ### Outras Áreas não Vegetadas
    input_StackedAreaChart_AOOinEOObuffer_outras_nao_vegetadas <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Outras Áreas não Vegetadas")
    input_StackedAreaChart_AOOinEOObuffer_outras_nao_vegetadas <- t(input_StackedAreaChart_AOOinEOObuffer_outras_nao_vegetadas)
    input_StackedAreaChart_AOOinEOObuffer_outras_nao_vegetadas <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_outras_nao_vegetadas)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_outras_nao_vegetadas[2:37]), Ameaca = "Outras Áreas não Vegetadas")

    ### Não Observado
    input_StackedAreaChart_AOOinEOObuffer_nao_observado <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Não Observado")
    input_StackedAreaChart_AOOinEOObuffer_nao_observado <- t(input_StackedAreaChart_AOOinEOObuffer_nao_observado)
    input_StackedAreaChart_AOOinEOObuffer_nao_observado <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_nao_observado)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_nao_observado[2:37]), Ameaca = "Não Observado")

    ### Afloramento Rochoso
    input_StackedAreaChart_AOOinEOObuffer_afloramento_rochoso <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Afloramento Rochoso")
    input_StackedAreaChart_AOOinEOObuffer_afloramento_rochoso <- t(input_StackedAreaChart_AOOinEOObuffer_afloramento_rochoso)
    input_StackedAreaChart_AOOinEOObuffer_afloramento_rochoso <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_afloramento_rochoso)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_afloramento_rochoso[2:37]), Ameaca = "Afloramento Rochoso")

    ### Aquicultura
    input_StackedAreaChart_AOOinEOObuffer_aquicultura <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Aquicultura")
    input_StackedAreaChart_AOOinEOObuffer_aquicultura <- t(input_StackedAreaChart_AOOinEOObuffer_aquicultura)
    input_StackedAreaChart_AOOinEOObuffer_aquicultura <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_aquicultura)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_aquicultura[2:37]), Ameaca = "Aquicultura")

    ### Apicum
    input_StackedAreaChart_AOOinEOObuffer_apicum <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Apicum")
    input_StackedAreaChart_AOOinEOObuffer_apicum <- t(input_StackedAreaChart_AOOinEOObuffer_apicum)
    input_StackedAreaChart_AOOinEOObuffer_apicum <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_apicum)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_apicum[2:37]), Ameaca = "Apicum")

    ### Rio, Lago e Oceano
    input_StackedAreaChart_AOOinEOObuffer_rio_lago_oceano <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Rio, Lago e Oceano")
    input_StackedAreaChart_AOOinEOObuffer_rio_lago_oceano <- t(input_StackedAreaChart_AOOinEOObuffer_rio_lago_oceano)
    input_StackedAreaChart_AOOinEOObuffer_rio_lago_oceano <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_rio_lago_oceano)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_rio_lago_oceano[2:37]), Ameaca = "Rio, Lago e Oceano")

    ### Arroz
    input_StackedAreaChart_AOOinEOObuffer_arroz <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Arroz")
    input_StackedAreaChart_AOOinEOObuffer_arroz <- t(input_StackedAreaChart_AOOinEOObuffer_arroz)
    input_StackedAreaChart_AOOinEOObuffer_arroz <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_arroz)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_arroz[2:37]), Ameaca = "Arroz")

    ### Café
    input_StackedAreaChart_AOOinEOObuffer_cafe <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Café")
    input_StackedAreaChart_AOOinEOObuffer_cafe <- t(input_StackedAreaChart_AOOinEOObuffer_cafe)
    input_StackedAreaChart_AOOinEOObuffer_cafe <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_cafe)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_cafe[2:37]), Ameaca = "Café")

    ### Citrus
    input_StackedAreaChart_AOOinEOObuffer_citrus <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Citrus")
    input_StackedAreaChart_AOOinEOObuffer_citrus <- t(input_StackedAreaChart_AOOinEOObuffer_citrus)
    input_StackedAreaChart_AOOinEOObuffer_citrus <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_citrus)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_citrus[2:37]), Ameaca = "Citrus")

    ### Outras Lavouras Perenes
    input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_perenes <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Outras Lavouras Perenes")
    input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_perenes <- t(input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_perenes)
    input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_perenes <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_perenes)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_perenes[2:37]), Ameaca = "Outras Lavouras Perenes")

    ### Restinga Arborizada
    input_StackedAreaChart_AOOinEOObuffer_restinga_arborizada <- input_StackedAreaChart_AOOinEOObuffer %>% dplyr::filter(Ameaca == "Restinga Arborizada")
    input_StackedAreaChart_AOOinEOObuffer_restinga_arborizada <- t(input_StackedAreaChart_AOOinEOObuffer_restinga_arborizada)
    input_StackedAreaChart_AOOinEOObuffer_restinga_arborizada <- data.frame(Ano = sub("Porcentagem_AOO_", "", row.names(input_StackedAreaChart_AOOinEOObuffer_restinga_arborizada)[2:37]), Porc = as.numeric(input_StackedAreaChart_AOOinEOObuffer_restinga_arborizada[2:37]), Ameaca = "Restinga Arborizada")

    ## Data.frame das Feições
    StackedAreaChart_df_AOOinEOObuffer <- rbind (input_StackedAreaChart_AOOinEOObuffer_floresta,
                                                 input_StackedAreaChart_AOOinEOObuffer_pastagem,
                                                 input_StackedAreaChart_AOOinEOObuffer_mineracao,
                                                 input_StackedAreaChart_AOOinEOObuffer_soja,
                                                 input_StackedAreaChart_AOOinEOObuffer_cana,
                                                 input_StackedAreaChart_AOOinEOObuffer_lavouras_perenes,
                                                 input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_temporarias,
                                                 input_StackedAreaChart_AOOinEOObuffer_mosaico_agricultura_pastagem,
                                                 input_StackedAreaChart_AOOinEOObuffer_infraestrutura_urbana,
                                                 input_StackedAreaChart_AOOinEOObuffer_silvicultura,
                                                 input_StackedAreaChart_AOOinEOObuffer_savana,
                                                 input_StackedAreaChart_AOOinEOObuffer_mangue,
                                                 input_StackedAreaChart_AOOinEOObuffer_alagado_pantanosa,
                                                 input_StackedAreaChart_AOOinEOObuffer_campos,
                                                 input_StackedAreaChart_AOOinEOObuffer_outras_nao_florestais,
                                                 input_StackedAreaChart_AOOinEOObuffer_lavouras_temporarias,
                                                 input_StackedAreaChart_AOOinEOObuffer_praia_duna_areal,
                                                 input_StackedAreaChart_AOOinEOObuffer_outras_nao_vegetadas,
                                                 input_StackedAreaChart_AOOinEOObuffer_nao_observado,
                                                 input_StackedAreaChart_AOOinEOObuffer_afloramento_rochoso,
                                                 input_StackedAreaChart_AOOinEOObuffer_aquicultura,
                                                 input_StackedAreaChart_AOOinEOObuffer_apicum,
                                                 input_StackedAreaChart_AOOinEOObuffer_rio_lago_oceano,
                                                 input_StackedAreaChart_AOOinEOObuffer_arroz,
                                                 input_StackedAreaChart_AOOinEOObuffer_cafe,
                                                 input_StackedAreaChart_AOOinEOObuffer_citrus,
                                                 input_StackedAreaChart_AOOinEOObuffer_outras_lavouras_perenes,
                                                 input_StackedAreaChart_AOOinEOObuffer_restinga_arborizada)

    ## Organizar por Ano
    StackedAreaChart_df_AOOinEOObuffer <- StackedAreaChart_df_AOOinEOObuffer %>% arrange(Ano)
    StackedAreaChart_df_AOOinEOObuffer <- StackedAreaChart_df_AOOinEOObuffer %>% dplyr::mutate(Porc = replace_na(Porc, 0))
    StackedAreaChart_df_AOOinEOObuffer$Ano <- as.numeric(StackedAreaChart_df_AOOinEOObuffer$Ano)

    ## Converter as feições como fatores
    StackedAreaChart_df_AOOinEOObuffer$Ameaca<-factor(StackedAreaChart_df_AOOinEOObuffer$Ameaca, levels=c("Floresta", "Savana", "Mangue", "Restinga Arborizada", "Campo Alagado e Área Pantanosa", "Formação Campestre", "Apicum", "Afloramento Rochoso", "Outras Formações não Florestais", "Pastagem", "Lavoura Temporária", "Soja", "Cana-de-açúcar", "Arroz", "Outras lavouras temporárias", "Lavouras perenes", "Café", "Citrus", "Outras Lavouras Perenes", "Silvicultura", "Mosaico Agricultura e Pastagem", "Praia, Duna e Areal", "Infraestrutura Urbana", "Mineração", "Outras Áreas não Vegetadas", "Rio, Lago e Oceano", "Aquicultura", "Não Observado") )

    ## Salvar imagem
    g <- ggplot(StackedAreaChart_df_AOOinEOObuffer, aes(x=Ano, y=Porc, fill=Ameaca)) +
      geom_area() +
      scale_fill_manual(values=c("#129912", "#00ff00", "#687537", "#6b9932", "#45C2A5", "#B8AF4F", "#968c46", "#665a3a", "#f1c232", "#FFD966", "#D5A6BD", "#e075ad", "#C27BA0", "#982c9e", "#e787f8", "#f3b4f1", "#cca0d4", "#d082de", "#cd49e4", "#ad4413", "#fff3bf", "#DD7E6B", "#aa0000", "#af2a2a", "#ff3d3d", "#0000FF", "#02106f", "#D5D5E5")) +
      theme(legend.text=element_text(size=12))
    ggsave(paste0("StackedArea_AOOinEOObuffer-", SPECIES, ".png"), width = 12, height = 10, dpi = 500)

    ## Salvar imagem Natural vs. Ameaça
    StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df <- StackedAreaChart_df_AOOinEOObuffer %>% dplyr::filter(Ameaca %in% c("Floresta",
                                                                                                                                "Savana",
                                                                                                                                "Mangue",
                                                                                                                                "Restinga Arborizada",
                                                                                                                                "Campo Alagado",
                                                                                                                                "Formação Campestre",
                                                                                                                                "Apicum",
                                                                                                                                "Afloramento Rochoso",
                                                                                                                                "Outras Formações não Florestais",
                                                                                                                                "Praia, Duna e Areal",
                                                                                                                                "Rio, Lago e Oceano"))
    stackedareachartthreat_AOOinEOObuffer_df <-StackedAreaChart_df_AOOinEOObuffer %>% dplyr::filter(Ameaca %in% c("Pastagem",
                                                                                                                  "Lavoura Temporária",
                                                                                                                  "Soja",
                                                                                                                  "Cana-de-açúcar",
                                                                                                                  "Arroz",
                                                                                                                  "Outras lavouras temporárias",
                                                                                                                  "Lavouras perens",
                                                                                                                  "Café",
                                                                                                                  "Citrus",
                                                                                                                  "Outras Lavouras Perenes",
                                                                                                                  "Silvicultura",
                                                                                                                  "Mosaico Agricultura e Pastagem",
                                                                                                                  "Infraestrutura Urbana",
                                                                                                                  "Mineração",
                                                                                                                  "Outras Áreas não Vegetadas",
                                                                                                                  "Aquicultura"))

    StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df <- StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df %>% group_by(Ano) %>% summarise(Porc = sum(Porc), Feicao = "Natural")
    stackedareachartthreat_AOOinEOObuffer_df <- stackedareachartthreat_AOOinEOObuffer_df %>% group_by(Ano) %>% summarise(Porc = sum(Porc), Feicao = "Uso Alternativo")

    StackedAreaChartNaturalThreat_AOOinEOObuffer_df <- rbind(StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df, stackedareachartthreat_AOOinEOObuffer_df)
    StackedAreaChartNaturalThreat_AOOinEOObuffer_df$Feicao <- factor(StackedAreaChartNaturalThreat_AOOinEOObuffer_df$Feicao, levels = c("Natural", "Uso Alternativo"))

    g <- ggplot(StackedAreaChartNaturalThreat_AOOinEOObuffer_df, aes(x=Ano, y=Porc, fill=Feicao)) +
      geom_area() +
      scale_fill_manual(values = c("#32CD32", "red")) +
      theme(legend.text=element_text(size=12))
    ggsave(paste0("StackedArea_NaturalvsThreat_AOOinEOObuffer-", SPECIES, ".png"), width = 12, height = 10, dpi = 500)

    output_AOOinEOObuffer<-data.frame(Especie=i, Texto=Texto, Considerações=considerations)


    # Análise de tendência: AOOinEOObuffer ####

    output_AOOinEOObuffer_trendline <- NULL

    input_MapBiomas_AOOinEOObuffer <- MapBiomas_coverLand_AOOinEOObuffer_results %>%

      dplyr::filter(Species == SPECIES)

    input_Ameaca_AOOinEOObuffer <- unique(input_MapBiomas_AOOinEOObuffer$Ameaca[-29])


    ## Feições isoladas ####

    if(

      all(input_MapBiomas_AOOinEOObuffer[1,3:76] == 0)

    ){

      output_AOOinEOObuffer_trendline <-
        data.frame(

          Inicio_ultima_tendencia = "Sem reversão de tendência",
          Taxa_ultima_tendencia = "Sem reversão de tendência"

        )

      output_AOOinEOObuffer_trendline_n <- 0

    } else {

      for(

        feicao in input_Ameaca_AOOinEOObuffer

      ){

        input2_AOOinEOObuffer <- input_MapBiomas_AOOinEOObuffer %>%

          dplyr::filter(Ameaca == feicao)

        input2_AOOinEOObuffer <- input2_AOOinEOObuffer %>%

          dplyr::select(contains("Porcentagem_AOO"))

        if(

          sum(input2_AOOinEOObuffer) == 0

        ){

        } else {

          FEICAO <- feicao
          input3_AOOinEOObuffer <- t(input2_AOOinEOObuffer)
          rownames(input3_AOOinEOObuffer) <- 1:length(input3_AOOinEOObuffer[,1])
          input4_AOOinEOObuffer <- data.frame(Ano = 1985:2020, Porcentagem_AOO = input3_AOOinEOObuffer[,1])

          png(file = paste0("TrendAOOinEOObuffer-", SPECIES, "_" , feicao, ".png"), height = 500, width = 1000)
          trendline(input4_AOOinEOObuffer$Ano, input4_AOOinEOObuffer$Porcentagem_AOO, model = "line2P", eDigit = 4, main = paste0("AOOinEOObuffer ", SPECIES,": ", FEICAO), xlab = "", ylab = "%")
          dev.off()

          trendline_summary_AOOinEOObuffer <- trendline_summary(input4_AOOinEOObuffer$Ano, input4_AOOinEOObuffer$Porcentagem_AOO, model = "line2P", eDigit = 4)

          output_AOOinEOObuffer_trendline_ <- data.frame(Especie = SPECIES, Feição = FEICAO, Taxa_anual = round(trendline_summary_AOOinEOObuffer$parameter$a*100/length(input4_AOOinEOObuffer$Ano), 2))

          tsfit <- trendsegment(

            x = as.numeric(input4_AOOinEOObuffer$Porcentagem_AOO),
            minsegL = 10

          ) # Mudar para 3

          png(file = paste0("TrendAOOinEOObuffer_detectTrends-", SPECIES, "_" , FEICAO, ".png"), height = 500, width = 1000)
          plot(

            input4_AOOinEOObuffer,
            type = "b",
            ylim = range(as.numeric(input4_AOOinEOObuffer$Porcentagem_AOO), tsfit$est),
            main = paste0("AOOinEOObuffer ", SPECIES,": ", FEICAO),
            xlab = "",
            ylab = "%"

          )
          lines(input4_AOOinEOObuffer$Ano, tsfit$est, col=2, lwd=2)
          dev.off()

          if(

            length(tsfit$cpt) == 0

          ){

            no_trend_AOOinEOObuffer <- T
            png(file = paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_" , FEICAO, ".png"), height = 500, width = 1000)
            trendline(input4_AOOinEOObuffer$Ano[1:36], input4_AOOinEOObuffer$Porcentagem_AOO[1:36], model = "line2P", eDigit = 4, main = paste0("AOOinEOObuffer ", SPECIES,": ", FEICAO), sub = paste0("Última Tendência", " (1985 a 2020)"), xlab = "", ylab = "%")
            dev.off()
            trendline_summary_last_trend_AOOinEOObuffer <- trendline_summary(input4_AOOinEOObuffer$Ano[1:36], input4_AOOinEOObuffer$Porcentagem_AOO[1:36], model = "line2P", eDigit = 4)

          } else {

            no_trend_AOOinEOObuffer <- F
            png(file = paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_" , FEICAO, ".png"), height = 500, width = 1000)
            trendline(input4_AOOinEOObuffer$Ano[tsfit$cpt[length(tsfit$cpt)]:36], input4_AOOinEOObuffer$Porcentagem_AOO[tsfit$cpt[length(tsfit$cpt)]:36], model = "line2P", eDigit = 4, main = paste0("AOOinEOObuffer ", SPECIES,": ", FEICAO), sub = paste0("Última Tendência", " (", input4_AOOinEOObuffer$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"), xlab = "", ylab = "%")
            dev.off()
            trendline_summary_last_trend_AOOinEOObuffer <- trendline_summary(input4_AOOinEOObuffer$Ano[tsfit$cpt[length(tsfit$cpt)]:36], input4_AOOinEOObuffer$Porcentagem_AOO[tsfit$cpt[length(tsfit$cpt)]:36], model = "line2P", eDigit = 4)

          }

          if(

            no_trend_AOOinEOObuffer == T

          ){

            output_AOOinEOObuffer_trendline__ <- data.frame(Inicio_ultima_tendencia = "Sem reversão de tendência", Taxa_ultima_tendencia = "Sem reversão de tendência")

          } else {

            output_AOOinEOObuffer_trendline__ <- data.frame(Inicio_ultima_tendencia = input4_AOOinEOObuffer$Ano[tsfit$cpt[length(tsfit$cpt)]], Taxa_ultima_tendencia = round(trendline_summary_last_trend_AOOinEOObuffer$parameter$a*100/trendline_summary_last_trend_AOOinEOObuffer$N, 2))

          }

          output_AOOinEOObuffer_trendline_ <- cbind(output_AOOinEOObuffer_trendline_, output_AOOinEOObuffer_trendline__)

          output_AOOinEOObuffer_trendline <- rbind(output_AOOinEOObuffer_trendline, output_AOOinEOObuffer_trendline_)

          output_AOOinEOObuffer_trendline_n <- nrow(output_AOOinEOObuffer_trendline)


          rm(no_trend_AOOinEOObuffer)

        }

      }

    }



    ## Feições aglutinadas: Natural ####

    trendline_summary_Natural_AOOinEOObuffer <-
      trendline_summary(

        StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano,
        StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Porc,
        model = "line2P",
        eDigit = 4

      )

    output_trendline_Natural_AOOinEOObuffer_Taxa_anual <-
      data.frame(

        Taxa_anual = round(

          trendline_summary_Natural_AOOinEOObuffer$parameter$a *
            100 /
            length(

              StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano

            ),
          2
        )

      )
    ########### output_trendline_Natural_AOOinEOObufferLastTrend_Taxa_anual <- data.frame(Taxa_anual = round(trendline_summary_Natural_AOOinEOObuffer$parameter$a*100/length(StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano), 2))

    if(

      output_trendline_Natural_AOOinEOObuffer_Taxa_anual$Taxa_anual == 0

    ){

      plot(stackedareachartthreat_AOOinEOObuffer_df[,1:2], main = paste0("AOOinEOObuffer ", SPECIES,": Natural"), xlab = "", ylab = "%")

    } else {

      png(

        file = paste0("TrendAOOinEOObuffer-", SPECIES, "_Natural", ".png"),
        height = 500,
        width = 1000

      )

      trendline(

        StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano,
        StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Porc,
        model = "line2P",
        eDigit = 4,
        main = paste0("AOOinEOObuffer ", SPECIES,": Natural"),
        xlab = "",
        ylab = "%"

      )

      dev.off()

    }

    tsfit <-
      trendsegment(

        x = as.numeric(StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Porc),
        minsegL = 10

      ) # Mudar para 3

    png(

      file = paste0("TrendAOOinEOObuffer_detectTrends-", SPECIES, "_Natural", ".png"),
      height = 500,
      width = 1000

    )

    plot(

      StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df[,1:2],
      type = "b",
      ylim = range(as.numeric(StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Porc), tsfit$est),
      main = paste0("AOOinEOObuffer ", SPECIES,": Natural"),
      xlab = "",
      ylab = "%"

    )

    lines(

      StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano,
      tsfit$est,
      col=2,
      lwd=2

    )

    dev.off()

    if(

      output_trendline_Natural_AOOinEOObuffer_Taxa_anual == 0

    ){

      png(

        file = paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_Natural", ".png"),
        height = 500,
        width = 1000

      )

      plot(

        stackedareachartthreat_AOOinEOObuffer_df[,1:2],
        main = paste0("AOOinEOObuffer ", SPECIES,": Natural"),
        sub = "Última Tendência (1985 a 2020)",
        xlab = "",
        ylab = "%"

      )

      dev.off()

    } else {

      if(

        is.null(tsfit$cpt) == T

      ){

        png(

          file = paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_Natural", ".png"),
          height = 500,
          width = 1000

        )

        trendline(

          StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano,
          StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Porc,
          model = "line2P",
          eDigit = 4,
          main = paste0("AOOinEOObuffer ", SPECIES,": Natural"),
          sub = paste0("Última Tendência", " (1985 a 2020)"),
          xlab = "",
          ylab = "%"

        )
        dev.off()

      } else {

        png(

          file = paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_Natural", ".png"),
          height = 500,
          width = 1000

        )

        trendline(

          StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
          StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36],
          model = "line2P",
          eDigit = 4,
          main = paste0("AOO ", SPECIES,": Natural"),
          sub = paste0("Última Tendência", " (", input4_AOOinEOObuffer$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"),
          xlab = "",
          ylab = "%"

        )
        dev.off()

      }
    }

    if(

      output_trendline_Natural_AOOinEOObuffer_Taxa_anual == 0

    ){

      trendline_summary_Natural_AOOinEOObuffer_LastTrend <-
        trendline_summary(
          StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano,
          StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Porc,
          model = "line2P",
          eDigit = 4

        )

      output_trendline_Natural_AOOinEOObuffer_LastTrend <-
        data.frame(

          Inicio_ultima_tendencia = "Sem reversão de tendência",
          Taxa_ultima_tendencia = "Sem reversão de tendência"

        )

    } else {

      if(

        is.null(tsfit$cpt) == T

      ){

        trendline_summary_Natural_AOOinEOObuffer_LastTrend <-
          trendline_summary(

            StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano,
            StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Porc,
            model = "line2P",
            eDigit = 4

          )

        output_trendline_Natural_AOOinEOObuffer_LastTrend <-
          data.frame(

            Inicio_ultima_tendencia = as.numeric(1985),
            Taxa_ultima_tendencia =
              round(
                trendline_summary_Natural_AOOinEOObuffer_LastTrend$parameter$a *
                  100 /
                  length(StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano),
                2

              )

          )

      } else {

        trendline_summary_Natural_AOOinEOObuffer_LastTrend <-
          trendline_summary(
            StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
            StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36],
            model = "line2P",
            eDigit = 4

          )

        output_trendline_Natural_AOOinEOObuffer_LastTrend <-
          data.frame(

            Inicio_ultima_tendencia = StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano[tsfit$cpt[length(tsfit$cpt)]],
            Taxa_ultima_tendencia = round(trendline_summary_Natural_AOOinEOObuffer_LastTrend$parameter$a*100/length(StackedAreaChartNatural_AOOinEOObuffer_StackedArea_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36]),
                                          2

            )

          )

      }
    }

    output_trendline_Natural_AOOinEOObuffer <-
      cbind(

        output_trendline_Natural_AOOinEOObuffer_Taxa_anual,
        output_trendline_Natural_AOOinEOObuffer_LastTrend

      )

    ## Feições Aglutinadas: Uso Alternativo ####
    trendline_summary_Threat_AOOinEOObuffer <-
      trendline_summary(

        stackedareachartthreat_AOOinEOObuffer_df$Ano,
        stackedareachartthreat_AOOinEOObuffer_df$Porc,
        model = "line2P",
        eDigit = 4

      )

    output_trendline_Threat_AOOinEOObuffer_Taxa_anual <-
      data.frame(

        Taxa_anual = round(

          trendline_summary_Threat_AOOinEOObuffer$parameter$a *
            100 /
            length(stackedareachartthreat_AOOinEOObuffer_df$Ano),

          2

        )

      )

    if(

      output_trendline_Threat_AOOinEOObuffer_Taxa_anual == 0

    ){

      png(

        file = paste0("TrendAOOinEOObuffer-", SPECIES, "_Threat", ".png"),
        height = 500,
        width = 1000

      )

      plot(

        stackedareachartthreat_AOOinEOObuffer_df[,1:2],
        main = paste0("AOOinEOObuffer ", SPECIES,": Uso Alternativo"),
        xlab = "",
        ylab = "%"

      )

      dev.off()

    } else {

      png(

        file = paste0("TrendAOOinEOObuffer-", SPECIES, "_Threat", ".png"),
        height = 500,
        width = 1000

      )

      trendline(

        stackedareachartthreat_AOOinEOObuffer_df$Ano,
        stackedareachartthreat_AOOinEOObuffer_df$Porc,
        main = paste0("AOOinEOObuffer ", SPECIES,": Uso Alternativo"),
        xlab = "",
        ylab = "%"

      )

      dev.off()

    }

    tsfit <-
      trendsegment(

        x = as.numeric(stackedareachartthreat_AOOinEOObuffer_df$Porc),
        minsegL = 10

      ) # Mudar para 3

    png(

      file = paste0("TrendAOOinEOObuffer_detectTrends-", SPECIES, "_Threat", ".png"),
      height = 500,
      width = 1000

    )

    plot(

      stackedareachartthreat_AOOinEOObuffer_df[,1:2],
      type = "b",
      ylim = range(as.numeric(stackedareachartthreat_AOOinEOObuffer_df$Porc), tsfit$est),
      main = paste0("AOOinEOObuffer ", SPECIES,": Uso Alternativo"),
      xlab = "",
      ylab = "%"

    )

    lines(stackedareachartthreat_AOOinEOObuffer_df$Ano, tsfit$est, col=2, lwd=2)

    dev.off()

    if(

      output_trendline_Threat_AOOinEOObuffer_Taxa_anual == 0

    ){

      png(

        file = paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_Threat", ".png"),
        height = 500,
        width = 1000

      )

      plot(

        stackedareachartthreat_AOOinEOObuffer_df[,1:2],
        model = "line2P",
        eDigit = 4,
        main = paste0("AOOinEOObuffer ", SPECIES,": Uso Alternativo"),
        sub = paste0("Última Tendência", " (1985 a 2020)"),
        xlab = "",
        ylab = "%"

      )

      dev.off()

    } else {

      if(

        output_trendline_Threat_AOOinEOObuffer_Taxa_anual < 1

      ){

        png(

          file = paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_Threat", ".png"),
          height = 500,
          width = 1000

        )

        trendline(

          stackedareachartthreat_AOOinEOObuffer_df$Ano[1:36],
          stackedareachartthreat_AOOinEOObuffer_df$Porc[1:36],
          model = "line2P",
          eDigit = 4,
          main = paste0("AOOinEOObuffer ", SPECIES,": Uso Alternativo"),
          sub = paste0("Última Tendência", " (1985 a 2020)"),
          xlab = "",
          ylab = "%"

        )

        dev.off()

      } else {

        if(

          is.null(tsfit$cpt) == T

        ){

          png(

            file = paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_Threat", ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            stackedareachartthreat_AOOinEOObuffer_df$Ano[1:36],
            stackedareachartthreat_AOOinEOObuffer_df$Porc[1:36],
            model = "line2P",
            eDigit = 4,
            main = paste0("AOOinEOObuffer ", SPECIES,": Uso Alternativo"),
            sub = paste0("Última Tendência", " (1985 a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

        } else {

          png(

            file = paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_Threat", ".png"),
            height = 500,
            width = 1000

          )

          trendline(

            stackedareachartthreat_AOOinEOObuffer_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
            stackedareachartthreat_AOOinEOObuffer_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36],
            model = "line2P",
            eDigit = 4,
            main = paste0("AOOinEOObuffer ", SPECIES,": Uso Alternativo"),
            sub = paste0("Última Tendência", " (", input4_AOOinEOObuffer$Ano[tsfit$cpt[length(tsfit$cpt)]], " a 2020)"),
            xlab = "",
            ylab = "%"

          )

          dev.off()

        }

      }

    }

    if(

      output_trendline_Threat_AOOinEOObuffer_Taxa_anual == 0

    ){

      output_trendline_Threat_AOOinEOObuffer_LastTrend <-
        data.frame(

          Inicio_ultima_tendencia = "Sem reversão de tendência",
          Taxa_ultima_tendencia = "Sem reversão de tendência"

        )

    } else {

      if(

        output_trendline_Threat_AOOinEOObuffer_Taxa_anual < 1

      ){

        trendline_summary_Threat_AOOinEOObuffer_LastTrend <-
          trendline_summary(

            stackedareachartthreat_AOOinEOObuffer_df$Ano[1:36],
            stackedareachartthreat_AOOinEOObuffer_df$Porc[1:36],
            model = "line2P",
            eDigit = 4

          )

        output_trendline_Threat_AOOinEOObuffer_LastTrend <-
          data.frame(

            Inicio_ultima_tendencia = stackedareachartthreat_AOOinEOObuffer_df$Ano[1],
            Taxa_ultima_tendencia = round(trendline_summary_Threat_AOOinEOObuffer_LastTrend$parameter$a *
                                            100 /
                                            length(stackedareachartthreat_AOOinEOObuffer_df$Ano[1:36]),
                                          2

            )

          )

      } else {

        if(

          is.null(tsfit$cpt) == T

        ){

          trendline_summary_Threat_AOOinEOObuffer_LastTrend <-
            trendline_summary(

              stackedareachartthreat_AOOinEOObuffer_df$Ano[1:36],
              stackedareachartthreat_AOOinEOObuffer_df$Porc[1:36],
              model = "line2P",
              eDigit = 4

            )

          output_trendline_Threat_AOOinEOObuffer_LastTrend <-
            data.frame(

              Inicio_ultima_tendencia = stackedareachartthreat_AOOinEOObuffer_df$Ano[1],
              Taxa_ultima_tendencia = round(trendline_summary_Threat_AOOinEOObuffer_LastTrend$parameter$a *
                                              100 /
                                              length(stackedareachartthreat_AOOinEOObuffer_df$Ano[1:36]),
                                            2

              )

            )

        } else {

          trendline_summary_Threat_AOOinEOObuffer_LastTrend <-
            trendline_summary(

              stackedareachartthreat_AOOinEOObuffer_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36],
              stackedareachartthreat_AOOinEOObuffer_df$Porc[tsfit$cpt[length(tsfit$cpt)]:36],
              model = "line2P",
              eDigit = 4

            )

          output_trendline_Threat_AOOinEOObuffer_LastTrend <-
            data.frame(

              Inicio_ultima_tendencia = stackedareachartthreat_AOOinEOObuffer_df$Ano[tsfit$cpt[length(tsfit$cpt)]],
              Taxa_ultima_tendencia = round(trendline_summary_Threat_AOOinEOObuffer_LastTrend$parameter$a*100/length(stackedareachartthreat_AOOinEOObuffer_df$Ano[tsfit$cpt[length(tsfit$cpt)]:36]),
                                            2

              )

            )


        }

      }
    }

    output_trendline_Threat_AOOinEOObuffer <-
      cbind(

        output_trendline_Threat_AOOinEOObuffer_Taxa_anual,
        output_trendline_Threat_AOOinEOObuffer_LastTrend

      )

    ## Reunir Outputs Natural e Uso Alternativo ####

    output_trendline_NaturalANDThreat_AOOinEOObuffer <-
      rbind(

        output_trendline_Natural_AOOinEOObuffer,
        output_trendline_Threat_AOOinEOObuffer

      )

    output_trendline_NaturalANDThreat_AOOinEOObuffer <-
      data.frame(

        Feições_Aglutinadas = c("Natural", "Uso Alternativo"),
        output_trendline_NaturalANDThreat_AOOinEOObuffer
      )

    # AOO: Quadrículas do Grid (AOO_QuadOfGrid) ####

    AOO_QuadOfGrid <-
      fread(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "CNCFlora_data/outputs/trend_analyses/",
          "Trend analysis of AOO_QuadOfGrid(", OverlayAnalyses_ID, ").csv"

        ),
        header = T
        , encoding = "UTF-8"

      )

    AOO_QuadOfGrid_shapefile <-
      st_read(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "CNCFlora_data/inputs/shapefiles/MapBiomas_AOO_QuadOfGrid/",
          "AOO_QuadOfGrid ", SPECIES, ".shp"

        ),
        quiet = TRUE

      )

    AOO_QuadOfGrid2 <-
      AOO_QuadOfGrid %>%
      dplyr::filter(Especie == SPECIES)

    QuadAOO <- AOO_QuadOfGrid2 %>%
      dplyr::select(QuadAOO) %>%
      unique()

    AOO_QuadOfGrid_final <- NULL

    for(

      QuadAOO_i in QuadAOO$QuadAOO

    ){

      AOO_QuadOfGrid3 <- AOO_QuadOfGrid2 %>%
        dplyr::filter(QuadAOO == QuadAOO_i)

      AOO_QuadOfGrid4_natural <- AOO_QuadOfGrid3 %>%
        dplyr::filter(

          Ameaça %in%
            c("Floresta",
              "Savana",
              "Mangue",
              "Campo Alagado e Área Pantanosa",
              "Formação Campestre",
              "Outras Formações não Florestais",
              "Praia, Duna e Areal",
              "Afloramento Rochoso",
              "Apicum",
              "Rio, Lago e Oceano",
              "Restinga Arborizada"
            )

        ) %>%

        summarise(

          Quad = QuadAOO_i, Taxa_anual = sum(Taxa_anual)

        )

      AOO_QuadOfGrid4_threat <- AOO_QuadOfGrid3 %>%
        dplyr::filter(

          Ameaça %in%
            c(

              "Silvicultura",
              "Pastagem",
              "Lavoura Temporária",
              "Cana-de-açúcar",
              "Mosaico Agricultura e Pastagem",
              "Infraestrutura Urbana",
              "Outras Áreas não Vegetadas",
              "Mineração",
              "Aquicultura",
              "Lavouras perenes",
              "Soja",
              "Arroz",
              "Outras lavouras temporárias",
              "Café",
              "Citrus",
              "Outras Lavouras Perenes"

            )

        ) %>%
        summarise(Quad = QuadAOO_i, Taxa_anual = sum(Taxa_anual)

        )

      AOO_QuadOfGrid5 <- data.frame(

        Quad = QuadAOO_i,
        Taxa_anual = AOO_QuadOfGrid4_natural$Taxa_anual

      )

      AOO_QuadOfGrid_final <- rbind (

        AOO_QuadOfGrid_final,
        AOO_QuadOfGrid5

      )

    }

    AOO_QuadOfGrid_shapefile <- as_Spatial(AOO_QuadOfGrid_shapefile)

    ## AOO: Quadrículas do Grid (AOO_QuadOfGrid) - Proporção de conversão ####
    AOObyQuadOfGrid_results <- AOObyQuadOfGrid_results %>%
      dplyr::filter(Species == SPECIES)

    input_Quad_AOO <- unique(AOObyQuadOfGrid_results$Quad_AOO)

    output_QuadOfGrid <- NULL

    for(

      j in input_Quad_AOO

    ){

      input_Quad_AOO2 <- AOObyQuadOfGrid_results %>%
        dplyr::filter(Quad_AOO == j)

      input_Quad_AOO3_natural <- input_Quad_AOO2 %>%
        dplyr::filter(Ameaca %in%
                        c("Floresta",
                          "Savana",
                          "Mangue",
                          "Campo Alagado e Área Pantanosa",
                          "Formação Campestre",
                          "Outras Formações não Florestais",
                          "Praia, Duna e Areal",
                          "Afloramento Rochoso",
                          "Apicum",
                          "Rio, Lago e Oceano"
                        )
        )

      input_Quad_AOO3_threat <- input_Quad_AOO2 %>%
        dplyr::filter(Ameaca %in%
                        c("Silvicultura",
                          "Pastagem",
                          "Lavoura Temporária",
                          "Cana-de-açúcar",
                          "Mosaico Agricultura e Pastagem",
                          "Infraestrutura Urbana",
                          "Outras Áreas não Vegetadas",
                          "Mineração",
                          "Aquicultura",
                          "Lavoura Perene",
                          "Soja",
                          "Arroz",
                          "Outras Lavouras Temporárias",
                          "Café",
                          "Citrus",
                          "Outras Lavouras Perenes"
                        )
        )

      input_Quad_AOO4_natural <- sum(input_Quad_AOO3_natural$Porcentagem_AOO_2020)
      input_Quad_AOO4_threat <- sum(input_Quad_AOO3_threat$Porcentagem_AOO_2020)
      output_QuadOfGrid_ <- data.frame(Quad = j, Porcentagem_2020 = input_Quad_AOO4_threat)
      output_QuadOfGrid <- rbind(output_QuadOfGrid, output_QuadOfGrid_)

    }

    output_AOO_QuadOfGrid_final <-
      merge(AOO_QuadOfGrid_final, output_QuadOfGrid)

    AOO_QuadOfGrid_shapefile_order <-
      match(AOO_QuadOfGrid_shapefile$Quad, output_AOO_QuadOfGrid_final$Quad)

    output_AOO_QuadOfGrid_final <-
      output_AOO_QuadOfGrid_final[AOO_QuadOfGrid_shapefile_order,]
    row.names(output_AOO_QuadOfGrid_final) <- 1:nrow(output_AOO_QuadOfGrid_final)

    AOO_QuadOfGrid_shapefile <-
      SpatialPolygonsDataFrame(

        AOO_QuadOfGrid_shapefile,
        data = output_AOO_QuadOfGrid_final

      )

    AOO_QuadOfGrid_shapefile$Quad_Porcentagem_2020 <-
      paste0(

        AOO_QuadOfGrid_shapefile$Quad,
        " | ",
        AOO_QuadOfGrid_shapefile$Porcentagem_2020

      )

    AOO_QuadOfGrid_shapefile$Quad_Taxa_anual_2020 <-
      paste0(

        AOO_QuadOfGrid_shapefile$Quad,
        " | ",
        AOO_QuadOfGrid_shapefile$Taxa_anual

      )


    # Objetos de Controle de Fluxo ####


    ## MULTIPLE BIOMES AND VEGETATIONS ####


    ### Multiple biomes and vegetations ####

    if(

      length(biomes) >= 3 &
      length(vegetations) >= 3

    ){

      MultiplosBiomasVegetacoes <- T

    } else {

      MultiplosBiomasVegetacoes <- F

    }


    ### Multiple biomes ####

    if(

      length(biomes) >= 3

    ){

      MultiplosBiomas <- T

    } else {

      MultiplosBiomas <- F

    }


    ### Multiple vegetations ####

    if(

      length(vegetations) >= 3

    ){

      MultiplasVegetacoes <- T

    } else {

      MultiplasVegetacoes <- F

    }


    ## BIOMES ####


    ### Somente Amazônia ####

    if(

      length(biomes) == 1 &
      biomes[1] == "Amazônia"

    ){

      onlyAmazonia <- T

    } else {

      onlyAmazonia <- F

    }


    ## VEGETAÇÔES ####


    ### Somente Campos Rupestes ####

    if(

      length(vegetations) == 1 &
      vegetations[1] == "campos rupestres"

    ){

      onlyCampos_Rupestres <- T

    } else {

      onlyCampos_Rupestres <- F

    }


    ## AMPLITUDE DE DISTRIBUIÇÃO


    ### Ampla Distribuição ####

    if(

      EOO > 30000

    ){

      AmplaDistribuicao <- T

    } else {

      AmplaDistribuicao <- F

    }


    ### Distribuição Restrita ####

    if(

      EOO < 100

    ){

      DistribuicaoRestrita <- T

    } else {

      DistribuicaoRestrita <- F

    }


    ## ENQUADRAMENTO DE AOO E EOO EM CATEGORIAS DE AMEAÇAS


    ### AOO enquadra como ameaçada & EOO enquadra como ameaçada

    if(

      AOO < 2000 &
      EOO < 20000

    ){

      AOOenquadraEOOenquadra <- T

    } else {

      AOOenquadraEOOenquadra <- F

    }


    ### AOO enquadra como ameaçada e EOO não enquadra como ameaçada

    if(

      AOO < 2000 &
      EOO >= 20000

    ){

      AOOenquadraEOOnaoenquadra <- T

    } else {

      AOOenquadraEOOnaoenquadra <- F

    }


    ### AOO não enquadra como ameaçada e EOO enquadra como ameaçada

    if(

      AOO > 2000 &
      EOO < 20000

    ){

      AOOnaoenquadraEOOenquadra <- T

    } else {

      AOOnaoenquadraEOOenquadra <- F

    }


    ## ENQUADRAMENTO DE AOO E EOO EM CATEGORIAS DE AMEAÇAS & ÁREA POUCO ESTUDADA (AMAZÔNIA)


    ### AOO enquadra como ameaçada & EOO enquadra como ameaçada & Incerteza da distribuição efetiva (Amazônia)

    if(

      onlyAmazonia == T &
      AOO < 2000 &
      EOO < 20000

    ){

      AOOenquadraEOOenquadra_IncertDistEfetiv <- T

    } else {

      AOOenquadraEOOenquadra_IncertDistEfetiv <- F

    }


    ### AOO enquadra como ameaçada e EOO não enquadra como ameaçada & Incerteza da distribuição efetiva (Amazônia)

    if(

      onlyAmazonia == T &
      AOO < 2000 &
      EOO >= 20000

    ){

      AOOenquadraEOOnaoenquadra_IncertDistEfetiv <- T

    } else {

      AOOenquadraEOOnaoenquadra_IncertDistEfetiv <- F

    }


    ### AOO não enquadra como ameaçada e EOO enquadra como ameaçada & Incerteza da distribuição efetiva (Amazônia)

    if(

      onlyAmazonia == T &
      AOO > 2000 &
      EOO < 20000

    ){

      AOOnaoenquadraEOOenquadra_IncertDistEfetiv <- T

    } else {

      AOOnaoenquadraEOOenquadra_IncertDistEfetiv <- F

    }


    # Ameaças ####

    sp_in_MapBiomas_coverLand_total <- sp_in_MapBiomas_coverLand %>%
      dplyr::filter(Ameaca == "Total")

    sp_in_MapBiomas_coverLand <- sp_in_MapBiomas_coverLand %>%
      dplyr::filter(Ameaca != "Total")

    if(

      is.na(sp_in_MapBiomas_coverLand$Area_EOO_sites_km2_2020[1]) == T){

    } else {

      if(

        sp_in_MapBiomas_coverLand$Area_EOO_sites_km2_2020[1] == "Análise dispensada"

      ){

        sp_in_MapBiomas_coverLand_total <- data.frame(

          Ameaca = "Total",
          AOO_util_km2 = sp_in_MapBiomas_coverLand_total$AOO_util_km2,
          Area_AOO_sites_km2_2020 = sum(sp_in_MapBiomas_coverLand$Area_AOO_sites_km2_2020),
          Porcentagem_AOO_2020 = sum(sp_in_MapBiomas_coverLand$Porcentagem_AOO_2020),
          Area_EOO_km2 = sp_in_MapBiomas_coverLand_total$Area_EOO_km2,
          Area_EOO_sites_km2_2020 = "Análise dispensada",
          Porcentagem_EOO_2020 = NA

        )

      } else {

        sp_in_MapBiomas_coverLand_total <-
          data.frame(

            Ameaca = "Total",
            AOO_util_km2 = sp_in_MapBiomas_coverLand_total$AOO_util_km2,
            Area_AOO_sites_km2_2020 = sum(sp_in_MapBiomas_coverLand$Area_AOO_sites_km2_2020),
            Porcentagem_AOO_2020 = sum(sp_in_MapBiomas_coverLand$Porcentagem_AOO_2020),
            Area_EOO_km2 = sp_in_MapBiomas_coverLand_total$Area_EOO_km2,
            Area_EOO_sites_km2_2020 = sum(as.numeric(sp_in_MapBiomas_coverLand$Area_EOO_sites_km2_2020)),
            Porcentagem_EOO_2020 = sum(sp_in_MapBiomas_coverLand$Porcentagem_EOO_2020)

          )

      }

    }

    sp_in_MapBiomas_coverLand <-
      rbind(
        sp_in_MapBiomas_coverLand,
        sp_in_MapBiomas_coverLand_total

      )

    if(

      nrow(sp_in_MapBiomas_coverLand) > 0

    ){


      ## Nenhuma conversão para uso alternativo solo da AOO ####

      if(

        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_AOO_2020) == 0

      ){

        NenhumaConversaoUsoDoSoloAOO <- T

      } else {

        NenhumaConversaoUsoDoSoloAOO <- F

      }


      ## Nenhuma conversão para uso alternativo solo na EOO ####

      if(

        is.na(

          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_EOO_2020)

        ) == T |

        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_EOO_2020) == 0

      ){

        ConversaoUsoDoSoloEOONaoAnalisado <- T

      } else {

        ConversaoUsoDoSoloEOONaoAnalisado <- F

      }


      ## Nenhuma conversão para uso alternativo solo na AOO / EOO não analisado ####
      if(

        NenhumaConversaoUsoDoSoloAOO == T &
        ConversaoUsoDoSoloEOONaoAnalisado == T

      ){

        NenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado <- T

      } else {

        NenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado <- F

      }


      ## Baixa proporção de conversão para uso alternativo solo da AOO ####

      if(

        onlyAmazonia == F &
        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_AOO_2020) < 15

      ){

        BaixaConversaoUsoDoSoloAOO <- T

      } else {

        BaixaConversaoUsoDoSoloAOO <- F

      }


      ## Baixa proporção de conversão para uso alternativo solo da EOO ####
      if(

        ConversaoUsoDoSoloEOONaoAnalisado == F

      ){

        if(

          onlyAmazonia == F &
          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_EOO_2020) < 15

        ){

          BaixaConversaoUsoDoSoloEOO <- T

        } else {

          BaixaConversaoUsoDoSoloEOO <- F

        }

      } else {

        BaixaConversaoUsoDoSoloEOO <- F

      }


      ## Baixa proporção de conversão para uso alternativo solo da AOO e EOO ####

      if(

        onlyAmazonia == F

      ){

        if(

          nrow(sp_in_MapBiomas_coverLand) > 0

        ){

          if(

            sp_in_MapBiomas_coverLand_total %>%
            dplyr::filter(Ameaca == "Total") %>%
            dplyr::select(Porcentagem_AOO_2020) < 15 &

            (

              sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020) < 15 |

              is.na(

                sp_in_MapBiomas_coverLand %>%
                dplyr::filter(Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)) == T

            )

          ){

            BaixaConversaoUsoDoSoloAOOEOO <- T

          } else {

            BaixaConversaoUsoDoSoloAOOEOO <- F

          }

        } else {

          BaixaConversaoUsoDoSoloAOOEOO <- F

        }

      } else {

        BaixaConversaoUsoDoSoloAOOEOO <- F

      }

      ### Eliminando redundância

      if(

        BaixaConversaoUsoDoSoloAOO == T &
        BaixaConversaoUsoDoSoloEOO == T &
        BaixaConversaoUsoDoSoloAOOEOO == T

      ){

        BaixaConversaoUsoDoSoloAOO <- F
        BaixaConversaoUsoDoSoloEOO <- F

      }


      ## Baixa proporção de conversão para uso alternativo do solo da AOO na Amazônia ####

      if(

        onlyAmazonia == T &
        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_AOO_2020) < 15

      ){

        BaixaConversaoUsoDoSoloAOOAmaz <- T

      } else {

        BaixaConversaoUsoDoSoloAOOAmaz <- F

      }


      ## Baixa proporção de conversão para uso alternativo do solo da EOO na Amazônia ####

      if(

        onlyAmazonia == T &
        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_AOO_2020) < 15

      ){

        BaixaConversaoUsoDoSoloEOOAmaz <- T

      } else {

        BaixaConversaoUsoDoSoloEOOAmaz <- F

      }


      ## Baixa proporção de conversão para uso alternativo do solo da AOO e EOO na Amazônia ####

      if(

        onlyAmazonia == T

      ){

        if(

          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_AOO_2020) < 15 &
          (

            is.na(

              sp_in_MapBiomas_coverLand %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020)) < 15 |
            is.na(

              sp_in_MapBiomas_coverLand %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020)) == T

          )

        ){

          BaixaConversaoUsoDoSoloAOOEOOAmaz <- T

        } else {

          BaixaConversaoUsoDoSoloAOOEOOAmaz <- F

        }

      } else {

        BaixaConversaoUsoDoSoloAOOEOOAmaz <- F

      }

      ### Eliminando redundância

      if(

        BaixaConversaoUsoDoSoloAOOAmaz == T &
        BaixaConversaoUsoDoSoloEOOAmaz == T &
        BaixaConversaoUsoDoSoloAOOEOOAmaz == T

      ){

        BaixaConversaoUsoDoSoloAOOAmaz <- F
        BaixaConversaoUsoDoSoloEOOAmaz <- F

      }

      ## Moderada proporção de conversão para uso alternativo solo da AOO ####

      if(

        (

          onlyAmazonia == F &
          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_AOO_2020) >= 15 &
          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_AOO_2020) < 35

        ) &
        (

          is.na(

            sp_in_MapBiomas_coverLand %>%
            dplyr::filter(Ameaca == "Total") %>%
            dplyr::select(Porcentagem_EOO_2020)) == T |
          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_EOO_2020) < 15

        )

      ){

        ModeradaConversaoUsoDoSoloAOO <- T

      } else {

        ModeradaConversaoUsoDoSoloAOO <- F

      }


      ## Moderada proporção de conversão para uso alternativo solo da EOO ####

      if(

        ConversaoUsoDoSoloEOONaoAnalisado == F

      ){

        if(

          onlyAmazonia == F &
          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_EOO_2020

          ) < 10

        ){

          ModeradaConversaoUsoDoSoloEOO <- T

        } else {

          ModeradaConversaoUsoDoSoloEOO <- F

        }

      } else {

        ModeradaConversaoUsoDoSoloEOO <- F

      }


      ## Moderada proporção de conversão para uso alternativo solo da AOO e EOO ####

      if(

        nrow(sp_in_MapBiomas_coverLand) > 0

      ){

        if(

          is.na(

            sp_in_MapBiomas_coverLand$Porcentagem_EOO_2020[sp_in_MapBiomas_coverLand$Ameaca == "Total"]) == F

        ){

          if(

            (

              onlyAmazonia == F &
              sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020) >= 15 &
              sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020) < 35

            ) &
            (

              (

                sp_in_MapBiomas_coverLand_total %>%
                dplyr::filter(Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020) >= 15 &
                sp_in_MapBiomas_coverLand_total %>%
                dplyr::filter(Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020) < 35

              ) |
              is.na(

                sp_in_MapBiomas_coverLand %>%
                dplyr::filter(Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)) == T

            )

          ){

            ModeradaConversaoUsoDoSoloAOOEOO <- T

          } else {

            ModeradaConversaoUsoDoSoloAOOEOO <- F

          }

        } else {

          ModeradaConversaoUsoDoSoloAOOEOO <- F

        }

      } else {

        ModeradaConversaoUsoDoSoloAOOEOO <- F

      }

      ### Eliminando redundância

      if(

        ModeradaConversaoUsoDoSoloAOO == T &
        ModeradaConversaoUsoDoSoloEOO == T &
        ModeradaConversaoUsoDoSoloAOOEOO == T

      ){

        ModeradaConversaoUsoDoSoloAOO <- F
        ModeradaConversaoUsoDoSoloEOO <- F

      }


      ## Moderada proporção de conversão para uso alternativo do solo da AOO na Amazônia ####

      if(

        onlyAmazonia == T &
        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_AOO_2020) < 15

      ){

        ModeradaConversaoUsoDoSoloAOOAmaz <- T

      } else {

        ModeradaConversaoUsoDoSoloAOOAmaz <- F

      }


      ## Moderada proporção de conversão para uso alternativo do solo da EOO na Amazônia ####

      if(

        onlyAmazonia == T &
        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_AOO_2020) < 15

      ){

        ModeradaConversaoUsoDoSoloEOOAmaz <- T

      } else {

        ModeradaConversaoUsoDoSoloEOOAmaz <- F

      }


      ## Moderada proporção de conversão para uso alternativo do solo da AOO e EOO na Amazônia ####

      if(

        onlyAmazonia == T

      ){

        if(

          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_AOO_2020) < 15 &
          (

            is.na(

              sp_in_MapBiomas_coverLand %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020)) < 15 |
            is.na(

              sp_in_MapBiomas_coverLand %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020)) == T

          )

        ){

          ModeradaConversaoUsoDoSoloAOOEOOAmaz <- T

        } else {

          ModeradaConversaoUsoDoSoloAOOEOOAmaz <- F

        }

      } else {

        ModeradaConversaoUsoDoSoloAOOEOOAmaz <- F

      }

      ### Eliminando redundância

      if(

        ModeradaConversaoUsoDoSoloAOOAmaz == T &
        ModeradaConversaoUsoDoSoloEOOAmaz == T &
        ModeradaConversaoUsoDoSoloAOOEOOAmaz == T

      ){

        ModeradaConversaoUsoDoSoloAOOAmaz <- F
        ModeradaConversaoUsoDoSoloEOOAmaz <- F

      }


      ## Alta proporção de conversão para uso alternativo solo da AOO ####

      if(

        onlyAmazonia == F &
        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_AOO_2020) >= 35 &
        (

          is.na(

            sp_in_MapBiomas_coverLand %>% dplyr::filter(Ameaca == "Total") %>%
            dplyr::select(Porcentagem_EOO_2020)

          ) == T |
          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_EOO_2020) < 15

        )

      ){

        AltaConversaoUsoDoSoloAOO <- T

      } else {

        AltaConversaoUsoDoSoloAOO <- F

      }


      ## Alta proporção de conversão para uso alternativo solo da EOO ####

      if(

        ConversaoUsoDoSoloEOONaoAnalisado == F

      ){

        if(

          onlyAmazonia == F &
          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_EOO_2020) < 10

        ){

          AltaConversaoUsoDoSoloEOO <- T

        } else {

          AltaConversaoUsoDoSoloEOO <- F

        }

      } else {

        AltaConversaoUsoDoSoloEOO <- F

      }


      ## Alta proporção de conversão para uso alternativo solo da AOO e EOO ####

      if(

        nrow(sp_in_MapBiomas_coverLand) > 0

      ){

        if(

          is.na(

            sp_in_MapBiomas_coverLand$Porcentagem_EOO_2020[sp_in_MapBiomas_coverLand$Ameaca == "Total"]

          ) == F

        ){

          if(

            onlyAmazonia == F &
            sp_in_MapBiomas_coverLand_total %>%
            dplyr::filter(Ameaca == "Total") %>%
            dplyr::select(Porcentagem_AOO_2020) >= 35 &
            (

              sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020) >= 35 |
              is.na(

                sp_in_MapBiomas_coverLand %>%
                dplyr::filter(Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)

              ) == T

            )

          ){

            AltaConversaoUsoDoSoloAOOEOO <- T

          } else {

            AltaConversaoUsoDoSoloAOOEOO <- F

          }

        } else {

          AltaConversaoUsoDoSoloAOOEOO <- F

        }

      } else {

        AltaConversaoUsoDoSoloAOOEOO <- F

      }

      ### Eliminando redundância

      if(

        AltaConversaoUsoDoSoloAOO == T &
        AltaConversaoUsoDoSoloEOO == T &
        AltaConversaoUsoDoSoloAOOEOO == T

      ){

        AltaConversaoUsoDoSoloAOO <- F
        AltaConversaoUsoDoSoloEOO <- F

      }


      ## Alta proporção de conversão para uso alternativo do solo da AOO na Amazônia ####

      if(

        onlyAmazonia == T &
        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_AOO_2020) < 15

      ){

        AltaConversaoUsoDoSoloAOOAmaz <- T

      } else {

        AltaConversaoUsoDoSoloAOOAmaz <- F

      }


      ## Alta proporção de conversão para uso alternativo do solo da EOO na Amazônia ####

      if(

        onlyAmazonia == T &
        sp_in_MapBiomas_coverLand_total %>%
        dplyr::filter(Ameaca == "Total") %>%
        dplyr::select(Porcentagem_AOO_2020) < 15

      ){

        AltaConversaoUsoDoSoloEOOAmaz <- T

      } else {

        AltaConversaoUsoDoSoloEOOAmaz <- F

      }


      ## Alta proporção de conversão para uso alternativo do solo da AOO e EOO na Amazônia ####

      if(

        onlyAmazonia == T

      ){

        if(

          sp_in_MapBiomas_coverLand_total %>%
          dplyr::filter(Ameaca == "Total") %>%
          dplyr::select(Porcentagem_AOO_2020) < 15 &
          (

            is.na(

              sp_in_MapBiomas_coverLand %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020)) < 15 |
            is.na(

              sp_in_MapBiomas_coverLand %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020)) == T

          )

        ){

          AltaConversaoUsoDoSoloAOOEOOAmaz <- T

        } else {

          AltaConversaoUsoDoSoloAOOEOOAmaz <- F

        }

      } else {

        AltaConversaoUsoDoSoloAOOEOOAmaz <- F

      }

      ### Eliminando redundância

      if(

        AltaConversaoUsoDoSoloAOOAmaz == T &
        AltaConversaoUsoDoSoloEOOAmaz == T &
        AltaConversaoUsoDoSoloAOOEOOAmaz == T

      ){

        AltaConversaoUsoDoSoloAOOAmaz <- F
        AltaConversaoUsoDoSoloEOOAmaz <- F

      }


    } else {

      BaixaConversaoUsoDoSoloAOO <- F
      BaixaConversaoUsoDoSoloAOOEOO <- F
      BaixaConversaoUsoDoSoloEOO <- F
      BaixaConversaoUsoDoSoloAOOAmaz <- F
      BaixaConversaoUsoDoSoloEOOAmaz <- F
      BaixaConversaoUsoDoSoloAOOEOOAmaz <- F
      AltaConversaoUsoDoSoloAOOEOO <- F
      AltaConversaoUsoDoSoloAOO <- F
      AltaConversaoUsoDoSoloEOO <- F
      ModeradaConversaoUsoDoSoloAOOEOO <- F
      ModeradaConversaoUsoDoSoloAOO <- F
      ModeradaConversaoUsoDoSoloEOO <- F

    }


    ## DECLÍNIO: AOO ####
    trendNatural <- output_trendline %>%

      dplyr::filter(

        Feição %in% c("Floresta",
                      "Savana",
                      "Mangue",
                      "Restinga Arborizada",
                      "Campo Alagado",
                      "Formação Campestre",
                      "Apicum",
                      "Afloramento Rochoso",
                      "Outras Formações não Florestais",
                      "Praia, Duna e Areal",
                      "Rio, Lago e Oceano")

      )

    trendNatural_total <- output_trendline_Natural$Taxa_anual

    trendNatural_LastTrend <- output_trendline_Natural$Taxa_ultima_tendencia

    if(

      output_trendline_Natural$Taxa_ultima_tendencia == output_trendline_Natural$Taxa_anual &
      output_trendline_Natural$Inicio_ultima_tendencia == 1985

    ){

      trendNatural_LastTrend <- 0

    }

    trendThreat <-output_trendline %>%

      dplyr::filter(

        Feição %in% c("Pastagem",
                      "Lavoura Temporária",
                      "Soja",
                      "Cana-de-açúcar",
                      "Arroz",
                      "Outras lavouras temporárias",
                      "Lavouras perens",
                      "Café",
                      "Citrus",
                      "Outras Lavouras Perenes",
                      "Silvicultura",
                      "Mosaico Agricultura e Pastagem",
                      "Infraestrutura Urbana",
                      "Mineração",
                      "Outras Áreas não Vegetadas",
                      "Aquicultura")

      )

    trendThreat_total <-
      output_trendline_Threat$Taxa_anual

    trendThreat_LastTrend <-
      output_trendline_Threat$Taxa_ultima_tendencia


    ### Tendência Total Feições Naturais incremento/decrescimento ####

    if(

      trendNatural_total == 0

    ){

      trendNatural_total_NOTREND <- T

    } else {

      trendNatural_total_NOTREND <- F

    }

    if(

      trendNatural_total < 0

    ){

      trendNatural_total_DOWN <- T
      trendNatural_total_UP <- F

    } else {

      if(

        trendNatural_total > 0

      ){

        trendNatural_total_UP <- T
        trendNatural_total_DOWN <- F

      } else {

        if(

          trendNatural_total == 0

        ){

          trendNatural_total_UP <- F
          trendNatural_total_DOWN <- F

        }

      }

    }


    ### Tendência Total Feições Ameaças incremento/decrescimento ####

    if(

      trendThreat_total == 0

    ){

      trendThreat_total_NOTREND <- T

    } else {

      trendThreat_total_NOTREND <- F

    }

    if(

      trendThreat_total < 0

    ){

      trendThreat_total_DOWN <- T
      trendThreat_total_UP <- F

    } else {

      if(

        trendThreat_total > 0

      ){

        trendThreat_total_UP <- T
        trendThreat_total_DOWN <- F

      } else {

        if(

          trendThreat_total == 0

        ){

          trendThreat_total_UP <- F
          trendThreat_total_DOWN <- F

        }

      }

    }


    ### Tendência Última Tendência Feições Naturais incremento/decrescimento ####

    if(

      trendNatural_LastTrend == 0 |
      is.na(trendNatural_LastTrend) == T

    ){

      trendNatural_LastTrend_NOTREND <- T

    } else {

      trendNatural_LastTrend_NOTREND <- F

    }

    if(

      is.na(trendNatural_LastTrend) == T

    ){

      trendNatural_LastTrend_UP <- F
      trendNatural_LastTrend_DOWN <- F

    } else {

      if(

        trendNatural_LastTrend > 0 &
        trendNatural_LastTrend != 0

      ){

        trendNatural_LastTrend_UP <- T
        trendNatural_LastTrend_DOWN <- F

      } else {

        trendNatural_LastTrend_UP <- F
        trendNatural_LastTrend_DOWN <- T}

    }


    ### Tendência Última Tendência Feições Ameaças incremento/decrescimento ####

    if(

      trendThreat_LastTrend == 0 |
      is.na(trendThreat_LastTrend) == T

    ){

      trendThreat_LastTrend_NOTREND <- T

    } else {

      trendThreat_LastTrend_NOTREND <- F

    }

    if(

      trendThreat_LastTrend > 0 &
      trendThreat_LastTrend != 0

    ){

      trendThreat_LastTrend_UP <- T
      trendThreat_LastTrend_DOWN <- F

    } else {

      if(

        trendThreat_LastTrend == 0

      ){

        trendThreat_LastTrend_UP <- F
        trendThreat_LastTrend_DOWN <- F

      } else {

        trendThreat_LastTrend_UP <- F
        trendThreat_LastTrend_DOWN <- T

      }

    }


    ### Natural vs. Ameaças: Tendência Total

    if(

      trendNatural_total == trendThreat_total

    ){

      trend_Natural_Threat_total_EQUAL <- T

    } else {

      trend_Natural_Threat_total_EQUAL <- F

    }

    if(

      trendNatural_total > trendThreat_total

    ){

      trend_MORE_Natural_THAN_Threat_total <- T

    } else {

      trend_MORE_Natural_THAN_Threat_total <- F

    }

    if(

      trendNatural_total < trendThreat_total

    ){

      trend_MORE_Threat_THAN_Natural_total <- T

    } else {

      trend_MORE_Threat_THAN_Natural_total <- F

    }


    ### Natural vs. Ameaças: Última Tendência

    if(

      is.na(trendNatural_LastTrend) == T

    ){

      if(

        trendThreat_LastTrend == 0

      ){

        trend_Natural_Threat_LastTrend_EQUAL <- T

      }

    } else {

      if(

        trendNatural_LastTrend == trendThreat_LastTrend

      ){

        trend_Natural_Threat_LastTrend_EQUAL <- T

      } else {

        trend_Natural_Threat_LastTrend_EQUAL <- F

      }

    }

    if(

      is.na(trendNatural_LastTrend) == T

    ){

      if(

        trendThreat_LastTrend > 0 &
        trendThreat_LastTrend != 0

      ){

        trend_MORE_Natural_THAN_Threat_LastTrend <- T

      } else {

        trend_MORE_Natural_THAN_Threat_LastTrend <- F

      }

    } else {

      if(

        trendNatural_LastTrend > trendThreat_LastTrend

      ){

        trend_MORE_Natural_THAN_Threat_LastTrend <- T

      } else {

        trend_MORE_Natural_THAN_Threat_LastTrend <- F

      }

    }

    if(

      is.na(trendNatural_LastTrend) == T

    ){

      if(

        trendThreat_LastTrend > 0 &
        trendThreat_LastTrend != 0

      ){

        trend_MORE_Threat_THAN_Natural_LastTrend <- T

      } else {

        trend_MORE_Threat_THAN_Natural_LastTrend <- F

      }

    } else {

      if(

        trendNatural_LastTrend < trendThreat_LastTrend

      ){

        trend_MORE_Threat_THAN_Natural_LastTrend <- T

      } else {

        trend_MORE_Threat_THAN_Natural_LastTrend <- F

      }

    }


    ## DECLÍNIO: AOOinEOObuffer ####

    if(

      all(

        output_AOOinEOObuffer_trendline$Inicio_ultima_tendencia == "Sem reversão de tendência"

      ) == T

    ){

    } else {

      trendAOOinEOObufferNatural <-

        output_AOOinEOObuffer_trendline %>%
        dplyr::filter(

          Feição %in% c(

            "Floresta",
            "Savana",
            "Mangue",
            "Restinga Arborizada",
            "Campo Alagado",
            "Formação Campestre",
            "Apicum",
            "Afloramento Rochoso",
            "Outras Formações não Florestais",
            "Praia, Duna e Areal",
            "Rio, Lago e Oceano"

          )

        )

    }

    trendAOOinEOObufferNatural_total <-
      output_trendline_NaturalANDThreat_AOOinEOObuffer$Taxa_anual[1]

    trendAOOinEOObufferNatural_LastTrend <-
      output_trendline_NaturalANDThreat_AOOinEOObuffer$Taxa_ultima_tendencia[1]

    if(

      all(

        output_AOOinEOObuffer_trendline$Inicio_ultima_tendencia == "Sem reversão de tendência"

      ) == T

    ){

    } else {

      trendAOOinEOObufferThreat <-
        output_AOOinEOObuffer_trendline %>%
        dplyr::filter(

          Feição %in% c(

            "Pastagem",
            "Lavoura Temporária",
            "Soja",
            "Cana-de-açúcar",
            "Arroz",
            "Outras lavouras temporárias",
            "Lavouras perens",
            "Café",
            "Citrus",
            "Outras Lavouras Perenes",
            "Silvicultura",
            "Mosaico Agricultura e Pastagem",
            "Infraestrutura Urbana",
            "Mineração",
            "Outras Áreas não Vegetadas",
            "Aquicultura"

          )

        )

    }

    trendAOOinEOObufferThreat_total <-
      output_trendline_NaturalANDThreat_AOOinEOObuffer$Taxa_anual[2]

    trendAOOinEOObufferThreat_LastTrend <-
      output_trendline_NaturalANDThreat_AOOinEOObuffer$Taxa_ultima_tendencia[2]


    ### Tendência AOOinEOObuffer Total Feições Naturais incremento/decrescimento ####

    if(

      trendAOOinEOObufferNatural_total == 0

    ){

      trendAOOinEOObufferNatural_total_NOTREND <- T

    } else {

      trendAOOinEOObufferNatural_total_NOTREND <- F

    }

    if(

      trendAOOinEOObufferNatural_total < 0

    ){

      trendAOOinEOObufferNatural_total_DOWN <- T
      trendAOOinEOObufferNatural_total_UP <- F

    } else {

      if(

        trendAOOinEOObufferNatural_total > 0

      ){

        trendAOOinEOObufferNatural_total_UP <- T
        trendAOOinEOObufferNatural_total_DOWN <- F

      } else {

        if(

          trendAOOinEOObufferNatural_total == 0

        ){

          trendAOOinEOObufferNatural_total_UP <- F
          trendAOOinEOObufferNatural_total_DOWN <- F

        }

      }

    }


    ### Tendência AOOinEOObuffer Total Feições Ameaças incremento/decrescimento ####

    if(

      trendAOOinEOObufferThreat_total == 0

    ){

      trendAOOinEOObufferThreat_total_NOTREND <- T

    } else {

      trendAOOinEOObufferThreat_total_NOTREND <- F

    }

    if(

      trendAOOinEOObufferThreat_total < 0

    ){

      trendAOOinEOObufferThreat_total_DOWN <- T
      trendAOOinEOObufferThreat_total_UP <- F

    } else {

      if(

        trendAOOinEOObufferThreat_total > 0

      ){

        trendAOOinEOObufferThreat_total_UP <- T
        trendAOOinEOObufferThreat_total_DOWN <- F

      } else {

        if(

          trendAOOinEOObufferThreat_total == 0

        ){

          trendAOOinEOObufferThreat_total_UP <- F
          trendAOOinEOObufferThreat_total_DOWN <- F

        }

      }

    }


    ### Tendência AOOinEOObuffer Última Tendência Feições Naturais incremento/decrescimento ####

    if(

      trendAOOinEOObufferNatural_LastTrend == 0 |
      is.na(trendAOOinEOObufferNatural_LastTrend) == T

    ){

      trendAOOinEOObufferNatural_LastTrend_NOTREND <- T

    } else {

      trendAOOinEOObufferNatural_LastTrend_NOTREND <- F

    }

    if(

      is.na(trendAOOinEOObufferNatural_LastTrend) == T

    ){

      trendAOOinEOObufferNatural_LastTrend_UP <- F
      trendAOOinEOObufferNatural_LastTrend_DOWN <- F

    } else {

      if(

        trendAOOinEOObufferNatural_LastTrend > 0 &
        trendAOOinEOObufferNatural_LastTrend != 0

      ){

        trendAOOinEOObufferNatural_LastTrend_UP <- T
        trendAOOinEOObufferNatural_LastTrend_DOWN <- F

      } else {

        trendAOOinEOObufferNatural_LastTrend_UP <- F
        trendAOOinEOObufferNatural_LastTrend_DOWN <- T

      }

    }


    ### Tendência AOOinEOObuffer Última Tendência Feições Ameaças incremento/decrescimento ####

    if(

      trendAOOinEOObufferThreat_LastTrend == 0 |
      is.na(trendAOOinEOObufferThreat_LastTrend) == T

    ){

      trendAOOinEOObufferThreat_LastTrend_NOTREND <- T

    } else {

      trendAOOinEOObufferThreat_LastTrend_NOTREND <- F

    }

    if(

      trendAOOinEOObufferThreat_LastTrend > 0 &
      trendAOOinEOObufferThreat_LastTrend != 0

    ){

      trendAOOinEOObufferThreat_LastTrend_UP <- T
      trendAOOinEOObufferThreat_LastTrend_DOWN <- F

    } else {

      trendAOOinEOObufferThreat_LastTrend_UP <- F
      trendAOOinEOObufferThreat_LastTrend_DOWN <- T

    }


    ### Natural vs. Ameaças: Tendência AOOinEOObuffer Total

    if(

      trendAOOinEOObufferNatural_total == trendAOOinEOObufferThreat_total

    ){

      trendAOOinEOObuffer_Natural_Threat_total_EQUAL <- T

    } else {

      trendAOOinEOObuffer_Natural_Threat_total_EQUAL <- F

    }

    if(

      trendAOOinEOObufferNatural_total > trendAOOinEOObufferThreat_total

    ){

      trendAOOinEOObuffer_MORE_Natural_THAN_Threat_total <- T

    } else {

      trendAOOinEOObuffer_MORE_Natural_THAN_Threat_total <- F

    }

    if(

      trendAOOinEOObufferNatural_total < trendAOOinEOObufferThreat_total

    ){

      trendAOOinEOObuffer_MORE_Threat_THAN_Natural_total <- T

    } else {

      trendAOOinEOObuffer_MORE_Threat_THAN_Natural_total <- F

    }


    ### Natural vs. Ameaças: Última Tendência AOOinEOObuffer

    if(

      is.na(trendAOOinEOObufferNatural_LastTrend) == T


    ){

      if(

        trendAOOinEOObufferThreat_LastTrend == 0

      ){

        trendAOOinEOObuffer_Natural_Threat_LastTrend_EQUAL <- T

      }

    } else {

      if(

        trendAOOinEOObufferNatural_LastTrend == trendAOOinEOObufferThreat_LastTrend

      ){

        trendAOOinEOObuffer_Natural_Threat_LastTrend_EQUAL <- T

      } else {

        trendAOOinEOObuffer_Natural_Threat_LastTrend_EQUAL <- F

      }

    }

    if(

      is.na(trendAOOinEOObufferNatural_LastTrend) == T

    ){

      if(

        trendAOOinEOObufferThreat_LastTrend > 0 &
        trendAOOinEOObufferThreat_LastTrend != 0

      ){

        trendAOOinEOObuffer_MORE_Natural_THAN_Threat_LastTrend <- T

      } else {

        trendAOOinEOObuffer_MORE_Natural_THAN_Threat_LastTrend <- F

      }

    } else {

      if(

        trendAOOinEOObufferNatural_LastTrend > trendAOOinEOObufferThreat_LastTrend

      ){

        trendAOOinEOObuffer_MORE_Natural_THAN_Threat_LastTrend <- T

      } else {

        trendAOOinEOObuffer_MORE_Natural_THAN_Threat_LastTrend <- F

      }

    }

    if(

      is.na(trendAOOinEOObufferNatural_LastTrend) == T

    ){

      if(

        trendAOOinEOObufferThreat_LastTrend > 0 &
        trendAOOinEOObufferThreat_LastTrend != 0

      ){

        trendAOOinEOObuffer_MORE_Threat_THAN_Natural_LastTrend <- T

      } else {

        trendAOOinEOObuffer_MORE_Threat_THAN_Natural_LastTrend <- F

      }

    } else {

      if(

        trendAOOinEOObufferNatural_LastTrend < trendAOOinEOObufferThreat_LastTrend

      ){

        trendAOOinEOObuffer_MORE_Threat_THAN_Natural_LastTrend <- T

      } else {

        trendAOOinEOObuffer_MORE_Threat_THAN_Natural_LastTrend <- F

      }

    }


    ## DECLÍNIO: EOO ####

    if(

      exists("output_EOO_trendline") == F

    ){

    } else {

      trendEOONatural <- output_EOO_trendline %>%
        dplyr::filter(

          Feição %in% c(

            "Floresta",
            "Savana",
            "Mangue",
            "Restinga Arborizada",
            "Campo Alagado",
            "Formação Campestre",
            "Apicum",
            "Afloramento Rochoso",
            "Outras Formações não Florestais",
            "Praia, Duna e Areal",
            "Rio, Lago e Oceano"

          )

        )

      trendEOONatural_total <-
        output_EOO_trendline_NaturalANDThreat$Taxa_anual[1]

      trendEOONatural_LastTrend <-
        output_EOO_trendline_NaturalANDThreat$Taxa_ultima_tendencia[1]

      trendEOOThreat <- output_EOO_trendline %>%
        dplyr::filter(

          Feição %in% c(

            "Pastagem",
            "Lavoura Temporária",
            "Soja",
            "Cana-de-açúcar",
            "Arroz",
            "Outras lavouras temporárias",
            "Lavouras perens",
            "Café",
            "Citrus",
            "Outras Lavouras Perenes",
            "Silvicultura",
            "Mosaico Agricultura e Pastagem",
            "Infraestrutura Urbana",
            "Mineração",
            "Outras Áreas não Vegetadas",
            "Aquicultura"

          )

        )

      trendEOOThreat_total <-
        output_EOO_trendline_NaturalANDThreat$Taxa_anual[2]

      trendEOOThreat_LastTrend <-
        output_EOO_trendline_NaturalANDThreat$Taxa_ultima_tendencia[2]


      ### Tendência EOO Total Feições Naturais incremento/decrescimento ####

      if(

        trendEOONatural_total == 0

      ){

        trendEOONatural_total_NOTREND <- T

      } else {

        trendEOONatural_total_NOTREND <- F

      }

      if(

        trendEOONatural_total < 0

      ){

        trendEOONatural_total_DOWN <- T
        trendEOONatural_total_UP <- F

      } else {

        if(

          trendEOONatural_total > 0

        ){

          trendEOONatural_total_UP <- T
          trendEOONatural_total_DOWN <- F

        } else {

          if(

            trendEOONatural_total == 0

          ){

            trendEOONatural_total_UP <- F
            trendEOONatural_total_DOWN <- F

          }

        }

      }


      ### Tendência EOO Total Feições Ameaças incremento/decrescimento ####

      if(

        trendEOOThreat_total == 0

      ){

        trendEOOThreat_total_NOTREND <- T

      } else {

        trendEOOThreat_total_NOTREND <- F

      }

      if(

        trendEOOThreat_total < 0

      ){

        trendEOOThreat_total_DOWN <- T
        trendEOOThreat_total_UP <- F

      } else {

        if(

          trendEOOThreat_total > 0

        ){

          trendEOOThreat_total_UP <- T
          trendEOOThreat_total_DOWN <- F

        } else {

          if(

            trendEOOThreat_total == 0

          ){

            trendEOOThreat_total_UP <- F
            trendEOOThreat_total_DOWN <- F

          }

        }

      }


      ### Tendência EOO Última Tendência Feições Naturais incremento/decrescimento ####

      if(

        trendEOONatural_LastTrend == 0 |
        is.na(trendEOONatural_LastTrend) == T

      ){

        trendEOONatural_LastTrend_NOTREND <- T

      } else {

        trendEOONatural_LastTrend_NOTREND <- F

      }

      if(

        is.na(trendEOONatural_LastTrend) == T

      ){

        trendEOONatural_LastTrend_UP <- F
        trendEOONatural_LastTrend_DOWN <- F

      } else {

        if(

          trendEOONatural_LastTrend > 0 &
          trendEOONatural_LastTrend != 0

        ){

          trendEOONatural_LastTrend_UP <- T
          trendEOONatural_LastTrend_DOWN <- F

        } else {

          trendEOONatural_LastTrend_UP <- F
          trendEOONatural_LastTrend_DOWN <- T

        }

      }


      ### Tendência EOO Última Tendência Feições Ameaças incremento/decrescimento ####

      if(

        trendEOOThreat_LastTrend == 0 |
        is.na(trendEOOThreat_LastTrend) == T

      ){

        trendEOOThreat_LastTrend_NOTREND <- T

      } else {

        trendEOOThreat_LastTrend_NOTREND <- F

      }

      if(

        trendEOOThreat_LastTrend > 0 &
        trendEOOThreat_LastTrend != 0

      ){

        trendEOOThreat_LastTrend_UP <- T
        trendEOOThreat_LastTrend_DOWN <- F

      } else {

        trendEOOThreat_LastTrend_UP <- F
        trendEOOThreat_LastTrend_DOWN <- T

      }


      ### Natural vs. Ameaças: Tendência EOO Total

      if(

        trendEOONatural_total == trendEOOThreat_total

      ){

        trendEOO_Natural_Threat_total_EQUAL <- T

      } else {

        trendEOO_Natural_Threat_total_EQUAL <- F

      }

      if(

        trendEOONatural_total > trendEOOThreat_total

      ){

        trendEOO_MORE_Natural_THAN_Threat_total <- T

      } else {

        trendEOO_MORE_Natural_THAN_Threat_total <- F

      }

      if(

        trendEOONatural_total < trendEOOThreat_total

      ){

        trendEOO_MORE_Threat_THAN_Natural_total <- T

      } else {

        trendEOO_MORE_Threat_THAN_Natural_total <- F

      }


      ### Natural vs. Ameaças: Última Tendência EOO

      if(

        is.na(trendEOONatural_LastTrend) == T

      ){

        if(

          trendEOOThreat_LastTrend == 0

        ){

          trendEOO_Natural_Threat_LastTrend_EQUAL <- T

        }

      } else {

        if(

          trendEOONatural_LastTrend == trendEOOThreat_LastTrend

        ){

          trendEOO_Natural_Threat_LastTrend_EQUAL <- T

        } else {

          trendEOO_Natural_Threat_LastTrend_EQUAL <- F

        }

      }

      if(

        is.na(trendEOONatural_LastTrend) == T

      ){

        if(

          trendEOOThreat_LastTrend > 0 &
          trendEOOThreat_LastTrend != 0

        ){

          trendEOO_MORE_Natural_THAN_Threat_LastTrend <- T

        } else {

          trendEOO_MORE_Natural_THAN_Threat_LastTrend <- F

        }

      } else {

        if(

          trendEOONatural_LastTrend > trendEOOThreat_LastTrend

        ){

          trendEOO_MORE_Natural_THAN_Threat_LastTrend <- T

        } else {

          trendEOO_MORE_Natural_THAN_Threat_LastTrend <- F

        }

      }

      if(

        is.na(trendEOONatural_LastTrend) == T

      ){

        if(

          trendEOOThreat_LastTrend > 0 &
          trendEOOThreat_LastTrend != 0

        ){

          trendEOO_MORE_Threat_THAN_Natural_LastTrend <- T

        } else {

          trendEOO_MORE_Threat_THAN_Natural_LastTrend <- F

        }

      } else {

        if(

          trendEOONatural_LastTrend < trendEOOThreat_LastTrend

        ){

          trendEOO_MORE_Threat_THAN_Natural_LastTrend <- T

        } else {

          trendEOO_MORE_Threat_THAN_Natural_LastTrend <- F

        }

      }

    }


    # Labels ####


    ## Amplamente Distribuída ####


    ### Labels: Amplamente Distribuída ####

    labelAmplamenteDistribuida =
      "sua ampla distribuição"


    ## Distribuição restrita ####


    ### Labels: Distribuição restrita ####

    labelDistribuicaoRestrita =
      "sua distribuição restrita"


    ## Área Conhecida ####


    ### Labels: Área Conhecida ####

    labelAreaConhecida1 =
      "sua área de ocorrência relativamente bem amostrada"

    labelAreaConhecida2 =
      "sua distribuição em região pouco estudada, onde esforços de coleta ainda são necessários"


    ### Condicionais ####


    #### Condicionais: Biomas ####

    if(

      onlyAmazonia == T

    ){

      labelAreaConhecida2 =
        "<markG>sua distribuição em região pouco estudada, onde esforços de coleta ainda são necessários</markG>"

    }


    #### Condicionais: Vegetações ####

    if(

      onlyCampos_Rupestres == T

    ){

      labelAreaConhecida2 =
        "<markG>sua distribuição em região pouco estudada, onde esforços de coleta ainda são necessários</markG>"

    }


    ## AMEAÇAS ####


    ### Labels ####

    if(

      NenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado == T

    ){

      labelNenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado =
        "que não há áreas convertidas para uso alternativo do solo na AOO"

    }

    if(

      BaixaConversaoUsoDoSoloAOOEOO == T

    ){

      labelBaixaConversaoAOOEOO1 =
        "a baixa proporção de áreas convertidas para uso alternativo do solo na AOO e na EOO"

      labelBaixaConversaoAOOEOO2 =
        paste0(

          'a baixa proporção de áreas convertidas para uso alternativo do solo na AOO (',
          sub(
            "\\.",
            ",",
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%) e EOO ('

          )
          ,
          if(

            is.na(sp_in_MapBiomas_coverLand %>%
                  dplyr::filter (Ameaca == "Total") %>%
                  dplyr::select(Porcentagem_AOO_2020)

            ) == F

          ){

            sub(
              "\\.",
              ",",
              sp_in_MapBiomas_coverLand_total %>%
                dplyr::filter (Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)

            )

          } else {

            "não calculado"

          },
          '), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a>'

        )

      labelBaixaConversaoAOOEOOAmaz1 =
        "a baixa proporção de áreas convertidas para uso alternativo do solo, não apenas na AOO e na EOO, mas também à nível regional"

      labelBaixaConversaoAOOEOOAmaz2 =
        paste0(

          'a baixa proporção de áreas convertidas para uso alternativo do solo, não apenas na AOO (',
          sub(
            "\\.",
            ",",
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020)

          ),
          '%) e EOO (',
          if(

            is.na(

              sub(
                "\\.",
                ",",
                sp_in_MapBiomas_coverLand %>%
                dplyr::filter (Ameaca == "Total") %>%
                dplyr::select(Porcentagem_AOO_2020)

              )

            ) == F

          ){

            sub(
              "\\.",
              ",",
              sp_in_MapBiomas_coverLand_total %>%
                dplyr::filter (Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)

            )

          } else {

            "não calculado"

          },
          '%), mas também à nível regional, em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a>'

        )

    }

    if(

      ModeradaConversaoUsoDoSoloAOOEOO == T

    ){

      labelModeradaConversaoAOOEOO =
        paste0(

          'a moderada proporção de áreas convertidas para uso alternativo do solo na AOO (',
          sub(
            "\\.",
            ",",
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020)

          ),
          '%) e na EOO (',
          sub(
            "\\.",
            ",",
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020)

          ),
          '%), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a>'

        )

      labelModeradaConversaoAOO =
        paste0(

          'a moderada proporção de áreas convertidas para uso alternativo do solo na AOO (',
          sub(
            "\\.",
            ",",
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020)

          ),
          '%), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a>'

        )

    }

    if(

      AltaConversaoUsoDoSoloAOOEOO == T

    ){

      labelAltaConversaoAOOEOO =
        paste0(

          'a alta proporção de áreas convertidas para uso alternativo do solo na AOO (',
          sub(
            "\\.",
            ",",
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020)

          ),
          '%) e na EOO (',
          sub(
            "\\.",
            ",",
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020)

          ),
          '%), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a>'

        )

    }

    if(

      AltaConversaoUsoDoSoloAOO == T

    ){

      labelAltaConversaoAOO =
        paste0(

          'a alta proporção de áreas convertidas para uso alternativo do solo na AOO (',
          sub(
            "\\.",
            ",",
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020)

          ),
          '%), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a>'

        )

    }


    ### DECLÍNIO: AOO ####


    #### Values ####

    valueTrendDeclinioNaturalAOO =
      paste0(

        "a tendência de perda de áreas naturais na AOO (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_Natural_Taxa_anual

        ),
        "% ao ano) desde 1985 até 2020, decorrente do crescimento da conversão de áreas para uso alternativo do solo (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_Threat_Taxa_anual

        ),
        "% ao ano), que resulta em um declínio contínuo observado da qualidade do habitat"

      )

    valueTrendIncrementoNaturalAOO =
      paste0(

        "a tendência de ganho de áreas naturais na AOO (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_Natural_Taxa_anual

        ),
        "% ao ano) desde 1985 até 2020, com a consequente diminuição de áreas convertidas para uso alternativo do solo (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_Threat_Taxa_anual

        ),
        "% ao ano)"

      )

    valueNoTrendAOO =
      "a tendência nula de conversão de áreas naturais para uso alternativo do solo na AOO desde 1985 até 2020"


    #### Labels ####

    labelTrendDeclinioNaturalAOO = valueTrendDeclinioNaturalAOO

    labelTrendDeclinioNaturalAOO =
      paste0(

        "<markG>",
        labelTrendDeclinioNaturalAOO,
        "</markG>"

      )

    labelTrendDeclinioNaturalAOO =
      sub(

        "AOO",
        "<b>AOO</b>",
        labelTrendDeclinioNaturalAOO

      )

    labelTrendDeclinioNaturalAOO =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelTrendDeclinioNaturalAOO

      )

    labelTrendDeclinioNaturalAOO =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelTrendDeclinioNaturalAOO

      )

    labelTrendIncrementoNaturalAOO = valueTrendIncrementoNaturalAOO

    labelTrendIncrementoNaturalAOO =
      paste0(

        "<markG>",
        labelTrendIncrementoNaturalAOO,
        "</markG>"

      )

    labelTrendIncrementoNaturalAOO =
      sub(

        "AOO",
        "<b>AOO</b>",
        labelTrendIncrementoNaturalAOO

      )

    labelTrendIncrementoNaturalAOO =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelTrendIncrementoNaturalAOO

      )

    labelTrendIncrementoNaturalAOO =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelTrendIncrementoNaturalAOO

      )

    labelNoTrendAOO = valueNoTrendAOO

    labelNoTrendAOO =
      paste0(

        "<markG>",
        labelNoTrendAOO,
        "</markG>"

      )

    labelNoTrendAOO =
      sub(

        "AOO",
        "<b>AOO</b>",
        labelNoTrendAOO

      )

    labelNoTrendAOO =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelNoTrendAOO

      )

    labelNoTrendAOO =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelNoTrendAOO

      )


    ### DECLÍNIO: AOOLastTrend ####


    #### Values ####

    valueTrendDeclinioNaturalAOOLastTrend =
      paste0(

        "a última tendência de perda de áreas naturais na AOO (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_NaturalANDThreat$Taxa_ultima_tendencia[1]

        ),
        "% ao ano) desde ",
        output_trendline_NaturalANDThreat$Inicio_ultima_tendencia[1],
        " até 2020, decorrente do crescimento da conversão de áreas para uso alternativo do solo (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_NaturalANDThreat$Taxa_ultima_tendencia[2]

        ),
        "% ao ano), que resulta em um declínio contínuo observado da qualidade do habitat"

      )

    valueTrendIncrementoNaturalAOOLastTrend =
      paste0(

        "a última tendência de ganho de áreas naturais na AOO (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_NaturalANDThreat$Taxa_ultima_tendencia[1]

        ),
        "% ao ano) desde ",
        output_trendline_NaturalANDThreat$Inicio_ultima_tendencia[1],
        " até 2020, com a consequente diminuição de áreas convertidas para uso alternativo do solo (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_NaturalANDThreat$Taxa_ultima_tendencia[2]

        ),
        "% ao ano)"

      )

    valueNoTrendAOOLastTrend =
      paste0(

        "a ausência de reversão de tendência na AOO desde ",
        output_trendline_NaturalANDThreat$Inicio_ultima_tendencia[1],
        " até 2020"

      )

    #### Labels ####
    labelTrendDeclinioNaturalAOOLastTrend = valueTrendDeclinioNaturalAOOLastTrend

    labelTrendDeclinioNaturalAOOLastTrend =
      paste0(

        "<markG>",
        labelTrendDeclinioNaturalAOOLastTrend,
        "</markG>"

      )

    labelTrendDeclinioNaturalAOOLastTrend =
      sub(

        "AOO",
        "<b>AOO</b>",
        labelTrendDeclinioNaturalAOOLastTrend

      )

    labelTrendDeclinioNaturalAOOLastTrend =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelTrendDeclinioNaturalAOOLastTrend

      )

    labelTrendDeclinioNaturalAOOLastTrend =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelTrendDeclinioNaturalAOOLastTrend

      )

    labelTrendIncrementoNaturalAOOLastTrend = valueTrendIncrementoNaturalAOOLastTrend

    labelTrendIncrementoNaturalAOOLastTrend =
      paste0(

        "<markG>",
        labelTrendIncrementoNaturalAOOLastTrend,
        "</markG>"

      )

    labelTrendIncrementoNaturalAOOLastTrend =
      sub(

        "AOO",
        "<b>AOO</b>",
        labelTrendIncrementoNaturalAOOLastTrend

      )

    labelTrendIncrementoNaturalAOOLastTrend =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelTrendIncrementoNaturalAOOLastTrend

      )

    labelTrendIncrementoNaturalAOOLastTrend =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelTrendIncrementoNaturalAOOLastTrend

      )

    labelNoTrendAOOLastTrend = valueNoTrendAOOLastTrend

    labelNoTrendAOOLastTrend =
      paste0(

        "<markG>",
        labelNoTrendAOOLastTrend,
        "</markG>"

      )

    labelNoTrendAOOLastTrend =
      sub(

        "AOO",
        "<b>AOO</b>",
        labelNoTrendAOOLastTrend

      )

    labelNoTrendAOOLastTrend =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelNoTrendAOOLastTrend

      )

    labelNoTrendAOOLastTrend =
      sub(
        'perda',
        '<b style="color: red">perda</b>',
        labelNoTrendAOOLastTrend

      )


    ### DECLÍNIO: AOOinEOObuffer ####


    #### Values ####

    valueTrendDeclinioNaturalAOOinEOObuffer =
      paste0(

        "a tendência de perda de áreas naturais na AOO em região do perímetro da EOO (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_Natural_AOOinEOObuffer_Taxa_anual

        ),
        "% ao ano) desde 1985 até 2020, decorrente do crescimento da conversão de áreas para uso alternativo do solo (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_Threat_AOOinEOObuffer_Taxa_anual

        ),
        "% ao ano), que resulta em um declínio contínuo observado da EOO decorrente da redução da qualidade do habitat na região de seu perímetro"

      )

    valueTrendIncrementoNaturalAOOinEOObuffer =
      paste0(

        "a tendência de ganho de áreas naturais na AOO em região do perímetro da EOO (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_Natural_AOOinEOObuffer_Taxa_anual

        ),
        "% ao ano) desde 1985 até 2020, com a consequente diminuição de áreas convertidas para uso alternativo do solo (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_Threat_AOOinEOObuffer_Taxa_anual

        ),
        "% ao ano)"

      )

    valueNoTrendAOOinEOObuffer =
      "a tendência nula de conversão de áreas naturais na AOO em região do perímetro da EOO para uso alternativo do solo desde 1985 até 2020"


    #### Labels ####

    labelTrendDeclinioNaturalAOOinEOObuffer = valueTrendDeclinioNaturalAOOinEOObuffer

    labelTrendDeclinioNaturalAOOinEOObuffer =
      paste0(

        "<markG>",
        labelTrendDeclinioNaturalAOOinEOObuffer,
        "</markG>"

      )

    labelTrendDeclinioNaturalAOOinEOObuffer =
      sub(

        "AOO em região do perímetro da EOO",
        "<b>AOO em região do perímetro da EOO</b>",
        labelTrendDeclinioNaturalAOOinEOObuffer

      )

    labelTrendDeclinioNaturalAOOinEOObuffer =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelTrendDeclinioNaturalAOOinEOObuffer

      )

    labelTrendDeclinioNaturalAOOinEOObuffer =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelTrendDeclinioNaturalAOOinEOObuffer

      )

    labelTrendIncrementoNaturalAOOinEOObuffer = valueTrendIncrementoNaturalAOOinEOObuffer

    labelTrendIncrementoNaturalAOOinEOObuffer =
      paste0(

        "<markG>",
        labelTrendIncrementoNaturalAOOinEOObuffer,
        "</markG>"

      )

    labelTrendIncrementoNaturalAOOinEOObuffer =
      sub(

        "AOO em região do perímetro da EOO",
        "<b>AOO em região do perímetro da EOO</b>",
        labelTrendIncrementoNaturalAOOinEOObuffer

      )

    labelTrendIncrementoNaturalAOOinEOObuffer =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelTrendIncrementoNaturalAOOinEOObuffer

      )

    labelTrendIncrementoNaturalAOOinEOObuffer =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelTrendIncrementoNaturalAOOinEOObuffer

      )

    labelNoTrendAOOinEOObuffer = valueNoTrendAOOinEOObuffer

    labelNoTrendAOOinEOObuffer =
      paste0(

        "<markG>",
        labelNoTrendAOOinEOObuffer,
        "</markG>"

      )

    labelNoTrendAOOinEOObuffer =
      sub(

        "AOO em região do perímetro da EOO",
        "<b>AOO em região do perímetro da EOO</b>",
        labelNoTrendAOOinEOObuffer

      )

    labelNoTrendAOOinEOObuffer =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelNoTrendAOOinEOObuffer

      )

    labelNoTrendAOOinEOObuffer =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelNoTrendAOOinEOObuffer

      )


    ### DECLÍNIO: AOOinEOObuffer LastTrend ####


    #### Values ####

    valueTrendDeclinioNaturalAOOinEOObufferLastTrend =
      paste0(

        "a última tendência de perda de áreas naturais na AOO em região do perímetro da EOO (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_NaturalANDThreat_AOOinEOObuffer$Taxa_ultima_tendencia[1]

        ),
        "% ao ano) desde ",
        output_trendline_NaturalANDThreat_AOOinEOObuffer$Inicio_ultima_tendencia[1],
        " até 2020, decorrente do crescimento da conversão de áreas para uso alternativo do solo (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_NaturalANDThreat_AOOinEOObuffer$Taxa_ultima_tendencia[2]

        ),
        "% ao ano), que resulta em um declínio contínuo observado da EOO decorrente da redução da qualidade do habitat na região de seu perímetro"

      )

    valueTrendIncrementoNaturalAOOinEOObufferLastTrend =
      paste0(

        "a última tendência de ganho de áreas naturais na AOO em região do perímetro da EOO (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_NaturalANDThreat_AOOinEOObuffer$Taxa_ultima_tendencia[1]

        ),
        "% ao ano) desde ",
        output_trendline_NaturalANDThreat_AOOinEOObuffer$Inicio_ultima_tendencia[1],
        " até 2020, com a consequente diminuição de áreas convertidas para uso alternativo do solo (a uma taxa de ",
        sub(

          "\\.",
          ",",
          output_trendline_NaturalANDThreat_AOOinEOObuffer$Taxa_ultima_tendencia[2]

        ),
        "% ao ano)"

      )

    valueNoTrendAOOinEOObufferLastTrend =
      "a ausência de reversão de tendência de conversão de áreas naturais na AOO em região do perímetro da EOO para uso alternativo do solo desde 1985 até 2020"


    #### Labels ####

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend = valueTrendDeclinioNaturalAOOinEOObufferLastTrend

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend =
      paste0(

        "<markG>",
        labelTrendDeclinioNaturalAOOinEOObufferLastTrend,
        "</markG>"

      )

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend =
      sub(

        "AOO em região do perímetro da EOO",
        "<b>AOO em região do perímetro da EOO</b>",
        labelTrendDeclinioNaturalAOOinEOObufferLastTrend

      )

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelTrendDeclinioNaturalAOOinEOObufferLastTrend

      )

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelTrendDeclinioNaturalAOOinEOObufferLastTrend

      )

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend = valueTrendDeclinioNaturalAOOinEOObufferLastTrend

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend =
      paste0(

        "<markG>",
        labelTrendDeclinioNaturalAOOinEOObufferLastTrend,
        "</markG>"

      )

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend =
      sub(

        "AOO em região do perímetro da EOO",
        "<b>AOO em região do perímetro da EOO</b>",
        labelTrendDeclinioNaturalAOOinEOObufferLastTrend

      )

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelTrendDeclinioNaturalAOOinEOObufferLastTrend

      )

    labelTrendDeclinioNaturalAOOinEOObufferLastTrend =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelTrendDeclinioNaturalAOOinEOObufferLastTrend

      )

    labelTrendIncrementoNaturalAOOinEOObufferLastTrend = valueTrendIncrementoNaturalAOOinEOObufferLastTrend

    labelTrendIncrementoNaturalAOOinEOObufferLastTrend =
      paste0(

        "<markG>",
        labelTrendIncrementoNaturalAOOinEOObufferLastTrend,
        "</markG>"

      )

    labelTrendIncrementoNaturalAOOinEOObufferLastTrend =
      sub(

        "AOO em região do perímetro da EOO",
        "<b>AOO em região do perímetro da EOO</b>",
        labelTrendIncrementoNaturalAOOinEOObufferLastTrend

      )

    labelTrendIncrementoNaturalAOOinEOObufferLastTrend =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelTrendIncrementoNaturalAOOinEOObufferLastTrend

      )

    labelTrendIncrementoNaturalAOOinEOObufferLastTrend =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelTrendIncrementoNaturalAOOinEOObufferLastTrend

      )

    labelNoTrendAOOinEOObufferLastTrend = valueNoTrendAOOinEOObufferLastTrend

    labelNoTrendAOOinEOObufferLastTrend =
      paste0(

        "<markG>",
        labelNoTrendAOOinEOObufferLastTrend,
        "</markG>"

      )

    labelNoTrendAOOinEOObufferLastTrend =
      sub(

        "AOO em região do perímetro da EOO",
        "<b>AOO em região do perímetro da EOO</b>",
        labelNoTrendAOOinEOObufferLastTrend

      )

    labelNoTrendAOOinEOObufferLastTrend =
      sub(

        'ganho',
        '<b style="color: blue">ganho</b>',
        labelNoTrendAOOinEOObufferLastTrend

      )

    labelNoTrendAOOinEOObufferLastTrend =
      sub(

        'perda',
        '<b style="color: red">perda</b>',
        labelNoTrendAOOinEOObufferLastTrend

      )


    ### DECLÍNIO: EOO ####

    if(

      exists("output_EOO_trendline") == F

    ){

    } else {

      #### Values ####

      valueTrendDeclinioNaturalEOO =
        paste0(

          "a tendência de perda de áreas naturais na EOO (a uma taxa de ",
          sub(

            "\\.",
            ",",
            output_EOO_trendline_Natural_Taxa_anual

          ),
          "% ao ano) desde 1985 até 2020, decorrente do crescimento da conversão de áreas para uso alternativo do solo (a uma taxa de ",
          sub(

            "\\.",
            ",",
            output_EOO_trendline_Threat_Taxa_anual

          ),
          "% ao ano)"

        )

      valueTrendIncrementoNaturalEOO =
        paste0(

          "a tendência de ganho de áreas naturais na EOO (a uma taxa de ",
          output_EOO_trendline_Natural_Taxa_anual,
          "% ao ano) desde 1985 até 2020, com a consequente diminuição de áreas convertidas para uso alternativo do solo (a uma taxa de ",
          sub(

            "\\.",
            ",",
            output_EOO_trendline_Threat_Taxa_anual

          ),
          "% ao ano)"

        )

      valueNoTrendEOO =
        "a tendência nula de conversão de áreas naturais para uso alternativo do solo na EOO desde 1985 até 2020"


      #### Labels ####

      labelTrendDeclinioNaturalEOO = valueTrendDeclinioNaturalEOO

      labelTrendDeclinioNaturalEOO =
        paste0(

          "<markG>",
          labelTrendDeclinioNaturalEOO,
          "</markG>"

        )

      labelTrendDeclinioNaturalEOO =
        sub(

          "EOO",
          "<b>EOO</b>",
          labelTrendDeclinioNaturalEOO

        )

      labelTrendDeclinioNaturalEOO =
        sub(

          'ganho',
          '<b style="color: blue">ganho</b>',
          labelTrendDeclinioNaturalEOO

        )

      labelTrendDeclinioNaturalEOO =
        sub(

          'perda',
          '<b style="color: red">perda</b>',
          labelTrendDeclinioNaturalEOO

        )

      labelTrendIncrementoNaturalEOO =
        valueTrendIncrementoNaturalEOO

      labelTrendIncrementoNaturalEOO =
        paste0(

          "<markG>",
          labelTrendIncrementoNaturalEOO,
          "</markG>"

        )

      labelTrendIncrementoNaturalEOO =
        sub(

          "EOO",
          "<b>EOO</b>",
          labelTrendIncrementoNaturalEOO

        )

      labelTrendIncrementoNaturalEOO =
        sub(

          'ganho',
          '<b style="color: blue">ganho</b>',
          labelTrendIncrementoNaturalEOO

        )

      labelTrendIncrementoNaturalEOO =
        sub(

          'perda',
          '<b style="color: red">perda</b>',
          labelTrendIncrementoNaturalEOO

        )

      labelNoTrendEOO = valueNoTrendEOO

      if(

        trendThreat_total_NOTREND == T

      ){

        labelNoTrendEOO =
          paste0(

            "<markG>",
            labelNoTrendEOO,
            "</markG>"

          )

        labelNoTrendEOO =
          sub(

            "EOO",
            "<b>EOO</b>",
            labelNoTrendEOO

          )

        labelNoTrendEOO =
          sub(

            'ganho',
            '<b style="color: blue">ganho</b>',
            labelNoTrendEOO

          )

        labelNoTrendEOO =
          sub(

            'perda',
            '<b style="color: red">perda</b>',
            labelNoTrendEOO

          )

      }


      ### DECLÍNIO: EOOLastTrend ####


      #### Values ####

      valueTrendDeclinioNaturalEOOLastTrend =
        paste0(

          "a última tendência de perda de áreas naturais na EOO (a uma taxa de ",
          output_EOO_trendline_NaturalANDThreat$Taxa_ultima_tendencia[1],
          "% ao ano) desde ",
          output_EOO_trendline_NaturalANDThreat$Inicio_ultima_tendencia[1],
          " até 2020, decorrente do crescimento da conversão de áreas para uso alternativo do solo (a uma taxa de ",
          sub(

            "\\.",
            ",",
            output_EOO_trendline_NaturalANDThreat$Taxa_ultima_tendencia[2]

          ),
          "% ao ano)"

        )

      valueTrendIncrementoNaturalEOOLastTrend =
        paste0(

          "a última tendência de ganho de áreas naturais na EOO (a uma taxa de ",
          sub(

            "\\.",
            ",",
            output_EOO_trendline_NaturalANDThreat$Taxa_ultima_tendencia[1]

          ),
          "% ao ano) desde ",
          output_EOO_trendline_NaturalANDThreat$Inicio_ultima_tendencia[1],
          " até 2020, com a consequente diminuição de áreas convertidas para uso alternativo do solo (a uma taxa de ",
          sub(

            "\\.",
            ",",
            output_EOO_trendline_NaturalANDThreat$Taxa_ultima_tendencia[2]

          ),
          "% ao ano)"

        )

      valueNoTrendEOOLastTrend =
        paste0(

          "a ausência de reversão de tendência na EOO desde ",
          output_EOO_trendline_NaturalANDThreat$Inicio_ultima_tendencia[1],
          " até 2020"

        )


      #### Labels ####

      labelTrendDeclinioNaturalEOOLastTrend =
        valueTrendDeclinioNaturalEOOLastTrend

      labelTrendDeclinioNaturalEOOLastTrend =
        paste0(

          "<markG>",
          labelTrendDeclinioNaturalEOOLastTrend,
          "</markG>"

        )

      labelTrendDeclinioNaturalEOOLastTrend =
        sub(

          "EOO",
          "<b>EOO</b>",
          labelTrendDeclinioNaturalEOOLastTrend

        )

      labelTrendDeclinioNaturalEOOLastTrend =
        sub(

          'ganho',
          '<b style="color: blue">ganho</b>',
          labelTrendDeclinioNaturalEOOLastTrend

        )

      labelTrendDeclinioNaturalEOOLastTrend =
        sub(

          'perda',
          '<b style="color: red">perda</b>',
          labelTrendDeclinioNaturalEOOLastTrend

        )

      labelTrendIncrementoNaturalEOOLastTrend = valueTrendIncrementoNaturalEOOLastTrend

      labelTrendIncrementoNaturalEOOLastTrend =
        paste0(

          "<markG>",
          labelTrendIncrementoNaturalEOOLastTrend,
          "</markG>"
        )

      labelTrendIncrementoNaturalEOOLastTrend =
        sub(

          "EOO",
          "<b>EOO</b>",
          labelTrendIncrementoNaturalEOOLastTrend

        )

      labelTrendIncrementoNaturalEOOLastTrend =
        sub(

          'ganho',
          '<b style="color: blue">ganho</b>',
          labelTrendIncrementoNaturalEOOLastTrend

        )

      labelTrendIncrementoNaturalEOOLastTrend =
        sub(

          'perda',
          '<b style="color: red">perda</b>',
          labelTrendIncrementoNaturalEOOLastTrend

        )

      labelNoTrendEOOLastTrend = valueNoTrendEOOLastTrend

      labelNoTrendEOOLastTrend =
        paste0(

          "<markG>",
          labelNoTrendEOOLastTrend,
          "</markG>"

        )

      labelNoTrendEOOLastTrend =
        sub(

          "EOO",
          "<b>EOO</b>",
          labelNoTrendEOOLastTrend

        )

      labelNoTrendEOOLastTrend =
        sub(

          'ganho',
          '<b style="color: blue">ganho</b>',
          labelNoTrendEOOLastTrend

        )

      labelNoTrendEOOLastTrend =
        sub(

          'perda',
          '<b style="color: red">perda</b>',
          labelNoTrendEOOLastTrend

        )

    }


    ## CATEGORIA FINAL ####


    ### Labels ####

    labelcategoriaCR =
      "Criticamente Em Perigo (CR)"

    labelcategoriaEN =
      "Em Perigo (EN)"

    labelcategoriaVU =
      "Vulnerável (VU)"

    labelcategoriaLC =
      "Menos Preocupante (LC)"

    labelcategoriaDD =
      "Dados Insuficientes (DD)"

    labelcategoriaNT =
      "Quase Ameaçada (NT)"


    ## Orações subordinadas concessivas e adversativas ####


    ### Values ####


    #### Incerteza da distribuição efetiva ####

    valueIncertDistEfetiv =
      "Além disso, a espécie foi pouco coletada na área de sua ocorrência, não havendo certeza sobre sua distribuição efetiva. "


    #### Área Pouco Estudada + incerteza da distribuição efetiva ####

    valueAreaPoucoEstudada_incertDistEfetiv =
      "Além disso, a área de ocorrência é pouco estudada e possivelmente a espécie foi pouco coletada, não havendo certeza sobre sua distribuição efetiva. "


    #### AOO/EOO como ameaçada - locations ####

    valueApesarAOOenquadraEOOnaoenquadra_locations =
      "Apesar de o valor de AOO poder categorizá-la como ameaçada e do declínio contínuo verificado, o número de situações de ameaças excede os limiares das categorias de ameaçadas. "

    valueApesarAOOnaoenquadraEOOenquadra_locations =
      "Apesar de o valor de EOO poder categorizá-la como ameaçada e do declínio contínuo verificado, o número de situações de ameaças excede os limiares das categorias de ameaçadas. "

    valueApesarAOOenquadraEOOenquadra_locations =
      "Apesar de os valores de AOO e EOO poderem categorizá-la como ameaçada e do declínio contínuo verificado, o número de situações de ameaças excede os limiares das categorias de ameaçadas. "


    #### AOO/EOO como ameaçada - locations não ameaçadas (NaoAme) ####

    valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme =
      "Apesar de o valor de AOO e do número de situações de ameaças poderem categorizá-la como ameaçada, não se verificou declínio contínuo da AOO, EOO ou da qualidade do habitat. "

    valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme =
      "Apesar de o valor de EOO e do número de situações de ameaças poderem categorizá-la como ameaçada, não se verificou declínio contínuo da AOO, EOO ou da qualidade do habitat. "

    valueApesarAOOenquadraEOOenquadra_locationsNaoAme =
      "Apesar de o valor de AOO e EOO e do número de situações de ameaças poderem categorizá-la como ameaçada, não se verificou declínio contínuo da AOO, EOO ou da qualidade do habitat. "


    ##### AOO/EOO como ameaçada - locations + Proporção de conversão ####

    if(

      AltaConversaoUsoDoSoloAOOEOO == T |
      ModeradaConversaoUsoDoSoloAOOEOO == T |
      AltaConversaoUsoDoSoloAOO == T |
      ModeradaConversaoUsoDoSoloAOO == T |
      AltaConversaoUsoDoSoloEOO == T |
      ModeradaConversaoUsoDoSoloEOO == T

    ){

      if(

        AltaConversaoUsoDoSoloAOO == T

      ){

        valueApesarAOOenquadraEOOnaoenquadra_locations =
          sub(

            "categorizá-la como ameaçada e do declínio contínuo verificado, o número",
            "categorizá-la como ameaçada, da alta proporção de áreas convertidas para uso alternativo do solo na AOO e do declínio contínuo verificado, o número",
            valueApesarAOOenquadraEOOnaoenquadra_locations

          )

        valueApesarAOOnaoenquadraEOOenquadra_locations =
          sub(

            "categorizá-la como ameaçada e do declínio contínuo verificado, o número",
            "categorizá-la como ameaçada, da alta proporção de áreas convertidas para uso alternativo do solo na AOO e do declínio contínuo verificado, o número",
            valueApesarAOOnaoenquadraEOOenquadra_locations

          )

        valueApesarAOOenquadraEOOenquadra_locations =
          sub(

            "categorizá-la como ameaçada e do declínio contínuo verificado, o número",
            "categorizá-la como ameaçada, da alta proporção de áreas convertidas para uso alternativo do solo na AOO e do declínio contínuo verificado, o número",
            valueApesarAOOenquadraEOOenquadra_locations

          )

      } else {

        if(

          ModeradaConversaoUsoDoSoloAOO == T

        ){

          valueApesarAOOenquadraEOOnaoenquadra_locations =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na AOO, o número",
              valueApesarAOOenquadraEOOnaoenquadra_locations

            )

          valueApesarAOOnaoenquadraEOOenquadra_locations =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na AOO, o número",
              valueApesarAOOnaoenquadraEOOenquadra_locations

            )

          valueApesarAOOenquadraEOOenquadra_locations =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na AOO, o número",
              valueApesarAOOenquadraEOOenquadra_locations

            )

        }

      }

      if(

        AltaConversaoUsoDoSoloEOO == T

      ){

        valueApesarAOOenquadraEOOnaoenquadra_locations =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
            valueApesarAOOenquadraEOOnaoenquadra_locations

          )

        valueApesarAOOnaoenquadraEOOenquadra_locations =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
            valueApesarAOOnaoenquadraEOOenquadra_locations

          )

        valueApesarAOOenquadraEOOenquadra_locations =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
            valueApesarAOOenquadraEOOenquadra_locations

          )

      } else {

        if(

          ModeradaConversaoUsoDoSoloEOO == T

        ){

          valueApesarAOOenquadraEOOnaoenquadra_locations =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
              valueApesarAOOenquadraEOOnaoenquadra_locations

            )

          valueApesarAOOnaoenquadraEOOenquadra_locations =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
              valueApesarAOOnaoenquadraEOOenquadra_locations

            )

          valueApesarAOOenquadraEOOenquadra_locations =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
              valueApesarAOOenquadraEOOenquadra_locations

            )

        }

      }

      if(

        AltaConversaoUsoDoSoloAOOEOO == T

      ){

        valueApesarAOOenquadraEOOnaoenquadra_locations =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
            valueApesarAOOenquadraEOOnaoenquadra_locations

          )

        valueApesarAOOnaoenquadraEOOenquadra_locations =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
            valueApesarAOOnaoenquadraEOOenquadra_locations

          )

        valueApesarAOOenquadraEOOenquadra_locations =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
            valueApesarAOOenquadraEOOenquadra_locations

          )

      } else {

        if(

          ModeradaConversaoUsoDoSoloAOOEOO == T

        ){

          valueApesarAOOenquadraEOOnaoenquadra_locations =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
              valueApesarAOOenquadraEOOnaoenquadra_locations

            )

          valueApesarAOOnaoenquadraEOOenquadra_locations =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
              valueApesarAOOnaoenquadraEOOenquadra_locations

            )

          valueApesarAOOenquadraEOOenquadra_locations =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
              valueApesarAOOenquadraEOOenquadra_locations

            )

        }
      }

    } else {

      valueApesarAOOenquadraEOOnaoenquadra_locations =
        valueApesarAOOenquadraEOOnaoenquadra_locations

      valueApesarAOOnaoenquadraEOOenquadra_locations =
        valueApesarAOOnaoenquadraEOOenquadra_locations

      valueApesarAOOenquadraEOOenquadra_locations =
        valueApesarAOOenquadraEOOenquadra_locations

    }


    ##### AOO/EOO como ameaçada - locations não ameaçadas (NaoAme) + Proporção de conversão ####

    if(

      AltaConversaoUsoDoSoloAOOEOO == T |
      ModeradaConversaoUsoDoSoloAOOEOO == T |
      AltaConversaoUsoDoSoloAOO == T |
      ModeradaConversaoUsoDoSoloAOO == T |
      AltaConversaoUsoDoSoloEOO == T |
      ModeradaConversaoUsoDoSoloEOO == T

    ){

      if(

        AltaConversaoUsoDoSoloAOO == T

      ){

        valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme =
          sub(

            "poderem categorizá-la como ameaçada, ",
            "poderem categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO, ",
            valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme

          )

        valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme =
          sub(

            "poderem categorizá-la como ameaçada, ",
            "poderem categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na EOO, ",
            valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme

          )

        valueApesarAOOenquadraEOOenquadra_locationsNaoAme =
          sub(

            "poderem categorizá-la como ameaçada, ",
            "poderem categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, ",
            valueApesarAOOenquadraEOOenquadra_locationsNaoAme

          )

      } else {

        if(

          ModeradaConversaoUsoDoSoloAOO == T

        ){

          valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme =
            sub(

              "poderem categorizá-la como ameaçada, ",
              "poderem categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO, ",
              valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme

            )

          valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme =
            sub(

              "poderem categorizá-la como ameaçada, ",
              "poderem categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na EOO, ",
              valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme

            )

          valueApesarAOOenquadraEOOenquadra_locationsNaoAme =
            sub(

              "poderem categorizá-la como ameaçada, ",
              "poderem categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, ",
              valueApesarAOOenquadraEOOenquadra_locationsNaoAme

            )

        }

      }

      if(

        AltaConversaoUsoDoSoloEOO == T

      ){

        valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
            valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme

          )

        valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
            valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme

          )

        valueApesarAOOenquadraEOOenquadra_locationsNaoAme =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
            valueApesarAOOenquadraEOOenquadra_locationsNaoAme

          )

      } else {

        if(

          ModeradaConversaoUsoDoSoloEOO == T

        ){

          valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
              valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme

            )

          valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
              valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme

            )

          valueApesarAOOenquadraEOOenquadra_locationsNaoAme =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na EOO, o número",
              valueApesarAOOenquadraEOOenquadra_locationsNaoAme

            )

        }

      }

      if(

        AltaConversaoUsoDoSoloAOOEOO == T

      ){

        valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
            valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme

          )

        valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
            valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme

          )

        valueApesarAOOenquadraEOOenquadra_locationsNaoAme =
          sub(

            "categorizá-la como ameaçada, o número",
            "categorizá-la como ameaçada e da alta proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
            valueApesarAOOenquadraEOOenquadra_locationsNaoAme

          )

      } else {

        if(

          ModeradaConversaoUsoDoSoloAOOEOO == T

        ){

          valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
              valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme

            )

          valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
              valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme

            )

          valueApesarAOOenquadraEOOenquadra_locationsNaoAme =
            sub(

              "categorizá-la como ameaçada, o número",
              "categorizá-la como ameaçada e da moderada proporção de áreas convertidas para uso alternativo do solo na AOO e EOO, o número",
              valueApesarAOOenquadraEOOenquadra_locationsNaoAme

            )

        }
      }

    } else {

      valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme =
        valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme

      valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme =
        valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme

      valueApesarAOOenquadraEOOenquadra_locationsNaoAme =
        valueApesarAOOenquadraEOOenquadra_locationsNaoAme

    }


    ##### AOO/EOO como ameaçada - locations + Proporção de conversão + Declínio contínuo ausente ####
    valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusente =
      sub(

        "o número de situações de ameaças excede os limiares das categorias de ameaçadas",
        "o número de situações de ameaças excede os limiares das categorias de ameaçadas e não se constatou declínio contínuo da AOO, EOO ou da qualidade do habitat",
        valueApesarAOOenquadraEOOnaoenquadra_locations

      )

    valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusente =
      sub(

        " e do declínio contínuo verificado, ",
        ", ",
        valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusente

      )

    valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusente =
      sub(

        "o número de situações de ameaças excede os limiares das categorias de ameaçadas",
        "o número de situações de ameaças excede os limiares das categorias de ameaçadas e não se constatou declínio contínuo da AOO, EOO ou da qualidade do habitat",
        valueApesarAOOnaoenquadraEOOenquadra_locations

      )

    valueApesarAOOenquadraEOOenquadra_locations_declinioAusente =
      sub(

        "o número de situações de ameaças excede os limiares das categorias de ameaçadas",
        "o número de situações de ameaças excede os limiares das categorias de ameaçadas e não se constatou declínio contínuo da AOO, EOO ou da qualidade do habitat",
        valueApesarAOOenquadraEOOenquadra_locations

      )


    #### CR Quase DD ####
    if(

      collectionsNumber < 5

    ){

      if(

        collectionsNumber == 1

      ){

        valueCRQuaseDDBemAmostradaAmeacadaAntropiz =
          "A espécie poderia ser categorizada como Dados Insuficientes (DD) pela ausência de dados disponíveis sobre sua distribuição e populações, porém adotando-se o princípio da precaução, optou-se por categorizá-la como CR, haja vista que seu único registro de ocorrência localiza-se em uma região bem amostrada e, paralelamente, ameaçada por atividades antrópicas. "

      } else {

        valueCRQuaseDDBemAmostradaAmeacadaAntropiz =
          paste0(

            "A espécie poderia ser categorizada como Dados Insuficientes (DD) pela ausência de dados disponíveis sobre sua distribuição e populações, porém adotando-se o princípio da precaução, optou-se por categorizá-la como CR, haja vista que seus ",
            collectionsNumber,
            " registros de ocorrência localizam-se em uma região bem amostrada e, paralelamente, ameaçada por atividades antrópicas. "

          )

      }

    }


    ### Labels ####


    #### AOO/EOO como ameaçada - incerteza da distribuição efetiva ####

    labelIncertDistEfetiv =
      sub(

        "\\.\\s$",
        "",
        valueIncertDistEfetiv

      )


    #### AOO/EOO como ameaçada - Área Pouco Estudada - incerteza da distribuição efetiva ####

    labelAreaPoucoEstudada_incertDistEfetiv =
      sub(

        "\\.\\s$",
        "",
        valueAreaPoucoEstudada_incertDistEfetiv

      )


    #### AOO/EOO como ameaçada - locations ####

    labelApesarAOOenquadraEOOnaoenquadra_locations =
      sub(

        "\\.\\s$",
        "",
        valueApesarAOOenquadraEOOnaoenquadra_locations

      )

    labelApesarAOOnaoenquadraEOOenquadra_locations =
      sub(

        "\\.\\s$",
        "",
        valueApesarAOOnaoenquadraEOOenquadra_locations

      )

    labelApesarAOOenquadraEOOenquadra_locations =
      sub(

        "\\.\\s$",
        "",
        valueApesarAOOenquadraEOOenquadra_locations

      )


    #### AOO/EOO como nao ameaçada - locations ####

    labelApesarAOOenquadraEOOnaoenquadra_locationsNaoAme =
      sub(

        "\\.\\s$",
        "",
        valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme

      )

    labelApesarAOOnaoenquadraEOOenquadra_locationsNaoAme =
      sub(

        "\\.\\s$",
        "",
        valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme

      )

    labelApesarAOOenquadraEOOenquadra_locationsNaoAme =
      sub(

        "\\.\\s$",
        "",
        valueApesarAOOenquadraEOOenquadra_locationsNaoAme

      )


    #### AOO/EOO como ameaçada - locations + declínio contínuo ausente ####

    labelApesarAOOenquadraEOOnaoenquadra_locations_declinioAusente =
      sub(

        "\\.\\s$",
        "",
        valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusente

      )

    labelApesarAOOnaoenquadraEOOenquadra_locations_declinioAusente =
      sub(

        "\\.\\s$",
        "",
        valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusente

      )

    labelApesarAOOenquadraEOOenquadra_locations_declinioAusente =
      sub(

        "\\.\\s$",
        "",
        valueApesarAOOenquadraEOOenquadra_locations_declinioAusente

      )


    #### CR Quase DD ####

    if(

      collectionsNumber < 5

    ){

      if(

        collectionsNumber == 1

      ){

        labelCRQuaseDDBemAmostradaAmeacadaAntropiz =
          "A espécie poderia ser categorizada como Dados Insuficientes (DD) pela ausência de dados disponíveis sobre sua distribuição e populações, porém adotando-se o princípio da precaução, optou-se por categorizá-la como CR, haja vista que seu único registro de ocorrência localiza-se em uma região bem amostrada e, paralelamente, ameaçada por atividades antrópicas."

      } else {

        labelCRQuaseDDBemAmostradaAmeacadaAntropiz =
          paste0("A espécie poderia ser categorizada como Dados Insuficientes (DD) pela ausência de dados disponíveis sobre sua distribuição e populações, porém adotando-se o princípio da precaução, optou-se por categorizá-la como CR, haja vista que seus ", collectionsNumber, " registros de ocorrência localizam-se em uma região bem amostrada e, paralelamente, ameaçada por atividades antrópicas.")

      }

    }

    ## Recomendações ####

    ### Labels ####

    labelRecomNecesMaisColetas =
      "Recomendam-se trabalhos de campo direcionados para recoletá-la em sua distribuição conhecida e em outras localidades próximas, visando ampliar o conhecimento de sua distribuição, e para coletar dados populacionais"

    labelRecomNecesMaisColetasNaoRelacUsoSolo =
      "Recomendam-se trabalhos de campo direcionados para recoletá-la em sua distribuição conhecida e em outras localidades próximas, visando ampliar o conhecimento de sua distribuição, para coletar dados populacionais, e verificar possíveis ameaças diretas não relacionadas ao uso alternativo do solo"

    labelAposEsforcosCRouEX =
      "Recomendam-se trabalhos de campo direcionados para recoletá-la em sua distribuição conhecida e em outras localidades próximas, visando ampliar o conhecimento de sua distribuição, e para coletar dados populacionais. Após esforços consistentes direcionados à encontrá-la na natureza, seria possível considerá-la como Criticamente em Perigo (CR) ou Exinta (EX)"

    labelRecomEstudosPopul =
      "Recomendam-se estudos populacionais e monitoramento da espécie"


    ### Condicionais ####

    #### Condicionais: Baixa proporção de conversão para uso alternativo do solo da AOO e EOO ####

    if(

      NenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado == T

    ){

      labelNenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado =
        "<markG>que não há áreas convertidas para uso alternativo do solo na AOO</markG>"

      valueNenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado =
        "que não há áreas convertidas para uso alternativo do solo na AOO"

    }

    if(

      exists("BaixaConversaoUsoDoSoloAOOEOO") == T

    ){

      if(

        BaixaConversaoUsoDoSoloAOOEOO == T

      ){

        labelBaixaConversaoUsoDoSoloAOOEOO1 =
          "<markG>a baixa proporção de áreas convertidas para uso alternativo do solo na AOO e na EOO</markG>"

        labelBaixaConversaoAOOEOO1 <- labelBaixaConversaoUsoDoSoloAOOEOO1

        valueBaixaConversaoAOOEOO1 =
          "a baixa proporção de áreas convertidas para uso alternativo do solo na AOO e na EOO"

        labelBaixaConversaoUsoDoSoloAOOEOO2 =
          paste0(

            '<markG>a baixa proporção de áreas convertidas para uso alternativo do solo na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter(Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%) e EOO (',
            if(

              is.na(

                sp_in_MapBiomas_coverLand %>%
                dplyr::filter(Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)

              ) == F

            ){

              sp_in_MapBiomas_coverLand_total %>%
                dplyr::filter(Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)

            } else {

              "não calculado"

            },

            '), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a></markG>'

          )
        labelBaixaConversaoAOOEOO2 <- labelBaixaConversaoUsoDoSoloAOOEOO2

        valueBaixaConversaoAOOEOO2 =
          paste0(

            'a baixa proporção de áreas convertidas para uso alternativo do solo não apenas na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%) e na EOO (',
            if(

              is.na(sp_in_MapBiomas_coverLand %>%
                    dplyr::filter (Ameaca == "Total") %>%
                    dplyr::select(Porcentagem_EOO_2020)

              ) == F

            ){

              sp_in_MapBiomas_coverLand_total %>%
                dplyr::filter (Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)

            } else {

              "não calculado"

            },
            '), em 2020, na qual foi possível contabilizar x situações de ameaça das quais x situações_ameaça são afetadas pela conversão de áreas para atividades de '

          )

      }

    }

    #### Condicionais: Baixa proporção de conversão para uso alternativo do solo da AOO e EOO na Amazônia ####
    if(

      exists("BaixaConversaoUsoDoSoloAOOEOOAmaz") == T

    ){

      if(

        BaixaConversaoUsoDoSoloAOOEOOAmaz == T

      ){

        labelBaixaConversaoUsoDoSoloAOOEOOAmaz1 =
          "<markG>a baixa proporção de áreas convertidas para uso alternativo do solo, não apenas na AOO e na EOO, mas também à nível regional</markG>"

        labelBaixaConversaoAOOEOOAmaz1 <- labelBaixaConversaoUsoDoSoloAOOEOOAmaz1

        valueBaixaConversaoAOOEOOAmaz1 =
          "a baixa proporção de áreas convertidas para uso alternativo do solo, não apenas na AOO e na EOO, mas também à nível regional"

        labelBaixaConversaoUsoDoSoloAOOEOOAmaz2 =
          paste0(

            '<markG>a baixa proporção de áreas convertidas para uso alternativo do solo, não apenas na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%) e EOO (',
            if(

              is.na(sp_in_MapBiomas_coverLand %>%
                    dplyr::filter (Ameaca == "Total") %>%
                    dplyr::select(Porcentagem_EOO_2020)

              ) == F

            ){

              sp_in_MapBiomas_coverLand_total %>%
                dplyr::filter (Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)

            } else {

              "não calculado"

            },
            '%), mas também à nível regional, em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a></markG>'

          )

        labelBaixaConversaoAOOEOOAmaz2 <-
          labelBaixaConversaoUsoDoSoloAOOEOOAmaz2

        valueBaixaConversaoAOOEOOAmaz2 =
          paste0(

            'a baixa proporção de áreas convertidas para uso alternativo do solo não apenas na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%) e na EOO (',
            if(

              is.na(sp_in_MapBiomas_coverLand %>%
                    dplyr::filter (Ameaca == "Total") %>%
                    dplyr::select(Porcentagem_EOO_2020)

              ) == F

            ){

              sp_in_MapBiomas_coverLand_total %>%
                dplyr::filter (Ameaca == "Total") %>%
                dplyr::select(Porcentagem_EOO_2020)

            } else {

              "não calculado"

            },
            '%), mas também à nível regional, em 2020, na qual foi possível contabilizar x situações de ameaça das quais x situações_ameaça são afetadas pela conversão de áreas para atividades de '

          )

      }

    }


    #### Condicionais: Moderada proporção de conversão para uso alternativo do solo da AOO ####

    if(

      exists("ModeradaConversaoUsoDoSoloAOO") == T

    ){

      if(

        ModeradaConversaoUsoDoSoloAOO == T

      ){

        labelModeradaConversaoAOO =
          paste0(

            '<markG>a moderada proporção de áreas convertidas para uso alternativo do solo na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a></markG>'

          )

        valueModeradaConversaoAOO =
          paste0(

            'a moderada proporção de áreas convertidas para uso alternativo do solo na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%), em 2020, na qual foi possível contabilizar x situações de ameaça das quais x situações_ameaça são afetadas pela conversão de áreas para atividades de '

          )

      }

    }


    #### Condicionais: moderada proporção de conversão para uso alternativo do solo da AOO e EOO ####

    if(

      exists("ModeradaConversaoUsoDoSoloAOOEOO") == T

    ){

      if(

        ModeradaConversaoUsoDoSoloAOOEOO == T

      ){

        labelModeradaConversaoAOOEOO =
          paste0(

            '<markG>a moderada proporção de áreas convertidas para uso alternativo do solo na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%) e na EOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020),
            '%), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a></markG>'

          )

        valueModeradaConversaoAOOEOO =
          paste0(

            'a moderada proporção de áreas convertidas para uso alternativo do solo na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%) e na EOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020),
            '%), em 2020, na qual foi possível contabilizar x situações de ameaça das quais x situações_ameaça são afetadas pela conversão de áreas para atividades de '

          )

      }

    }


    #### Condicionais: Alta proporção de conversão para uso alternativo do solo da AOO e EOO ####

    if(

      exists("AltaConversaoUsoDoSoloAOOEOO") == T

    ){

      if(

        AltaConversaoUsoDoSoloAOOEOO == T

      ){

        labelAltaConversaoAOOEOO =
          paste0(

            '<markG>a alta proporção de áreas convertidas para uso alternativo do solo na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%) e na EOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020),
            '%), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a></markG>'

          )

        valueAltaConversaoAOOEOO =
          paste0(

            'a alta proporção de áreas convertidas para uso alternativo do solo na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%) e na EOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_EOO_2020),
            '%), em 2020, na qual foi possível contabilizar x situações de ameaça das quais x situações_ameaça são afetadas pela conversão de áreas para atividades de '

          )

      }

    }


    #### Condicionais: Alta proporção de conversão para uso alternativo do solo da AOO ####

    if(

      exists("AltaConversaoUsoDoSoloAOO") == T

    ){

      if(

        AltaConversaoUsoDoSoloAOO == T

      ){

        labelAltaConversaoAOO =
          paste0(

            '<markG>a alta proporção de áreas convertidas para uso alternativo do solo na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%), em 2020, na qual foi possível contabilizar <a id="LOCATIONSinTEXT"></a> <a id="LOCATIONSn"></a> de ameaça <a id="LOCATIONSm_threatened_n"> <a id="LOCATIONSm_threatened"> <a id="LOCATIONSm"></a> pela conversão de áreas para atividades de <a id="AMEACAS"></a></markG>'

          )

        valueAltaConversaoAOO =
          paste0(

            'a alta proporção de áreas convertidas para uso alternativo do solo na AOO (',
            sp_in_MapBiomas_coverLand_total %>%
              dplyr::filter (Ameaca == "Total") %>%
              dplyr::select(Porcentagem_AOO_2020),
            '%), em 2020, na qual foi possível contabilizar x situações de ameaça das quais x situações_ameaça são afetadas pela conversão de áreas para atividades de '

          )

      }

    }

    ConversaoUsoDoSolo <-
      data.frame(

        BaixaConversaoUsoDoSoloAOO = BaixaConversaoUsoDoSoloAOO,
        BaixaConversaoUsoDoSoloAOOEOO = BaixaConversaoUsoDoSoloAOOEOO,
        BaixaConversaoUsoDoSoloEOO = BaixaConversaoUsoDoSoloEOO,
        BaixaConversaoUsoDoSoloAOOEOOAmaz2 = BaixaConversaoUsoDoSoloAOOEOOAmaz,
        AltaConversaoUsoDoSoloAOOEOO = AltaConversaoUsoDoSoloAOOEOO,
        AltaConversaoUsoDoSoloAOO = AltaConversaoUsoDoSoloAOO,
        AltaConversaoUsoDoSoloEOO = AltaConversaoUsoDoSoloEOO,
        ModeradaConversaoUsoDoSoloAOOEOO = ModeradaConversaoUsoDoSoloAOOEOO,
        ModeradaConversaoUsoDoSoloAOO = ModeradaConversaoUsoDoSoloAOO,
        ModeradaConversaoUsoDoSoloEOO = ModeradaConversaoUsoDoSoloEOO

      )

    ConversaoUsoDoSolo <-
      ConversaoUsoDoSolo[which(ConversaoUsoDoSolo == T)]


    # Renomeando data.frame da Sobreposição

    colnames(sp_in_MapBiomas_coverLand) <-
      c(

        "Ameaça",
        "AOO útil (km²)",
        "Área Ocupada da AOO (km²)",
        "Área Ocupada da AOO (%)",
        "EOO (km²)",
        "Área Ocupada da EOO (km²)",
        "Área Ocupada da EOO (%)"

      )


    # Mapa ####

    Estados_IBGE <-
      st_read(

        "D:/Shapefiles/BR_UF_2020/BR_UF_2020.shp",
        quiet = TRUE

        )

    Municipios_IBGE <-
      st_read(

        "D:/Shapefiles/BR_Municipios_2020/BR_Municipios_2020.shp",
        quiet = TRUE

        )
    Municipios_IBGE$Mun_ES <-
      paste0(

        Municipios_IBGE$NM_MUN,
        " (",
        Municipios_IBGE$SIGLA_UF,
        ")"

      )

    Municipios_IBGE <-
      Municipios_IBGE %>%
      dplyr::filter(Mun_ES %in% distribution_in_BrazilianStatesAndMunicipalities)


    ## Paleta de porcentagem último ano MapBiomas (2020) ####

    paletaPorc <- colorNumeric(

      palette = "RdYlGn",
      domain = scale_range <- c(0, 100),
      reverse = T

    )


    ## Paleta de tendência ####

    paletaTrend <- colorNumeric(

      palette = "RdYlGn",
      domain = scale_range <- c(-10, 10),
      reverse = T

    )

    paletaTrend <- colorNumeric(

      palette = "RdYlGn",
      domain = scale_range <- c(-10, 10),
      reverse = F

    )

    if(

      file.exists(

        paste0("C:/R/R-4.1.1/working/EOO ", SPECIES, ".shp")

      ) == T

    ){

      eoo <-
        st_read(

          paste0("C:/R/R-4.1.1/working/EOO ", SPECIES, ".shp"),
          quiet = TRUE

          )

    }

    if(

      file.exists(

        paste0("C:/R/R-4.1.1/working/EOO-buffer ", SPECIES, ".shp")

      ) == T

    ){

      EOObuffer <-
        st_read(

          paste0("C:/R/R-4.1.1/working/EOO-buffer ", SPECIES, ".shp"),
          quiet = TRUE

          )

    }

    if(

      file.exists(paste0("C:/R/R-4.1.1/working/AOOinCAR ", SPECIES, ".shp")

      ) == T

    ){

      AOOinCAR <-
        st_read(

          paste0("C:/R/R-4.1.1/working/AOOinCAR ", SPECIES, ".shp"),
          quiet = TRUE

          )

    }

    points <-
      st_read(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "CNCFlora_data/inputs/shapefiles/species_points/",
          "POINTS ", SPECIES, ".shp"

        ),
        quiet = TRUE

      ) %>%
      st_transform(4326)

    points$ano_clt <- sub("^[0-9][0-9]$", NA, points$ano_clt)
    points$ano_clt <- sub("^[0-9]$", NA, points$ano_clt)

    points$URNs_ano_clt <-
      paste0(

        points$URNs,
        "<br>Ano de coleta: <b>",
        points$ano_clt,
        "</b>"

      )

    points$ano_clt_character <-
      as.character(

        paste0(

          "ano_",points$ano_clt

        )

      )

    points <-
      st_as_sf(

        points,
        coords = c("lon", "lat"),
        crs = 4326,
        agr = "constant"

      )

    points2 <-
      st_make_valid(

        st_transform(

          points,
          "+proj=eqc +datum=WGS84"

        )

      )

    UCs_intersected <-
      st_intersects(points2, shapefile_UC)

    UCs_intersected <-
      unlist(UCs_intersected)

    UCs_intersected <-
      UCs_intersected[!is.na(UCs_intersected)]

    points_for_locations <-
      as_Spatial(points)

    Buffer <- seq(2000, 50000, by = 1000)

    Locations_length <- NULL

    for(

      buffer in Buffer

    ){

      Locations <-
        circles(

          points_for_locations,
          d = buffer,
          lonlat = T

        )

      Locations_length_ <-
        data.frame(

          Buffer = buffer,
          N_Locations = length(

            Locations@polygons@polygons[[1]]@Polygons

          )

        )
      Locations_length <-
        rbind(Locations_length, Locations_length_)

    }

    Buffers_set<-NULL
    for(

      locations in unique(Locations_length$N_Locations)

    ){

      Buffers_set_ <-
        max(

          Locations_length %>%
            dplyr::filter(N_Locations == locations)

        )

      Buffers_set <- rbind(Buffers_set, Buffers_set_)

    }

    Locations_Buffers <-
      Locations_length %>%
      dplyr::filter(Buffer %in% Buffers_set[,1])

    for(

      buffer in 1:length(Locations_Buffers$Buffer)

    ){

      assign(

        paste0(

          "Buffer",
          buffer

        ),
        circles(

          points_for_locations,
          d = Locations_Buffers$Buffer[buffer],
          lonlat = T

        )

      )

    }

    if(

      file.exists(paste0("AOO ", SPECIES, ".shp")) == T

    ){

      aoo <- st_read(paste0("AOO ", SPECIES, ".shp"))
      aoo <-
        st_as_sf(

          aoo,
          coords = c("lon", "lat"),
          crs = 4326,
          agr = "constant"

        )

    }

    if(

      file.exists(

        paste0(

          "MapBiomas_AOO ",
          SPECIES,
          ".shp"

        )

      ) == T

    ){

      MapBiomas_AOO <-
        st_read(

          paste0(

            "MapBiomas_AOO ",
            SPECIES,
            ".shp"

          )

        ) %>%
        st_transform(4326)

      MapBiomas_AOO <-
        st_as_sf(

          MapBiomas_AOO,
          coords = c("lon", "lat"),
          crs = 4326,
          agr = "constant"

        )

      CoresLegenda <- MapBiomas_AOO$MB_B_6_
      CoresLegenda <- sub("^3$", "#006400", CoresLegenda)
      CoresLegenda <- sub("^4$", "#00ff00", CoresLegenda)
      CoresLegenda <- sub("^5$", "#687537", CoresLegenda)
      CoresLegenda <- sub("^9$", "#ad4413", CoresLegenda)
      CoresLegenda <- sub("^11$", "#45C2A5", CoresLegenda)
      CoresLegenda <- sub("^12$", "#B8AF4F", CoresLegenda)
      CoresLegenda <- sub("^13$", "#f1c232", CoresLegenda)
      CoresLegenda <- sub("^15$", "#FFD966", CoresLegenda)
      CoresLegenda <- sub("^19$", "#D5A6BD", CoresLegenda)
      CoresLegenda <- sub("^20$", "#C27BA0", CoresLegenda)
      CoresLegenda <- sub("^21$", "#fff3bf", CoresLegenda)
      CoresLegenda <- sub("^23$", "#DD7E6B", CoresLegenda)
      CoresLegenda <- sub("^24$", "#aa0000", CoresLegenda)
      CoresLegenda <- sub("^25$", "#ff3d3d", CoresLegenda)
      CoresLegenda <- sub("^27$", "#D5D5E5", CoresLegenda)
      CoresLegenda <- sub("^29$", "#665a3a", CoresLegenda)
      CoresLegenda <- sub("^30$", "#af2a2a", CoresLegenda)
      CoresLegenda <- sub("^31$", "#02106f", CoresLegenda)
      CoresLegenda <- sub("^32$", "#968c46", CoresLegenda)
      CoresLegenda <- sub("^33$", "#0000FF", CoresLegenda)
      CoresLegenda <- sub("^36$", "#f3b4f1", CoresLegenda)
      CoresLegenda <- sub("^39$", "#e075ad", CoresLegenda)
      CoresLegenda <- sub("^40$", "#982c9e", CoresLegenda)
      CoresLegenda <- sub("^41$", "#e787f8", CoresLegenda)
      CoresLegenda <- sub("^46$", "#cca0d4", CoresLegenda)
      CoresLegenda <- sub("^47$", "#d082de", CoresLegenda)
      CoresLegenda <- sub("^48$", "#cd49e4", CoresLegenda)
      CoresLegenda <- sub("^49$", "#6b9932", CoresLegenda)

      Legenda <- MapBiomas_AOO$MB_B_6_
      Legenda <- sub("^3$", "Floresta", Legenda)
      Legenda <- sub("^4$", "Savana", Legenda)
      Legenda <- sub("^5$", "Mangue", Legenda)
      Legenda <- sub("^9$", "Silvicultura", Legenda)
      Legenda <- sub("^11$", "Campo Alagado e Área Pantanosa", Legenda)
      Legenda <- sub("^12$", "Formação Campestre", Legenda)
      Legenda <- sub("^13$", "Outras Formações não Florestais", Legenda)
      Legenda <- sub("^15$", "Pastagem", Legenda)
      Legenda <- sub("^19$", "Lavoura Temporária", Legenda)
      Legenda <- sub("^20$", "Cana-de-açúcar", Legenda)
      Legenda <- sub("^21$", "Mosaico de Agricultura e Pastagem", Legenda)
      Legenda <- sub("^23$", "Praia, Duna e Areal", Legenda)
      Legenda <- sub("^24$", "Infraestrutura Urbana", Legenda)
      Legenda <- sub("^25$", "Outras Áreas não Vegetadas", Legenda)
      Legenda <- sub("^27$", "Não Observado", Legenda)
      Legenda <- sub("^29$", "Afloramento Rochoso", Legenda)
      Legenda <- sub("^30$", "Mineração", Legenda)
      Legenda <- sub("^31$", "Aquicultura", Legenda)
      Legenda <- sub("^32$", "Apicum", Legenda)
      Legenda <- sub("^33$", "Rio, Lago e Oceano", Legenda)
      Legenda <- sub("^36$", "Lavoura Perene", Legenda)
      Legenda <- sub("^39$", "Soja", Legenda)
      Legenda <- sub("^40$", "Arroz", Legenda)
      Legenda <- sub("^41$", "Outras Lavouras Temporárias", Legenda)
      Legenda <- sub("^46$", "Café", Legenda)
      Legenda <- sub("^47$", "Citrus", Legenda)
      Legenda <- sub("^48$", "Outras Lavouras Perenes", Legenda)
      Legenda <- sub("^49$", "Restinga Arborizada", Legenda)

    }

    if(

      file.exists(paste0("MapBiomas_EOO ", SPECIES, ".shp")) == T

    ){

      MapBiomas_EOO <-
        st_read(

          paste0(

            "MapBiomas_EOO ",
            SPECIES,
            ".shp"

          )

        ) %>%
        st_transform(4326)

      MapBiomas_EOO <-
        st_as_sf(

          MapBiomas_EOO,
          coords = c("lon", "lat"),
          crs = 4326,
          agr = "constant"

        )

      CoresLegenda_EOO <- MapBiomas_EOO$MB_B_6_
      CoresLegenda_EOO <- sub("^3$", "#006400", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^4$", "#00ff00", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^5$", "#687537", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^9$", "#ad4413", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^11$", "#45C2A5", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^12$", "#B8AF4F", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^13$", "#f1c232", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^15$", "#FFD966", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^19$", "#D5A6BD", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^20$", "#C27BA0", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^21$", "#fff3bf", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^23$", "#DD7E6B", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^24$", "#aa0000", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^25$", "#ff3d3d", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^27$", "#D5D5E5", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^29$", "#665a3a", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^30$", "#af2a2a", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^31$", "#02106f", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^32$", "#968c46", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^33$", "#0000FF", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^36$", "#f3b4f1", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^39$", "#e075ad", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^40$", "#982c9e", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^41$", "#e787f8", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^46$", "#cca0d4", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^47$", "#d082de", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^48$", "#cd49e4", CoresLegenda_EOO)
      CoresLegenda_EOO <- sub("^49$", "#6b9932", CoresLegenda_EOO)

      Legenda_EOO <- MapBiomas_EOO$MB_B_6_
      Legenda_EOO <- sub("^3$", "Floresta", Legenda_EOO)
      Legenda_EOO <- sub("^4$", "Savana", Legenda_EOO)
      Legenda_EOO <- sub("^5$", "Mangue", Legenda_EOO)
      Legenda_EOO <- sub("^9$", "Silvicultura", Legenda_EOO)
      Legenda_EOO <- sub("^11$", "Campo Alagado e Área Pantanosa", Legenda_EOO)
      Legenda_EOO <- sub("^12$", "Formação Campestre", Legenda_EOO)
      Legenda_EOO <- sub("^13$", "Outras Formações não Florestais", Legenda_EOO)
      Legenda_EOO <- sub("^15$", "Pastagem", Legenda_EOO)
      Legenda_EOO <- sub("^19$", "Lavoura Temporária", Legenda_EOO)
      Legenda_EOO <- sub("^20$", "Cana-de-açúcar", Legenda_EOO)
      Legenda_EOO <- sub("^21$", "Mosaico de Agricultura e Pastagem", Legenda_EOO)
      Legenda_EOO <- sub("^23$", "Praia, Duna e Areal", Legenda_EOO)
      Legenda_EOO <- sub("^24$", "Infraestrutura Urbana", Legenda_EOO)
      Legenda_EOO <- sub("^25$", "Outras Áreas não Vegetadas", Legenda_EOO)
      Legenda_EOO <- sub("^27$", "Não Observado", Legenda_EOO)
      Legenda_EOO <- sub("^29$", "Afloramento Rochoso", Legenda_EOO)
      Legenda_EOO <- sub("^30$", "Mineração", Legenda_EOO)
      Legenda_EOO <- sub("^31$", "Aquicultura", Legenda_EOO)
      Legenda_EOO <- sub("^32$", "Apicum", Legenda_EOO)
      Legenda_EOO <- sub("^33$", "Rio, Lago e Oceano", Legenda_EOO)
      Legenda_EOO <- sub("^36$", "Lavoura Perene", Legenda_EOO)
      Legenda_EOO <- sub("^39$", "Soja", Legenda_EOO)
      Legenda_EOO <- sub("^40$", "Arroz", Legenda_EOO)
      Legenda_EOO <- sub("^41$", "Outras Lavouras Temporárias", Legenda_EOO)
      Legenda_EOO <- sub("^46$", "Café", Legenda_EOO)
      Legenda_EOO <- sub("^47$", "Citrus", Legenda_EOO)
      Legenda_EOO <- sub("^48$", "Outras Lavouras Perenes", Legenda_EOO)
      Legenda_EOO <- sub("^49$", "Restinga Arborizada", Legenda_EOO)

    }


    ## Paleta do Ano de Coleta ####

    paletaAnoColeta <- colorNumeric(

      palette = "Blues",
      domain = scale_range <- c(1800, 2022)

    )

    PaletaAnoColeta <- data.frame (

      Ano = 1800:2022,
      Cor = paletaAnoColeta(1800:2022)

    )

    SetCoresAnoColeta <- awesomeIconList(

      ano_NA = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = 'gray'),
      ano_1800 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#F7FBFF'),
      ano_1801 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#F6FAFF'),
      ano_1802 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#F5FAFE'),
      ano_1803 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#F4F9FE'),
      ano_1804 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#F3F9FE'),
      ano_1805 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#F3F8FE'),
      ano_1806 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#F2F8FD'),
      ano_1807 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#F1F7FD'),
      ano_1808 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#F0F6FD'),
      ano_1809 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#EFF6FC'),
      ano_1810 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#EEF5FC'),
      ano_1811 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#EDF5FC'),
      ano_1812 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#ECF4FC'),
      ano_1813 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#EBF3FB'),
      ano_1814 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#EAF3FB'),
      ano_1815 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#EAF2FB'),
      ano_1816 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E9F2FA'),
      ano_1817 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E8F1FA'),
      ano_1818 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E7F1FA'),
      ano_1819 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E6F0FA'),
      ano_1820 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E5EFF9'),
      ano_1821 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E4EFF9'),
      ano_1822 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E3EEF9'),
      ano_1823 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E2EEF8'),
      ano_1824 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E1EDF8'),
      ano_1825 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E0EDF8'),
      ano_1826 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#E0ECF8'),
      ano_1827 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#DFEBF7'),
      ano_1828 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#DEEBF7'),
      ano_1829 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#DDEAF7'),
      ano_1830 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#DCEAF6'),
      ano_1831 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#DBE9F6'),
      ano_1832 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#DAE9F6'),
      ano_1833 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D9E8F5'),
      ano_1834 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D9E7F5'),
      ano_1835 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D8E7F5'),
      ano_1836 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D7E6F5'),
      ano_1837 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D6E6F4'),
      ano_1838 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D5E5F4'),
      ano_1839 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D4E4F4'),
      ano_1840 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D3E4F3'),
      ano_1841 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D3E3F3'),
      ano_1842 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D2E3F3'),
      ano_1843 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D1E2F3'),
      ano_1844 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#D0E2F2'),
      ano_1845 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#CFE1F2'),
      ano_1846 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#CEE0F2'),
      ano_1847 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#CDE0F1'),
      ano_1848 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#CDDFF1'),
      ano_1849 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#CCDFF1'),
      ano_1850 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#CBDEF1'),
      ano_1851 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#CADEF0'),
      ano_1852 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#C9DDF0'),
      ano_1853 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#C8DCF0'),
      ano_1854 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#C7DCEF'),
      ano_1855 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#C6DBEF'),
      ano_1856 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#C5DBEF'),
      ano_1857 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#C4DAEE'),
      ano_1858 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#C2D9EE'),
      ano_1859 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#C1D9ED'),
      ano_1860 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#C0D8ED'),
      ano_1861 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#BED8EC'),
      ano_1862 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#BDD7EC'),
      ano_1863 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#BBD6EB'),
      ano_1864 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#BAD6EB'),
      ano_1865 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#B9D5EA'),
      ano_1866 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#B7D5EA'),
      ano_1867 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#B6D4E9'),
      ano_1868 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#B4D3E9'),
      ano_1869 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#B3D3E8'),
      ano_1870 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#B1D2E8'),
      ano_1871 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#B0D1E7'),
      ano_1872 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#AED1E7'),
      ano_1873 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#ADD0E6'),
      ano_1874 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#ACD0E6'),
      ano_1875 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#AACFE5'),
      ano_1876 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#A9CEE5'),
      ano_1877 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#A7CEE4'),
      ano_1878 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#A6CDE4'),
      ano_1879 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#A4CDE3'),
      ano_1880 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#A3CCE3'),
      ano_1881 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#A1CBE2'),
      ano_1882 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#A0CBE2'),
      ano_1883 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#9ECAE1'),
      ano_1884 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#9DC9E1'),
      ano_1885 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#9BC8E0'),
      ano_1886 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#99C7E0'),
      ano_1887 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#98C6E0'),
      ano_1888 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#96C5DF'),
      ano_1889 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#94C4DF'),
      ano_1890 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#92C3DE'),
      ano_1891 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#91C2DE'),
      ano_1892 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#8FC1DE'),
      ano_1893 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#8DC0DD'),
      ano_1894 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#8BBFDD'),
      ano_1895 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#89BEDC'),
      ano_1896 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#88BDDC'),
      ano_1897 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#86BCDC'),
      ano_1898 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#84BBDB'),
      ano_1899 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#82BADB'),
      ano_1900 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#80B9DA'),
      ano_1901 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#7EB8DA'),
      ano_1902 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#7DB7DA'),
      ano_1903 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#7BB6D9'),
      ano_1904 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#79B5D9'),
      ano_1905 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#77B4D8'),
      ano_1906 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#75B3D8'),
      ano_1907 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#73B2D8'),
      ano_1908 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#71B1D7'),
      ano_1909 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#6FB0D7'),
      ano_1910 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#6DAFD6'),
      ano_1911 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#6BAED6'),
      ano_1912 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#6AADD5'),
      ano_1913 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#68ACD5'),
      ano_1914 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#67ABD4'),
      ano_1915 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#66AAD4'),
      ano_1916 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#64A9D3'),
      ano_1917 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#63A8D3'),
      ano_1918 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#61A7D2'),
      ano_1919 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#60A6D1'),
      ano_1920 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#5FA5D1'),
      ano_1921 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#5DA4D0'),
      ano_1922 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#5CA3D0'),
      ano_1923 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#5AA2CF'),
      ano_1924 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#59A1CF'),
      ano_1925 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#58A0CE'),
      ano_1926 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#569FCD'),
      ano_1927 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#559ECD'),
      ano_1928 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#539DCC'),
      ano_1929 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#529CCC'),
      ano_1930 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#509BCB'),
      ano_1931 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4F9ACA'),
      ano_1932 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4D99CA'),
      ano_1933 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4B98C9'),
      ano_1934 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4A97C9'),
      ano_1935 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4896C8'),
      ano_1936 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4795C8'),
      ano_1937 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4594C7'),
      ano_1938 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4393C6'),
      ano_1939 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4292C6'),
      ano_1940 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#4190C5'),
      ano_1941 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#408FC5'),
      ano_1942 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3F8EC4'),
      ano_1943 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3E8DC3'),
      ano_1944 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3D8CC3'),
      ano_1945 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3C8AC2'),
      ano_1946 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3B89C2'),
      ano_1947 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3A88C1'),
      ano_1948 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3987C0'),
      ano_1949 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3786C0'),
      ano_1950 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3684BF'),
      ano_1951 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3583BF'),
      ano_1952 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3482BE'),
      ano_1953 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3381BD'),
      ano_1954 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#3280BD'),
      ano_1955 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#317EBC'),
      ano_1956 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2F7DBB'),
      ano_1957 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2E7CBB'),
      ano_1958 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2D7BBA'),
      ano_1959 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2C7ABA'),
      ano_1960 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2A79B9'),
      ano_1961 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2977B8'),
      ano_1962 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2876B8'),
      ano_1963 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2675B7'),
      ano_1964 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2574B7'),
      ano_1965 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2373B6'),
      ano_1966 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2272B5'),
      ano_1967 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#2170B5'),
      ano_1968 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#206FB4'),
      ano_1969 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1F6EB3'),
      ano_1970 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1F6DB2'),
      ano_1971 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1E6CB1'),
      ano_1972 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1D6BB0'),
      ano_1973 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1D69AF'),
      ano_1974 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1C68AE'),
      ano_1975 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1B67AD'),
      ano_1976 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1A66AC'),
      ano_1977 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1A65AB'),
      ano_1978 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1964AB'),
      ano_1979 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1862AA'),
      ano_1980 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1761A9'),
      ano_1981 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1660A8'),
      ano_1982 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#165FA7'),
      ano_1983 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#155EA6'),
      ano_1984 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#145DA5'),
      ano_1985 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#135BA4'),
      ano_1986 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#125AA3'),
      ano_1987 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1159A2'),
      ano_1988 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#1058A2'),
      ano_1989 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#0F57A1'),
      ano_1990 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#0E56A0'),
      ano_1991 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#0C559F'),
      ano_1992 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#0B549E'),
      ano_1993 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#0A529D'),
      ano_1994 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#08519C'),
      ano_1995 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#08509B'),
      ano_1996 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#084F99'),
      ano_1997 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#084E97'),
      ano_1998 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#084C95'),
      ano_1999 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#084B93'),
      ano_2000 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#094A92'),
      ano_2001 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#094990'),
      ano_2002 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#09488E'),
      ano_2003 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#09468C'),
      ano_2004 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#09458A'),
      ano_2005 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#094489'),
      ano_2006 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#094387'),
      ano_2007 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#094285'),
      ano_2008 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#094083'),
      ano_2009 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#093F82'),
      ano_2010 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#093E80'),
      ano_2011 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#093D7E'),
      ano_2012 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#093C7C'),
      ano_2013 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#093A7B'),
      ano_2014 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#093979'),
      ano_2015 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#093877'),
      ano_2016 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#083775'),
      ano_2017 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#083674'),
      ano_2018 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#083572'),
      ano_2019 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#083370'),
      ano_2020 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#08326E'),
      ano_2021 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#08316D'),
      ano_2022 = makeAwesomeIcon(icon = 'circle', markerColor = 'darkblue', library = 'fa', iconColor = '#08306B')

    )

    if(

      exists("aoo") == T

    ){

      if(

        length(UCs_intersected) == 0

      ){

        if(

          exists("MapBiomas_AOO") == T

        ){

          map <- leaflet(

            options = leafletOptions(preferCanvas = TRUE)

          ) %>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addPolygons(

              data = Estados_IBGE,
              fillOpacity = 0,
              weight = 2,
              color = "black"

            ) %>%
            addPolygons(

              data = Municipios_IBGE,
              fillOpacity = 0,
              weight = 2,
              color = "purple",
              label = ~Mun_ES

            ) %>%
            cond_addPolygons(

              data = eoo,
              color = "gray",
              fillOpacity = 0.3,
              weight = 1,
              group = "EOO",
              execute = if(exists('eoo') == T){T} else {F}

            ) %>%
            cond_addPolygons(

              data = EOObuffer,
              color = "pink",
              fillOpacity = 0.3,
              weight = 1,
              group = "EOO buffer",
              execute = if(exists('EOObuffer') == T){T} else {F}

            ) %>%
            cond_addPolygons(

              data = AOOinCAR,
              color = "yellow",
              fillOpacity = 0.3,
              weight = 1,
              group = "CAR",
              execute = if(exists('AOOinCAR') == T){T} else {F}

            ) %>%
            addAwesomeMarkers(

              data = points,
              group = "Pontos",
              popup = ~URNs_ano_clt,
              icon = ~SetCoresAnoColeta[ano_clt_character]

            ) %>%
            addPolygons(

              data = aoo,
              color = "blue",
              fillOpacity = 0.01,
              weight = 1

            ) %>%
            addMeasure(

              position = "bottomright",
              primaryLengthUnit = "kilometers",
              primaryAreaUnit = "sqmeters",
              localization = "pt_BR",
              activeColor = "#3D535D",
              completedColor = "#7D4479"

            ) %>%
            addLayersControl(

              overlayGroups = c(

                "Pontos",
                "CAR",
                "EOO",
                "EOO buffer",
                "MapBiomas 2020 - AOO",
                "Porcentagem convertida 2020",
                "Tendência 1985-2020"

              ),
              baseGroups = c(

                if(exists('Buffer1') == T){paste0(Locations_Buffers$Buffer[1]/1000, ' km | ', Locations_Buffers$N_Locations[1], ' locations')},
                if(exists('Buffer2') == T){paste0(Locations_Buffers$Buffer[2]/1000, ' km | ', Locations_Buffers$N_Locations[2], ' locations')},
                if(exists('Buffer3') == T){paste0(Locations_Buffers$Buffer[3]/1000, ' km | ', Locations_Buffers$N_Locations[3], ' locations')},
                if(exists('Buffer4') == T){paste0(Locations_Buffers$Buffer[4]/1000, ' km | ', Locations_Buffers$N_Locations[4], ' locations')},
                if(exists('Buffer5') == T){paste0(Locations_Buffers$Buffer[5]/1000, ' km | ', Locations_Buffers$N_Locations[5], ' locations')},
                if(exists('Buffer6') == T){paste0(Locations_Buffers$Buffer[6]/1000, ' km | ', Locations_Buffers$N_Locations[6], ' locations')},
                if(exists('Buffer7') == T){paste0(Locations_Buffers$Buffer[7]/1000, ' km | ', Locations_Buffers$N_Locations[7], ' locations')},
                if(exists('Buffer8') == T){paste0(Locations_Buffers$Buffer[8]/1000, ' km | ', Locations_Buffers$N_Locations[8], ' locations')},
                if(exists('Buffer9') == T){paste0(Locations_Buffers$Buffer[9]/1000, ' km | ', Locations_Buffers$N_Locations[9], ' locations')},
                if(exists('Buffer10') == T){paste0(Locations_Buffers$Buffer[10]/1000, ' km | ', Locations_Buffers$N_Locations[10], ' locations')},
                if(exists('Buffer11') == T){paste0(Locations_Buffers$Buffer[11]/1000, ' km | ', Locations_Buffers$N_Locations[11], ' locations')},
                if(exists('Buffer12') == T){paste0(Locations_Buffers$Buffer[12]/1000, ' km | ', Locations_Buffers$N_Locations[12], ' locations')},
                if(exists('Buffer13') == T){paste0(Locations_Buffers$Buffer[13]/1000, ' km | ', Locations_Buffers$N_Locations[13], ' locations')},
                if(exists('Buffer14') == T){paste0(Locations_Buffers$Buffer[14]/1000, ' km | ', Locations_Buffers$N_Locations[14], ' locations')},
                if(exists('Buffer15') == T){paste0(Locations_Buffers$Buffer[15]/1000, ' km | ', Locations_Buffers$N_Locations[15], ' locations')},
                if(exists('Buffer16') == T){paste0(Locations_Buffers$Buffer[16]/1000, ' km | ', Locations_Buffers$N_Locations[16], ' locations')},
                if(exists('Buffer17') == T){paste0(Locations_Buffers$Buffer[17]/1000, ' km | ', Locations_Buffers$N_Locations[17], ' locations')},
                if(exists('Buffer18') == T){paste0(Locations_Buffers$Buffer[18]/1000, ' km | ', Locations_Buffers$N_Locations[18], ' locations')},
                if(exists('Buffer19') == T){paste0(Locations_Buffers$Buffer[19]/1000, ' km | ', Locations_Buffers$N_Locations[19], ' locations')},
                if(exists('Buffer20') == T){paste0(Locations_Buffers$Buffer[20]/1000, ' km | ', Locations_Buffers$N_Locations[20], ' locations')},
                if(exists('Buffer21') == T){paste0(Locations_Buffers$Buffer[21]/1000, ' km | ', Locations_Buffers$N_Locations[21], ' locations')},
                if(exists('Buffer22') == T){paste0(Locations_Buffers$Buffer[22]/1000, ' km | ', Locations_Buffers$N_Locations[22], ' locations')},
                if(exists('Buffer23') == T){paste0(Locations_Buffers$Buffer[23]/1000, ' km | ', Locations_Buffers$N_Locations[23], ' locations')},
                if(exists('Buffer24') == T){paste0(Locations_Buffers$Buffer[24]/1000, ' km | ', Locations_Buffers$N_Locations[24], ' locations')},
                if(exists('Buffer25') == T){paste0(Locations_Buffers$Buffer[25]/1000, ' km | ', Locations_Buffers$N_Locations[25], ' locations')},
                if(exists('Buffer26') == T){paste0(Locations_Buffers$Buffer[26]/1000, ' km | ', Locations_Buffers$N_Locations[26], ' locations')},
                if(exists('Buffer27') == T){paste0(Locations_Buffers$Buffer[27]/1000, ' km | ', Locations_Buffers$N_Locations[27], ' locations')},
                if(exists('Buffer28') == T){paste0(Locations_Buffers$Buffer[28]/1000, ' km | ', Locations_Buffers$N_Locations[28], ' locations')},
                if(exists('Buffer29') == T){paste0(Locations_Buffers$Buffer[29]/1000, ' km | ', Locations_Buffers$N_Locations[29], ' locations')},
                if(exists('Buffer30') == T){paste0(Locations_Buffers$Buffer[30]/1000, ' km | ', Locations_Buffers$N_Locations[30], ' locations')},
                if(exists('Buffer31') == T){paste0(Locations_Buffers$Buffer[31]/1000, ' km | ', Locations_Buffers$N_Locations[31], ' locations')},
                if(exists('Buffer32') == T){paste0(Locations_Buffers$Buffer[32]/1000, ' km | ', Locations_Buffers$N_Locations[32], ' locations')},
                if(exists('Buffer33') == T){paste0(Locations_Buffers$Buffer[33]/1000, ' km | ', Locations_Buffers$N_Locations[33], ' locations')},
                if(exists('Buffer34') == T){paste0(Locations_Buffers$Buffer[34]/1000, ' km | ', Locations_Buffers$N_Locations[34], ' locations')},
                if(exists('Buffer35') == T){paste0(Locations_Buffers$Buffer[35]/1000, ' km | ', Locations_Buffers$N_Locations[35], ' locations')},
                if(exists('Buffer36') == T){paste0(Locations_Buffers$Buffer[36]/1000, ' km | ', Locations_Buffers$N_Locations[36], ' locations')},
                if(exists('Buffer37') == T){paste0(Locations_Buffers$Buffer[37]/1000, ' km | ', Locations_Buffers$N_Locations[37], ' locations')},
                if(exists('Buffer38') == T){paste0(Locations_Buffers$Buffer[38]/1000, ' km | ', Locations_Buffers$N_Locations[38], ' locations')},
                if(exists('Buffer39') == T){paste0(Locations_Buffers$Buffer[39]/1000, ' km | ', Locations_Buffers$N_Locations[39], ' locations')},
                if(exists('Buffer40') == T){paste0(Locations_Buffers$Buffer[40]/1000, ' km | ', Locations_Buffers$N_Locations[40], ' locations')},
                if(exists('Buffer41') == T){paste0(Locations_Buffers$Buffer[41]/1000, ' km | ', Locations_Buffers$N_Locations[41], ' locations')},
                if(exists('Buffer42') == T){paste0(Locations_Buffers$Buffer[42]/1000, ' km | ', Locations_Buffers$N_Locations[42], ' locations')},
                if(exists('Buffer43') == T){paste0(Locations_Buffers$Buffer[43]/1000, ' km | ', Locations_Buffers$N_Locations[43], ' locations')},
                if(exists('Buffer44') == T){paste0(Locations_Buffers$Buffer[44]/1000, ' km | ', Locations_Buffers$N_Locations[44], ' locations')},
                if(exists('Buffer45') == T){paste0(Locations_Buffers$Buffer[45]/1000, ' km | ', Locations_Buffers$N_Locations[45], ' locations')},
                if(exists('Buffer46') == T){paste0(Locations_Buffers$Buffer[46]/1000, ' km | ', Locations_Buffers$N_Locations[46], ' locations')},
                if(exists('Buffer47') == T){paste0(Locations_Buffers$Buffer[47]/1000, ' km | ', Locations_Buffers$N_Locations[47], ' locations')},
                if(exists('Buffer48') == T){paste0(Locations_Buffers$Buffer[48]/1000, ' km | ', Locations_Buffers$N_Locations[48], ' locations')},
                if(exists('Buffer49') == T){paste0(Locations_Buffers$Buffer[49]/1000, ' km | ', Locations_Buffers$N_Locations[49], ' locations')},
                if(exists('Buffer50') == T){paste0(Locations_Buffers$Buffer[50]/1000, ' km | ', Locations_Buffers$N_Locations[50], ' locations')}

              ),

              options = layersControlOptions(collapsed = FALSE)

            ) %>%
            cond_addPolygons(data = Buffer1@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[1]/1000, ' km | ', Locations_Buffers$N_Locations[1], ' locations'), execute = if(exists('Buffer1') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer2@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[2]/1000, ' km | ', Locations_Buffers$N_Locations[2], ' locations'), execute = if(exists('Buffer2') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer3@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[3]/1000, ' km | ', Locations_Buffers$N_Locations[3], ' locations'), execute = if(exists('Buffer3') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer4@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[4]/1000, ' km | ', Locations_Buffers$N_Locations[4], ' locations'), execute = if(exists('Buffer4') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer5@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[5]/1000, ' km | ', Locations_Buffers$N_Locations[5], ' locations'), execute = if(exists('Buffer5') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer6@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[6]/1000, ' km | ', Locations_Buffers$N_Locations[6], ' locations'), execute = if(exists('Buffer6') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer7@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[7]/1000, ' km | ', Locations_Buffers$N_Locations[7], ' locations'), execute = if(exists('Buffer7') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer8@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[8]/1000, ' km | ', Locations_Buffers$N_Locations[8], ' locations'), execute = if(exists('Buffer8') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer9@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[9]/1000, ' km | ', Locations_Buffers$N_Locations[9], ' locations'), execute = if(exists('Buffer9') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer10@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[10]/1000, ' km | ', Locations_Buffers$N_Locations[10], ' locations'), execute = if(exists('Buffer10') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer11@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[11]/1000, ' km | ', Locations_Buffers$N_Locations[11], ' locations'), execute = if(exists('Buffer11') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer12@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[12]/1000, ' km | ', Locations_Buffers$N_Locations[12], ' locations'), execute = if(exists('Buffer12') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer13@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[13]/1000, ' km | ', Locations_Buffers$N_Locations[13], ' locations'), execute = if(exists('Buffer13') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer14@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[14]/1000, ' km | ', Locations_Buffers$N_Locations[14], ' locations'), execute = if(exists('Buffer14') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer15@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[15]/1000, ' km | ', Locations_Buffers$N_Locations[15], ' locations'), execute = if(exists('Buffer15') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer16@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[16]/1000, ' km | ', Locations_Buffers$N_Locations[16], ' locations'), execute = if(exists('Buffer16') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer17@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[17]/1000, ' km | ', Locations_Buffers$N_Locations[17], ' locations'), execute = if(exists('Buffer17') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer18@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[18]/1000, ' km | ', Locations_Buffers$N_Locations[18], ' locations'), execute = if(exists('Buffer18') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer19@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[19]/1000, ' km | ', Locations_Buffers$N_Locations[19], ' locations'), execute = if(exists('Buffer19') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer20@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[20]/1000, ' km | ', Locations_Buffers$N_Locations[20], ' locations'), execute = if(exists('Buffer20') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer21@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[21]/1000, ' km | ', Locations_Buffers$N_Locations[21], ' locations'), execute = if(exists('Buffer21') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer22@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[22]/1000, ' km | ', Locations_Buffers$N_Locations[22], ' locations'), execute = if(exists('Buffer22') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer23@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[23]/1000, ' km | ', Locations_Buffers$N_Locations[23], ' locations'), execute = if(exists('Buffer23') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer24@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[24]/1000, ' km | ', Locations_Buffers$N_Locations[24], ' locations'), execute = if(exists('Buffer24') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer25@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[25]/1000, ' km | ', Locations_Buffers$N_Locations[25], ' locations'), execute = if(exists('Buffer25') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer26@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[26]/1000, ' km | ', Locations_Buffers$N_Locations[26], ' locations'), execute = if(exists('Buffer26') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer27@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[27]/1000, ' km | ', Locations_Buffers$N_Locations[27], ' locations'), execute = if(exists('Buffer27') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer28@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[28]/1000, ' km | ', Locations_Buffers$N_Locations[28], ' locations'), execute = if(exists('Buffer28') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer29@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[29]/1000, ' km | ', Locations_Buffers$N_Locations[29], ' locations'), execute = if(exists('Buffer29') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer30@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[30]/1000, ' km | ', Locations_Buffers$N_Locations[30], ' locations'), execute = if(exists('Buffer30') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer31@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[31]/1000, ' km | ', Locations_Buffers$N_Locations[31], ' locations'), execute = if(exists('Buffer31') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer32@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[32]/1000, ' km | ', Locations_Buffers$N_Locations[32], ' locations'), execute = if(exists('Buffer32') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer33@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[33]/1000, ' km | ', Locations_Buffers$N_Locations[33], ' locations'), execute = if(exists('Buffer33') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer34@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[34]/1000, ' km | ', Locations_Buffers$N_Locations[34], ' locations'), execute = if(exists('Buffer34') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer35@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[35]/1000, ' km | ', Locations_Buffers$N_Locations[35], ' locations'), execute = if(exists('Buffer35') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer36@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[36]/1000, ' km | ', Locations_Buffers$N_Locations[36], ' locations'), execute = if(exists('Buffer36') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer37@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[37]/1000, ' km | ', Locations_Buffers$N_Locations[37], ' locations'), execute = if(exists('Buffer37') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer38@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[38]/1000, ' km | ', Locations_Buffers$N_Locations[38], ' locations'), execute = if(exists('Buffer38') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer39@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[39]/1000, ' km | ', Locations_Buffers$N_Locations[39], ' locations'), execute = if(exists('Buffer39') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer40@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[40]/1000, ' km | ', Locations_Buffers$N_Locations[40], ' locations'), execute = if(exists('Buffer40') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer41@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[41]/1000, ' km | ', Locations_Buffers$N_Locations[41], ' locations'), execute = if(exists('Buffer41') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer42@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[42]/1000, ' km | ', Locations_Buffers$N_Locations[42], ' locations'), execute = if(exists('Buffer42') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer43@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[43]/1000, ' km | ', Locations_Buffers$N_Locations[43], ' locations'), execute = if(exists('Buffer43') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer44@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[44]/1000, ' km | ', Locations_Buffers$N_Locations[44], ' locations'), execute = if(exists('Buffer44') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer45@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[45]/1000, ' km | ', Locations_Buffers$N_Locations[45], ' locations'), execute = if(exists('Buffer45') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer46@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[46]/1000, ' km | ', Locations_Buffers$N_Locations[46], ' locations'), execute = if(exists('Buffer46') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer47@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[47]/1000, ' km | ', Locations_Buffers$N_Locations[47], ' locations'), execute = if(exists('Buffer47') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer48@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[48]/1000, ' km | ', Locations_Buffers$N_Locations[48], ' locations'), execute = if(exists('Buffer48') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer49@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[49]/1000, ' km | ', Locations_Buffers$N_Locations[49], ' locations'), execute = if(exists('Buffer49') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer50@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[50]/1000, ' km | ', Locations_Buffers$N_Locations[50], ' locations'), execute = if(exists('Buffer50') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer51@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[51]/1000, ' km | ', Locations_Buffers$N_Locations[51], ' locations'), execute = if(exists('Buffer51') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer52@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[52]/1000, ' km | ', Locations_Buffers$N_Locations[52], ' locations'), execute = if(exists('Buffer52') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer53@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[53]/1000, ' km | ', Locations_Buffers$N_Locations[53], ' locations'), execute = if(exists('Buffer53') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer54@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[54]/1000, ' km | ', Locations_Buffers$N_Locations[54], ' locations'), execute = if(exists('Buffer54') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer55@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[55]/1000, ' km | ', Locations_Buffers$N_Locations[55], ' locations'), execute = if(exists('Buffer55') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer56@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[56]/1000, ' km | ', Locations_Buffers$N_Locations[56], ' locations'), execute = if(exists('Buffer56') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer57@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[57]/1000, ' km | ', Locations_Buffers$N_Locations[57], ' locations'), execute = if(exists('Buffer57') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer58@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[58]/1000, ' km | ', Locations_Buffers$N_Locations[58], ' locations'), execute = if(exists('Buffer58') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer59@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[59]/1000, ' km | ', Locations_Buffers$N_Locations[59], ' locations'), execute = if(exists('Buffer59') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer60@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[60]/1000, ' km | ', Locations_Buffers$N_Locations[60], ' locations'), execute = if(exists('Buffer60') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer61@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[61]/1000, ' km | ', Locations_Buffers$N_Locations[61], ' locations'), execute = if(exists('Buffer61') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer62@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[62]/1000, ' km | ', Locations_Buffers$N_Locations[62], ' locations'), execute = if(exists('Buffer62') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer63@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[63]/1000, ' km | ', Locations_Buffers$N_Locations[63], ' locations'), execute = if(exists('Buffer63') == T){T} else {F}) %>%
            addPolygons(

              data = MapBiomas_AOO,
              fillOpacity = 0.9,
              weight = 1,
              color = "white",
              fillColor = CoresLegenda,
              label = Legenda,
              group = "MapBiomas 2020 - AOO"

            ) %>%
            cond_addPolygons(

              data = AOO_QuadOfGrid_shapefile,
              fillOpacity = 1,
              weight = 0,
              label = ~Quad_Taxa_anual_2020,
              color = ~paletaTrend(Taxa_anual),
              group = "Tendência 1985-2020",
              execute = if(exists('AOO_QuadOfGrid_shapefile') == T){T} else {F}


            ) %>%
            addLegend(

              "bottomleft",
              pal = paletaTrend,
              values = AOO_QuadOfGrid_shapefile$Taxa_anual,
              labFormat = labelFormat(suffix = "%"),
              title = "Uso Alternativo x Natural (% aa)",
              group = "Tendência 1985-2020",
              opacity = 1

            ) %>%
            cond_addPolygons(

              data = AOO_QuadOfGrid_shapefile,
              fillOpacity = 1,
              weight = 0,
              label = ~Quad_Porcentagem_2020,
              color = ~paletaPorc(Porcentagem_2020),
              group = "Porcentagem convertida 2020",
              execute = if(exists('AOO_QuadOfGrid_shapefile') == T){T} else {F}

            ) %>%
            addLegend(

              "bottomleft",
              pal = paletaPorc,
              values = AOO_QuadOfGrid_shapefile$Porcentagem_2020,
              labFormat = labelFormat(suffix = "%"),
              title = "Uso Alternativo (%)",
              group = "Porcentagem convertida 2020",
              opacity = 1

            )

        } else {

          map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addPolygons(

              data = Estados_IBGE,
              fillOpacity = 0,
              weight = 2,
              color = "black"

            ) %>%
            addPolygons(

              data = Municipios_IBGE,
              fillOpacity = 0,
              weight = 2,
              color = "purple",
              label = ~Mun_ES

            ) %>%
            cond_addPolygons(

              data = eoo,
              color = "gray",
              fillOpacity = 0.3,
              weight = 1,
              group = "EOO",
              execute = if(exists('eoo') == T){T} else {F}

            ) %>%
            cond_addPolygons(

              data = EOObuffer,
              color = "pink",
              fillOpacity = 0.3,
              weight = 1,
              group = "EOO buffer",
              execute = if(exists('EOObuffer') == T){T} else {F}

            ) %>%
            cond_addPolygons(

              data = AOOinCAR,
              color = "yellow",
              fillOpacity = 0.3,
              weight = 1,
              group = "CAR",
              execute = if(exists('AOOinCAR') == T){T} else {F}

            ) %>%
            addAwesomeMarkers(

              data = points,
              group = "Pontos",
              popup = ~URNs_ano_clt,
              icon = ~SetCoresAnoColeta[ano_clt_character]

            ) %>%
            addPolygons(

              data = aoo,
              color = "blue",
              fillOpacity = 0.01,
              weight = 1

            ) %>%
            addLayersControl(

              overlayGroups = c("Pontos", "CAR", "EOO", "EOO buffer"),
              options = layersControlOptions(collapsed = FALSE)


            ) %>%
            addMeasure(

              position = "bottomright",
              primaryLengthUnit = "kilometers",
              primaryAreaUnit = "sqmeters",
              localization = "pt_BR",
              activeColor = "#3D535D",
              completedColor = "#7D4479"

            )

        }

      } else {

        if(

          exists("MapBiomas_AOO") == T

        ){

          map <-

            leaflet(

              options = leafletOptions(preferCanvas = TRUE)

            ) %>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addPolygons(

              data = Estados_IBGE,
              fillOpacity = 0,
              weight = 2,
              color = "black"

            ) %>%
            addPolygons(

              data = Municipios_IBGE,
              fillOpacity = 0,
              weight = 2,
              color = "purple",
              label = ~Mun_ES

            ) %>%
            cond_addPolygons(

              data = eoo,
              color = "gray",
              fillOpacity = 0.3,
              weight = 1,
              group = "EOO",
              execute = if(exists('eoo') == T){T} else {F}

            ) %>%
            cond_addPolygons(

              data = EOObuffer,
              color = "pink",
              fillOpacity = 0.3,
              weight = 1,
              group = "EOO buffer",
              execute = if(exists('EOObuffer') == T){T} else {F}

            ) %>%
            cond_addPolygons(

              data = AOOinCAR,
              color = "yellow",
              fillOpacity = 0.3,
              weight = 1,
              group = "CAR",
              execute = if(exists('AOOinCAR') == T){T} else {F}

            ) %>%
            addAwesomeMarkers(

              data = points,
              group = "Pontos",
              popup = ~URNs_ano_clt,
              icon = ~SetCoresAnoColeta[ano_clt_character]

            ) %>%
            addPolygons(

              data = UCs_shape[UCs_intersected,],
              color = "lightgreen",
              fillOpacity = 0.01,
              weight = 1,
              label = ~NOME_UC1,
              group = "UCs"

            ) %>%
            addPolygons(

              data = aoo,
              color = "blue",
              fillOpacity = 0.01,
              weight = 1

            ) %>%
            addMeasure(

              position = "bottomright",
              primaryLengthUnit = "kilometers",
              primaryAreaUnit = "sqmeters",
              localization = "pt_BR",
              activeColor = "#3D535D",
              completedColor = "#7D4479"

            ) %>%
            addLayersControl(

              overlayGroups = c(

                "Pontos",
                "UCs",
                "CAR",
                "EOO",
                "EOO buffer",
                "MapBiomas 2020 - AOO",
                "Porcentagem convertida 2020",
                "Tendência 1985-2020"

              ),
              baseGroups = c(

                if(exists('Buffer1') == T){paste0(Locations_Buffers$Buffer[1]/1000, ' km | ', Locations_Buffers$N_Locations[1], ' locations')},
                if(exists('Buffer2') == T){paste0(Locations_Buffers$Buffer[2]/1000, ' km | ', Locations_Buffers$N_Locations[2], ' locations')},
                if(exists('Buffer3') == T){paste0(Locations_Buffers$Buffer[3]/1000, ' km | ', Locations_Buffers$N_Locations[3], ' locations')},
                if(exists('Buffer4') == T){paste0(Locations_Buffers$Buffer[4]/1000, ' km | ', Locations_Buffers$N_Locations[4], ' locations')},
                if(exists('Buffer5') == T){paste0(Locations_Buffers$Buffer[5]/1000, ' km | ', Locations_Buffers$N_Locations[5], ' locations')},
                if(exists('Buffer6') == T){paste0(Locations_Buffers$Buffer[6]/1000, ' km | ', Locations_Buffers$N_Locations[6], ' locations')},
                if(exists('Buffer7') == T){paste0(Locations_Buffers$Buffer[7]/1000, ' km | ', Locations_Buffers$N_Locations[7], ' locations')},
                if(exists('Buffer8') == T){paste0(Locations_Buffers$Buffer[8]/1000, ' km | ', Locations_Buffers$N_Locations[8], ' locations')},
                if(exists('Buffer9') == T){paste0(Locations_Buffers$Buffer[9]/1000, ' km | ', Locations_Buffers$N_Locations[9], ' locations')},
                if(exists('Buffer10') == T){paste0(Locations_Buffers$Buffer[10]/1000, ' km | ', Locations_Buffers$N_Locations[10], ' locations')},
                if(exists('Buffer11') == T){paste0(Locations_Buffers$Buffer[11]/1000, ' km | ', Locations_Buffers$N_Locations[11], ' locations')},
                if(exists('Buffer12') == T){paste0(Locations_Buffers$Buffer[12]/1000, ' km | ', Locations_Buffers$N_Locations[12], ' locations')},
                if(exists('Buffer13') == T){paste0(Locations_Buffers$Buffer[13]/1000, ' km | ', Locations_Buffers$N_Locations[13], ' locations')},
                if(exists('Buffer14') == T){paste0(Locations_Buffers$Buffer[14]/1000, ' km | ', Locations_Buffers$N_Locations[14], ' locations')},
                if(exists('Buffer15') == T){paste0(Locations_Buffers$Buffer[15]/1000, ' km | ', Locations_Buffers$N_Locations[15], ' locations')},
                if(exists('Buffer16') == T){paste0(Locations_Buffers$Buffer[16]/1000, ' km | ', Locations_Buffers$N_Locations[16], ' locations')},
                if(exists('Buffer17') == T){paste0(Locations_Buffers$Buffer[17]/1000, ' km | ', Locations_Buffers$N_Locations[17], ' locations')},
                if(exists('Buffer18') == T){paste0(Locations_Buffers$Buffer[18]/1000, ' km | ', Locations_Buffers$N_Locations[18], ' locations')},
                if(exists('Buffer19') == T){paste0(Locations_Buffers$Buffer[19]/1000, ' km | ', Locations_Buffers$N_Locations[19], ' locations')},
                if(exists('Buffer20') == T){paste0(Locations_Buffers$Buffer[20]/1000, ' km | ', Locations_Buffers$N_Locations[20], ' locations')},
                if(exists('Buffer21') == T){paste0(Locations_Buffers$Buffer[21]/1000, ' km | ', Locations_Buffers$N_Locations[21], ' locations')},
                if(exists('Buffer22') == T){paste0(Locations_Buffers$Buffer[22]/1000, ' km | ', Locations_Buffers$N_Locations[22], ' locations')},
                if(exists('Buffer23') == T){paste0(Locations_Buffers$Buffer[23]/1000, ' km | ', Locations_Buffers$N_Locations[23], ' locations')},
                if(exists('Buffer24') == T){paste0(Locations_Buffers$Buffer[24]/1000, ' km | ', Locations_Buffers$N_Locations[24], ' locations')},
                if(exists('Buffer25') == T){paste0(Locations_Buffers$Buffer[25]/1000, ' km | ', Locations_Buffers$N_Locations[25], ' locations')},
                if(exists('Buffer26') == T){paste0(Locations_Buffers$Buffer[26]/1000, ' km | ', Locations_Buffers$N_Locations[26], ' locations')},
                if(exists('Buffer27') == T){paste0(Locations_Buffers$Buffer[27]/1000, ' km | ', Locations_Buffers$N_Locations[27], ' locations')},
                if(exists('Buffer28') == T){paste0(Locations_Buffers$Buffer[28]/1000, ' km | ', Locations_Buffers$N_Locations[28], ' locations')},
                if(exists('Buffer29') == T){paste0(Locations_Buffers$Buffer[29]/1000, ' km | ', Locations_Buffers$N_Locations[29], ' locations')},
                if(exists('Buffer30') == T){paste0(Locations_Buffers$Buffer[30]/1000, ' km | ', Locations_Buffers$N_Locations[30], ' locations')},
                if(exists('Buffer31') == T){paste0(Locations_Buffers$Buffer[31]/1000, ' km | ', Locations_Buffers$N_Locations[31], ' locations')},
                if(exists('Buffer32') == T){paste0(Locations_Buffers$Buffer[32]/1000, ' km | ', Locations_Buffers$N_Locations[32], ' locations')},
                if(exists('Buffer33') == T){paste0(Locations_Buffers$Buffer[33]/1000, ' km | ', Locations_Buffers$N_Locations[33], ' locations')},
                if(exists('Buffer34') == T){paste0(Locations_Buffers$Buffer[34]/1000, ' km | ', Locations_Buffers$N_Locations[34], ' locations')},
                if(exists('Buffer35') == T){paste0(Locations_Buffers$Buffer[35]/1000, ' km | ', Locations_Buffers$N_Locations[35], ' locations')},
                if(exists('Buffer36') == T){paste0(Locations_Buffers$Buffer[36]/1000, ' km | ', Locations_Buffers$N_Locations[36], ' locations')},
                if(exists('Buffer37') == T){paste0(Locations_Buffers$Buffer[37]/1000, ' km | ', Locations_Buffers$N_Locations[37], ' locations')},
                if(exists('Buffer38') == T){paste0(Locations_Buffers$Buffer[38]/1000, ' km | ', Locations_Buffers$N_Locations[38], ' locations')},
                if(exists('Buffer39') == T){paste0(Locations_Buffers$Buffer[39]/1000, ' km | ', Locations_Buffers$N_Locations[39], ' locations')},
                if(exists('Buffer40') == T){paste0(Locations_Buffers$Buffer[40]/1000, ' km | ', Locations_Buffers$N_Locations[40], ' locations')},
                if(exists('Buffer41') == T){paste0(Locations_Buffers$Buffer[41]/1000, ' km | ', Locations_Buffers$N_Locations[41], ' locations')},
                if(exists('Buffer42') == T){paste0(Locations_Buffers$Buffer[42]/1000, ' km | ', Locations_Buffers$N_Locations[42], ' locations')},
                if(exists('Buffer43') == T){paste0(Locations_Buffers$Buffer[43]/1000, ' km | ', Locations_Buffers$N_Locations[43], ' locations')},
                if(exists('Buffer44') == T){paste0(Locations_Buffers$Buffer[44]/1000, ' km | ', Locations_Buffers$N_Locations[44], ' locations')},
                if(exists('Buffer45') == T){paste0(Locations_Buffers$Buffer[45]/1000, ' km | ', Locations_Buffers$N_Locations[45], ' locations')},
                if(exists('Buffer46') == T){paste0(Locations_Buffers$Buffer[46]/1000, ' km | ', Locations_Buffers$N_Locations[46], ' locations')},
                if(exists('Buffer47') == T){paste0(Locations_Buffers$Buffer[47]/1000, ' km | ', Locations_Buffers$N_Locations[47], ' locations')},
                if(exists('Buffer48') == T){paste0(Locations_Buffers$Buffer[48]/1000, ' km | ', Locations_Buffers$N_Locations[48], ' locations')},
                if(exists('Buffer49') == T){paste0(Locations_Buffers$Buffer[49]/1000, ' km | ', Locations_Buffers$N_Locations[49], ' locations')},
                if(exists('Buffer50') == T){paste0(Locations_Buffers$Buffer[50]/1000, ' km | ', Locations_Buffers$N_Locations[50], ' locations')}

              ),
              options = layersControlOptions(collapsed = FALSE)

            ) %>%
            cond_addPolygons(data = Buffer1@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[1]/1000, ' km | ', Locations_Buffers$N_Locations[1], ' locations'), execute = if(exists('Buffer1') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer2@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[2]/1000, ' km | ', Locations_Buffers$N_Locations[2], ' locations'), execute = if(exists('Buffer2') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer3@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[3]/1000, ' km | ', Locations_Buffers$N_Locations[3], ' locations'), execute = if(exists('Buffer3') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer4@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[4]/1000, ' km | ', Locations_Buffers$N_Locations[4], ' locations'), execute = if(exists('Buffer4') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer5@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[5]/1000, ' km | ', Locations_Buffers$N_Locations[5], ' locations'), execute = if(exists('Buffer5') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer6@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[6]/1000, ' km | ', Locations_Buffers$N_Locations[6], ' locations'), execute = if(exists('Buffer6') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer7@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[7]/1000, ' km | ', Locations_Buffers$N_Locations[7], ' locations'), execute = if(exists('Buffer7') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer8@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[8]/1000, ' km | ', Locations_Buffers$N_Locations[8], ' locations'), execute = if(exists('Buffer8') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer9@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[9]/1000, ' km | ', Locations_Buffers$N_Locations[9], ' locations'), execute = if(exists('Buffer9') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer10@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[10]/1000, ' km | ', Locations_Buffers$N_Locations[10], ' locations'), execute = if(exists('Buffer10') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer11@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[11]/1000, ' km | ', Locations_Buffers$N_Locations[11], ' locations'), execute = if(exists('Buffer11') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer12@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[12]/1000, ' km | ', Locations_Buffers$N_Locations[12], ' locations'), execute = if(exists('Buffer12') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer13@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[13]/1000, ' km | ', Locations_Buffers$N_Locations[13], ' locations'), execute = if(exists('Buffer13') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer14@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[14]/1000, ' km | ', Locations_Buffers$N_Locations[14], ' locations'), execute = if(exists('Buffer14') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer15@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[15]/1000, ' km | ', Locations_Buffers$N_Locations[15], ' locations'), execute = if(exists('Buffer15') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer16@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[16]/1000, ' km | ', Locations_Buffers$N_Locations[16], ' locations'), execute = if(exists('Buffer16') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer17@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[17]/1000, ' km | ', Locations_Buffers$N_Locations[17], ' locations'), execute = if(exists('Buffer17') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer18@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[18]/1000, ' km | ', Locations_Buffers$N_Locations[18], ' locations'), execute = if(exists('Buffer18') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer19@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[19]/1000, ' km | ', Locations_Buffers$N_Locations[19], ' locations'), execute = if(exists('Buffer19') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer20@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[20]/1000, ' km | ', Locations_Buffers$N_Locations[20], ' locations'), execute = if(exists('Buffer20') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer21@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[21]/1000, ' km | ', Locations_Buffers$N_Locations[21], ' locations'), execute = if(exists('Buffer21') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer22@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[22]/1000, ' km | ', Locations_Buffers$N_Locations[22], ' locations'), execute = if(exists('Buffer22') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer23@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[23]/1000, ' km | ', Locations_Buffers$N_Locations[23], ' locations'), execute = if(exists('Buffer23') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer24@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[24]/1000, ' km | ', Locations_Buffers$N_Locations[24], ' locations'), execute = if(exists('Buffer24') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer25@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[25]/1000, ' km | ', Locations_Buffers$N_Locations[25], ' locations'), execute = if(exists('Buffer25') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer26@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[26]/1000, ' km | ', Locations_Buffers$N_Locations[26], ' locations'), execute = if(exists('Buffer26') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer27@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[27]/1000, ' km | ', Locations_Buffers$N_Locations[27], ' locations'), execute = if(exists('Buffer27') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer28@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[28]/1000, ' km | ', Locations_Buffers$N_Locations[28], ' locations'), execute = if(exists('Buffer28') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer29@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[29]/1000, ' km | ', Locations_Buffers$N_Locations[29], ' locations'), execute = if(exists('Buffer29') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer30@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[30]/1000, ' km | ', Locations_Buffers$N_Locations[30], ' locations'), execute = if(exists('Buffer30') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer31@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[31]/1000, ' km | ', Locations_Buffers$N_Locations[31], ' locations'), execute = if(exists('Buffer31') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer32@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[32]/1000, ' km | ', Locations_Buffers$N_Locations[32], ' locations'), execute = if(exists('Buffer32') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer33@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[33]/1000, ' km | ', Locations_Buffers$N_Locations[33], ' locations'), execute = if(exists('Buffer33') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer34@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[34]/1000, ' km | ', Locations_Buffers$N_Locations[34], ' locations'), execute = if(exists('Buffer34') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer35@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[35]/1000, ' km | ', Locations_Buffers$N_Locations[35], ' locations'), execute = if(exists('Buffer35') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer36@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[36]/1000, ' km | ', Locations_Buffers$N_Locations[36], ' locations'), execute = if(exists('Buffer36') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer37@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[37]/1000, ' km | ', Locations_Buffers$N_Locations[37], ' locations'), execute = if(exists('Buffer37') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer38@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[38]/1000, ' km | ', Locations_Buffers$N_Locations[38], ' locations'), execute = if(exists('Buffer38') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer39@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[39]/1000, ' km | ', Locations_Buffers$N_Locations[39], ' locations'), execute = if(exists('Buffer39') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer40@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[40]/1000, ' km | ', Locations_Buffers$N_Locations[40], ' locations'), execute = if(exists('Buffer40') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer41@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[41]/1000, ' km | ', Locations_Buffers$N_Locations[41], ' locations'), execute = if(exists('Buffer41') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer42@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[42]/1000, ' km | ', Locations_Buffers$N_Locations[42], ' locations'), execute = if(exists('Buffer42') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer43@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[43]/1000, ' km | ', Locations_Buffers$N_Locations[43], ' locations'), execute = if(exists('Buffer43') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer44@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[44]/1000, ' km | ', Locations_Buffers$N_Locations[44], ' locations'), execute = if(exists('Buffer44') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer45@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[45]/1000, ' km | ', Locations_Buffers$N_Locations[45], ' locations'), execute = if(exists('Buffer45') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer46@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[46]/1000, ' km | ', Locations_Buffers$N_Locations[46], ' locations'), execute = if(exists('Buffer46') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer47@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[47]/1000, ' km | ', Locations_Buffers$N_Locations[47], ' locations'), execute = if(exists('Buffer47') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer48@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[48]/1000, ' km | ', Locations_Buffers$N_Locations[48], ' locations'), execute = if(exists('Buffer48') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer49@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[49]/1000, ' km | ', Locations_Buffers$N_Locations[49], ' locations'), execute = if(exists('Buffer49') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer50@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[50]/1000, ' km | ', Locations_Buffers$N_Locations[50], ' locations'), execute = if(exists('Buffer50') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer51@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[51]/1000, ' km | ', Locations_Buffers$N_Locations[51], ' locations'), execute = if(exists('Buffer51') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer52@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[52]/1000, ' km | ', Locations_Buffers$N_Locations[52], ' locations'), execute = if(exists('Buffer52') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer53@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[53]/1000, ' km | ', Locations_Buffers$N_Locations[53], ' locations'), execute = if(exists('Buffer53') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer54@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[54]/1000, ' km | ', Locations_Buffers$N_Locations[54], ' locations'), execute = if(exists('Buffer54') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer55@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[55]/1000, ' km | ', Locations_Buffers$N_Locations[55], ' locations'), execute = if(exists('Buffer55') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer56@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[56]/1000, ' km | ', Locations_Buffers$N_Locations[56], ' locations'), execute = if(exists('Buffer56') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer57@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[57]/1000, ' km | ', Locations_Buffers$N_Locations[57], ' locations'), execute = if(exists('Buffer57') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer58@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[58]/1000, ' km | ', Locations_Buffers$N_Locations[58], ' locations'), execute = if(exists('Buffer58') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer59@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[59]/1000, ' km | ', Locations_Buffers$N_Locations[59], ' locations'), execute = if(exists('Buffer59') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer60@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[60]/1000, ' km | ', Locations_Buffers$N_Locations[60], ' locations'), execute = if(exists('Buffer60') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer61@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[61]/1000, ' km | ', Locations_Buffers$N_Locations[61], ' locations'), execute = if(exists('Buffer61') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer62@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[62]/1000, ' km | ', Locations_Buffers$N_Locations[62], ' locations'), execute = if(exists('Buffer62') == T){T} else {F}) %>%
            cond_addPolygons(data = Buffer63@polygons, fillOpacity = 0.05, weight = 1.5, color = "orange", group = paste0(Locations_Buffers$Buffer[63]/1000, ' km | ', Locations_Buffers$N_Locations[63], ' locations'), execute = if(exists('Buffer63') == T){T} else {F}) %>%
            addPolygons(

              data = MapBiomas_AOO,
              fillOpacity = 0.9,
              weight = 1,
              color = "white",
              fillColor = CoresLegenda,
              label = Legenda,
              group = "MapBiomas 2020 - AOO"

            ) %>%
            cond_addPolygons(

              data = AOO_QuadOfGrid_shapefile,
              fillOpacity = 1,
              weight = 0,
              label = ~Quad_Taxa_anual_2020,
              color = ~paletaTrend(Taxa_anual),
              group = "Tendência 1985-2020",
              execute = if(exists('AOO_QuadOfGrid_shapefile') == T){T} else {F}

            ) %>%
            addLegend(

              "bottomleft",
              pal = paletaTrend,
              values = AOO_QuadOfGrid_shapefile$Taxa_anual,
              labFormat = labelFormat(suffix = "%"),
              title = "Uso Alternativo x Natural (% aa)",
              group = "Tendência 1985-2020",
              opacity = 1

            ) %>%
            cond_addPolygons(

              data = AOO_QuadOfGrid_shapefile,
              fillOpacity = 1,
              weight = 0,
              label = ~Quad_Porcentagem_2020,
              color = ~paletaPorc(Porcentagem_2020),
              group = "Porcentagem convertida 2020",
              execute = if(exists('AOO_QuadOfGrid_shapefile') == T){T} else {F}

            ) %>%
            addLegend(

              "bottomleft",
              pal = paletaPorc,
              values = AOO_QuadOfGrid_shapefile$Porcentagem_2020,
              labFormat = labelFormat(suffix = "%"),
              title = "Uso Alternativo (%)",
              group = "Porcentagem convertida 2020",
              opacity = 1

            )

        } else {

          map <-
            leaflet(

              options = leafletOptions(preferCanvas = TRUE)

            ) %>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addPolygons(

              data = Estados_IBGE,
              fillOpacity = 0,
              weight = 2,
              color = "black"

            ) %>%
            addPolygons(

              data = Municipios_IBGE,
              fillOpacity = 0,
              weight = 2,
              color = "purple",
              label = ~Mun_ES

            ) %>%
            cond_addPolygons(

              data = eoo,
              color = "gray",
              fillOpacity = 0.3,
              weight = 1,
              group = "EOO",
              execute = if(exists('eoo') == T){T} else {F}

            ) %>%
            cond_addPolygons(

              data = EOObuffer,
              color = "pink",
              fillOpacity = 0.3,
              weight = 1,
              group = "EOO buffer",
              execute = if(exists('EOObuffer') == T){T} else {F}

            ) %>%
            cond_addPolygons(

              data = AOOinCAR,
              color = "yellow",
              fillOpacity = 0.3,
              weight = 1,
              group = "CAR",
              execute = if(exists('AOOinCAR') == T){T} else {F}

            ) %>%
            addAwesomeMarkers(

              data = points,
              group = "Pontos",
              popup = ~URNs_ano_clt,
              icon = ~SetCoresAnoColeta[ano_clt_character]

            ) %>%
            addPolygons(

              data = UCs_shape[UCs_intersected,],
              color = "lightgreen",
              fillOpacity = 0.01,
              weight = 1,
              label = ~NOME_UC1,
              group = "UCs"

            ) %>%
            addPolygons(

              data = aoo,
              color = "blue",
              fillOpacity = 0.01,
              weight = 1

            ) %>%
            addMeasure(

              position = "bottomright",
              primaryLengthUnit = "kilometers",
              primaryAreaUnit = "sqmeters",
              localization = "pt_BR",
              activeColor = "#3D535D",
              completedColor = "#7D4479"

            ) %>%
            addLayersControl(

              overlayGroups = c(

                "Pontos",
                "UCs",
                "CAR",
                "EOO",
                "EOO buffer"

              ),
              options = layersControlOptions(collapsed = FALSE))

        }

      }

    } else {

      if(length(UCs_intersected)==0){

        map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          addPolygons(data = Estados_IBGE,
                      fillOpacity = 0,
                      weight = 2,
                      color = "black") %>%
          addPolygons(data = Municipios_IBGE,
                      fillOpacity = 0,
                      weight = 2,
                      color = "purple",
                      label = ~Mun_ES) %>%
          cond_addPolygons(data = eoo,
                           color = "gray",
                           fillOpacity = 0.3,
                           weight = 1,
                           group = "EOO",
                           execute = if(exists('eoo') == T){T} else {F}) %>%
          cond_addPolygons(data = EOObuffer,
                           color = "pink",
                           fillOpacity = 0.3,
                           weight = 1,
                           group = "EOO buffer",
                           execute = if(exists('EOObuffer') == T){T} else {F}) %>%
          cond_addPolygons(data = AOOinCAR,
                           color = "yellow",
                           fillOpacity = 0.3,
                           weight = 1,
                           group = "CAR",
                           execute = if(exists('AOOinCAR') == T){T} else {F}) %>%
          addAwesomeMarkers(data = points,
                            group = "Pontos",
                            popup = ~URNs_ano_clt,
                            icon = ~SetCoresAnoColeta[ano_clt_character]
          ) %>%
          addLayersControl(overlayGroups = c("Pontos", "CAR", "EOO", "EOO buffer"),
                           options = layersControlOptions(collapsed = FALSE)
          ) %>%
          addMeasure(
            position = "bottomright",
            primaryLengthUnit = "kilometers",
            primaryAreaUnit = "sqmeters",
            localization = "pt_BR",
            activeColor = "#3D535D",
            completedColor = "#7D4479")

      } else {

        map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          addPolygons(data = Estados_IBGE,
                      fillOpacity = 0,
                      weight = 2,
                      color = "black") %>%
          addPolygons(data = Municipios_IBGE,
                      fillOpacity = 0,
                      weight = 2,
                      color = "purple",
                      label = ~Mun_ES) %>%
          cond_addPolygons(data = eoo,
                           color = "gray",
                           fillOpacity = 0.3,
                           weight = 1,
                           group = "EOO",
                           execute = if(exists('eoo') == T){T} else {F}) %>%
          cond_addPolygons(data = EOObuffer,
                           color = "pink",
                           fillOpacity = 0.3,
                           weight = 1,
                           group = "EOO buffer",
                           execute = if(exists('EOObuffer') == T){T} else {F}) %>%
          cond_addPolygons(data = AOOinCAR,
                           color = "yellow",
                           fillOpacity = 0.3,
                           weight = 1,
                           group = "CAR",
                           execute = if(exists('AOOinCAR') == T){T} else {F}) %>%
          addAwesomeMarkers(data = points,
                            group = "Pontos",
                            popup = ~URNs_ano_clt,
                            icon = ~SetCoresAnoColeta[ano_clt_character]
          ) %>%
          addPolygons(data = UCs_shape[UCs_intersected,],
                      color = "lightgreen",
                      fillOpacity = 0.01,
                      weight = 1,
                      label = ~NOME_UC1,
                      group = "UCs") %>%
          addMeasure(
            position = "bottomright",
            primaryLengthUnit = "kilometers",
            primaryAreaUnit = "sqmeters",
            localization = "pt_BR",
            activeColor = "#3D535D",
            completedColor = "#7D4479") %>%
          addLayersControl(overlayGroups = c("Pontos", "UCs", "CAR", "EOO", "EOO buffer"),
                           options = layersControlOptions(collapsed = FALSE))

      }

    }

    if(exists("MapBiomas_EOO") == T){

      points <-
        st_read(paste0("POINTS ", SPECIES, ".shp")) %>%
        st_transform(4326)

      points$URNs_ano_clt <-
        paste0(points$URNs, "<br>Ano de coleta: <b>", points$ano_clt, "</b>")

      points$ano_clt_character <-
        as.character(paste0("ano_",points$ano_clt))

      points <-
        st_as_sf(points, coords = c("lon", "lat"), crs = 4326, agr = "constant")

      map_EOO <-
        leaflet(

          options = leafletOptions(preferCanvas = TRUE)

        ) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addAwesomeMarkers(

          data = points,
          group = "Pontos",
          popup = ~URNs_ano_clt,
          icon = ~SetCoresAnoColeta[ano_clt_character]

        ) %>%
        addLayersControl(overlayGroups = c("Pontos", "MapBiomas 2020 - EOO")) %>%
        addPolygons(

          data = MapBiomas_EOO,
          fillOpacity = 0.9,
          weight = 1,
          color = "white",
          fillColor = CoresLegenda_EOO,
          label = Legenda_EOO,
          group = "MapBiomas 2020 - EOO"

        )

      saveWidget(map_EOO, "map_EOO.html", selfcontained = T)
      file.rename("map_EOO.html", "map_EOO.txt")

      map_EOO_widget <- readtext("map_EOO.txt")

      map_EOO_widget_html <-
        str_extract(

          map_EOO_widget$text,
          '\\<div id\\=\\"htmlwidget\\-(.*?(\n))+.*?\\<\\/body\\>'

        )

      map_EOO_widget_html <-
        sub(

          "width:100%;height:400px",
          "width:100%;height:700px",
          map_EOO_widget_html

        )

    }

    saveWidget(map, "map.html", selfcontained = T)

    file.rename("map.html", "map.txt")

    map_widget <- readtext("map.txt")

    map_widget_html <-
      str_extract(

        map_widget$text,
        '\\<div id\\=\\"htmlwidget\\-(.*?(\n))+.*?\\<\\/body\\>'

      )

    map_widget_html <-
      sub(

        "width:100%;height:400px",
        "width:100%;height:700px",
        map_widget_html

      )

    header_HTML <- readtext("TextMining/header_html.txt")
    leaflet_header <- readtext("TextMining/leaflet_header.txt")
    leaflet_header$text <-
      sub(

        "Title_here",
        paste(i, "assessment"),
        leaflet_header$text

      )


    # Orações subordinadas concessivas e adversativas ####


    ## Condicionais ####


    ### Condicional: Incerteza da distribuição efetiva ####

    # if(
    #
    #   AOOnaoenquadraEOOenquadra == T |
    #   AOOenquadraEOOnaoenquadra == T |
    #   AOOenquadraEOOenquadra == T
    #
    # ){
    #
    #   labelIncertDistEfetiv =
    #     paste0(
    #       "<markG>",
    #       labelIncertDistEfetiv,
    #       "</markG>"
    #     )
    #
    # }


    ### Condicional: Área Pouco Estudada (Amazônia) ####

    if(

      (
        AOOnaoenquadraEOOenquadra == T |
        AOOenquadraEOOnaoenquadra == T |
        AOOenquadraEOOenquadra == T

      ) &
      onlyAmazonia == T

    ){

      labelAreaPoucoEstudada_incertDistEfetiv =
        paste0(
          "<markG>",
          labelAreaPoucoEstudada_incertDistEfetiv,
          "</markG>"
        )

    }

    # Compilação do HTML ####
    sink("output.txt")
    cat(leaflet_header$text)
    cat('<br><br>\n')
    cat(header_HTML$text)
    cat(paste0('<h1 style="margin-left: 10px;">', i, '</h1>', '\n'))
    cat('<br>\n')
    cat(kable(AOO_EOO_dt) %>%
          kable_styling(full_width = F, position = "center") %>%
          cond_row_spec(threatned_rowCR,
                        bold = T,
                        color = "white",
                        background = "#D7261E",
                        execute = if(exists("threatned_rowCR") == T){T} else {F}
          ) %>%
          cond_row_spec(threatned_rowEN,
                        bold = T,
                        color = "white",
                        background = "orange",
                        execute = if(exists("threatned_rowEN") == T){T} else {F}
          ) %>%
          cond_row_spec(threatned_rowVU,
                        bold = T,
                        color = "white",
                        background = "yellow",
                        execute = if(exists("threatned_rowVU") == T){T} else {F}
          )
    )
    cat('<br><br>\n<p style="margin-left: 10px;">')
    cat(output$Texto)
    cat('</p>\n<br>\n')

    cat('<div class="container-fluid">\n\n')
    cat('  <ul class="nav nav-tabs\n">')
    cat('    <li class="active"><a href="#menu1_mapa">Mapa principal</a></li>\n')
    cat('    <li><a href="#menu2_mapaEOO">MapBiomas 2020 na EOO</a></li>\n')
    cat('  </ul>\n\n')
    cat('<div class="tab-content">\n')
    cat('  <div id="menu1_mapa" class="tab-pane fade in active">\n')
    cat(map_widget_html)

    cat('  <div id="menu2_mapaEOO" class="tab-pane fade">\n')
    if(

      exists("map_EOO_widget_html") == T

    ){

      cat(map_EOO_widget_html)

    } else {

      cat("Sem EOO")

    }
    cat('  </div>\n\n')

    ## Considerations ####

    cat('<h4><b>Considerações:</b></h4>\n')

    ### Amplitude de Distribuição ####
    if(

      AmplaDistribuicao == T | DistribuicaoRestrita == T

    ){

      cat("<b>Amplitude de distribuição:</b><br>\n\n")

    }

    if(

      AmplaDistribuicao == T

    ){

      cat('<input id="AmplamenteDistribuida" type="checkbox" style="float: left;" value="')
      cat("sua ampla distribuição")
      cat('" onclick="incluirNaFrase(\n')
      cat("'")
      cat("sua ampla distribuição")
      cat("')")
      cat('"><label for="AmplamenteDistribuida" style="display: flex; margin-bottom: -12px;">')
      cat(labelAmplamenteDistribuida)
      cat('</label>\n<br><br>\n')

    }

    if(

      DistribuicaoRestrita == T

    ){

      cat('<input id="DistribuicaoRestrita" type="checkbox" style="float: left;" value="')
      cat("sua distribuição restrita")
      cat('" onclick="incluirNaFrase(\n')
      cat("'")
      cat("sua distribuição restrita")
      cat("')")
      cat('"><label for="DistribuicaoRestrita" style="display: flex; margin-bottom: -12px;">')
      cat(labelDistribuicaoRestrita)
      cat('</label>\n<br><br>\n')

    }

    ### Área conhecida/coletada ####
    cat("<b>Conhecimento da área de distribuição:</b><br>\n\n")

    cat('<input id="AreaConhecida1" type="checkbox" style="float: left;" value="')
    cat("sua área de ocorrência relativamente bem amostrada")
    cat('" onclick="incluirNaFrase(\n')
    cat("'")
    cat("sua área de ocorrência relativamente bem amostrada")
    cat("')")
    cat('"><label for="AreaConhecida1" style="display: flex; margin-bottom: -12px;">')
    cat(labelAreaConhecida1)
    cat('</label><br>\n')

    cat("\n")

    cat('<input id="labelAreaConhecida2" type="checkbox" style="float: left;" value="')
    cat("sua distribuição em região pouco estudada, onde esforços de coleta ainda são necessários")
    cat('" onclick="incluirNaFrase(\n')
    cat("'")
    cat("sua distribuição em região pouco estudada, onde esforços de coleta ainda são necessários")
    cat("')")
    cat('"><label for="labelAreaConhecida2" style="display: flex; margin-bottom: -12px;">')
    cat(labelAreaConhecida2)
    cat('</label>\n<br><br>\n')

    ### AOO e EOO ####
    cat("<b>AOO e EOO:</b><br>\n\n")

    cat('<input id="myCheck1" type="checkbox" style="float: left;" value="')
    cat(AOO_EOO)
    cat('" onclick="incluirNaFrase(\n')
    cat("'")
    cat(AOO_EOO)
    cat("')")
    cat('"><label for="myCheck1">')
    cat(AOO_EOO)
    cat('</label>\n<br><br>\n')

    ### Anos decorridos do intervalo de coletas ####
    cat("<b>Anos decorridos do intervalo de coletas:</b><br>\n\n")

    cat('<input id="myCheck2" type="checkbox" style="float: left;" value="')
    cat(yearsElapsedSinceLastCollections)
    cat('" onclick="incluirNaFrase(\n')
    cat("'")
    cat(yearsElapsedSinceLastCollections)
    cat("')")
    cat('"><label for="myCheck2">')
    cat(yearsElapsedSinceLastCollections)
    cat('</label>\n<br><br>\n')

    ### Proporção de conversão para uso alternativo do solo ####
    if(

      NenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado == F &
      (

        AltaConversaoUsoDoSoloAOOEOO == T |
        AltaConversaoUsoDoSoloAOO == T |
        AltaConversaoUsoDoSoloEOO == T |
        ModeradaConversaoUsoDoSoloAOOEOO == T |
        ModeradaConversaoUsoDoSoloAOO == T |
        ModeradaConversaoUsoDoSoloEOO == T |
        BaixaConversaoUsoDoSoloAOOEOO == T |
        BaixaConversaoUsoDoSoloAOO == T |
        BaixaConversaoUsoDoSoloEOO == T |
        BaixaConversaoUsoDoSoloAOOEOOAmaz == T

      )

    ){

      cat("<b>Proporção de conversão para uso alternativo do solo (MapBiomas 2020):</b><br>\n\n")
      cat('<div class="container">\n')
      cat('<ul class="nav nav-tabs">\n')
      cat('  <li class="active"><a href="#menu1_TabelaSobreposicao">Análise de sobreposição (MapBiomas)</a></li>\n')
      cat('    <li><a href="#menu2_GraficoAOO">Gráfico AOO</a></li>\n')
      cat('    <li><a href="#menu3_GraficoAOOinEOObuffer">Gráfico AOO in EOO buffer</a></li>\n')
      cat('    <li><a href="#menu4_GraficoEOO">Gráfico EOO</a></li>\n')
      cat('        </ul>\n\n')
      cat('        <div class="tab-content">\n')
      cat('          <div id="menu1_TabelaSobreposicao" class="tab-pane fade in active">\n')
      if(

        nrow(sp_in_MapBiomas_coverLand) > 0

      ){

        if(

          (

            sum(sp_in_MapBiomas_coverLand$`Área Ocupada da AOO (km²)`) > 0 &
            sp_in_MapBiomas_coverLand$`Área Ocupada da EOO (km²)`[1] == "Análise dispensada"

          ) |
          (

            sum(sp_in_MapBiomas_coverLand$`Área Ocupada da AOO (km²)`) > 0 &
            is.numeric(suppressWarnings(as.numeric(sp_in_MapBiomas_coverLand$`Área Ocupada da EOO (km²)`))) == T

          )

        ){
          cat(kable(sp_in_MapBiomas_coverLand %>%
                      dplyr::filter(`Área Ocupada da AOO (km²)` != 0 &
                                      `Área Ocupada da AOO (km²)` != 0)) %>%
                kable_styling(font_size = 16) %>%
                kable_paper("hover", full_width = F))
        }
        cat("\n")
        cat("          </div>\n")
        cat('        <div id="menu2_GraficoAOO" class="tab-pane fade">\n')
        if(

          exists("dataPieAOO") == T

        ){

          if(

            dataPieAOO$Porcentagem_AOO_2020[1] != 100

          ){

            cat('          <img src="')
            cat(image_uri(paste0("PieAOO-", i, ".png")))
            cat('" width="500" height="500"><br>\n')

          } else {

            cat("          <br>\n")

          }

        }
        cat("          </div>\n")

        cat('        <div id="menu3_GraficoAOOinEOObuffer" class="tab-pane fade">\n')
        if(

          exists("dataPieAOO") == T

        ){

          if(

            dataPie_AOOinEOObuffer$Porcentagem_AOO_2020[1] != 100

          ){

            cat('          <img src="')
            cat(image_uri(paste0("PieAOOinEOObuffer-", i, ".png")))
            cat('" width="500" height="500"><br>\n')

          } else {

            cat("          <br>\n")

          }

        }
        cat("          </div>\n")

        cat('        <div id="menu4_GraficoEOO" class="tab-pane fade">\n')
        if(

          exists("dataPieEOO") == T

        ){

          cat('          <img src="')
          cat(image_uri(paste0("PieEOO-", i, ".png")))
          cat('" width="500" height="500"><br>\n')

        } else {

          cat("          <br>\n")

        }
        cat("          </div>\n")
        cat("        </div>\n")
        cat("      </div>\n\n")
        cat("<script>\n")
        cat('  $(document).ready(function(){\n')
        cat('    $(".nav-tabs a").click(function(){\n')
        cat("      $(this).tab('show');\n")
        cat("    });\n")
        cat("  });\n")
        cat("</script>\n\n")
      }

      ## Definição das principais ameaças
      sp_in_MapBiomas_coverLand2 <-
        sp_in_MapBiomas_coverLand

      sp_in_MapBiomas_coverLand2 <-
        sp_in_MapBiomas_coverLand2 %>%
        dplyr::filter (Ameaça != "Total")

      sp_in_MapBiomas_coverLand2_Agricultura <-
        sp_in_MapBiomas_coverLand2 %>%
        dplyr::filter(

          Ameaça %in%
            c(

              "Soja",
              "Cana-de-açúcar",
              "Citrus",
              "Arroz",
              "Café",
              "Outras Lavouras Perenes",
              "Outras lavouras temporárias",
              "Lavouras perenes"

            )

        )

      sp_in_MapBiomas_coverLand2_Agricultura <-
        sp_in_MapBiomas_coverLand2_Agricultura %>%
        summarise(

          Ameaça = "Agricultura",
          `AOO útil (km²)` = "",
          `Área Ocupada da AOO (km²)` = sum(`Área Ocupada da AOO (km²)`),
          `Área Ocupada da AOO (%)`= sum(`Área Ocupada da AOO (%)`),
          `EOO` = "",
          `Área Ocupada da EOO (km²)` =
            if(is.numeric(sp_in_MapBiomas_coverLand2_Agricultura$`Área Ocupada da EOO (km²)`) == T){

              sum(`Área Ocupada da EOO (km²)`)

            } else {

              sp_in_MapBiomas_coverLand2_Agricultura$`Área Ocupada da EOO (km²)`[1]

            },
          `Área Ocupada da EOO (%)` =
            if(is.numeric(sp_in_MapBiomas_coverLand2_Agricultura$`Área Ocupada da EOO (%)`) == T){

              sum(`Área Ocupada da EOO (%)`)

            } else {

              sp_in_MapBiomas_coverLand2_Agricultura$`Área Ocupada da EOO (%)`[1]

            }

        )

      sp_in_MapBiomas_coverLand2 <-
        sp_in_MapBiomas_coverLand2 %>%
        dplyr::filter(

          !Ameaça %in%
            c(

              "Soja",
              "Cana-de-açúcar",
              "Citrus",
              "Arroz",
              "Café",
              "Outras Lavouras Perenes",
              "Outras lavouras temporárias",
              "Lavouras perenes"

            )

        )

      sp_in_MapBiomas_coverLand2 <-
        rbind(

          sp_in_MapBiomas_coverLand2,
          sp_in_MapBiomas_coverLand2_Agricultura,
          use.names=FALSE

        )

      sp_in_MapBiomas_coverLand2 <-
        sp_in_MapBiomas_coverLand2 %>%
        dplyr::arrange(desc(`Área Ocupada da AOO (%)`))

      sp_in_MapBiomas_coverLand2 <-
        sp_in_MapBiomas_coverLand2 %>%
        dplyr::filter(

          `Área Ocupada da AOO (km²)` != 0 &
            (`Área Ocupada da EOO (km²)` != 0 |
               `Área Ocupada da EOO (km²)` == "Análise dispensada"

            )

        )

      sp_in_MapBiomas_coverLand2$inputID <- sp_in_MapBiomas_coverLand2$Ameaça
      sp_in_MapBiomas_coverLand2$inputID <- sub("Mosaico Agricultura e Pastagem", "MOSAICO", sp_in_MapBiomas_coverLand2$inputID)
      sp_in_MapBiomas_coverLand2$inputID <- sub("Agricultura", "AGRICULTURA", sp_in_MapBiomas_coverLand2$inputID)
      sp_in_MapBiomas_coverLand2$inputID <- sub("Pastagem", "PECUARIA", sp_in_MapBiomas_coverLand2$inputID)
      sp_in_MapBiomas_coverLand2$inputID <- sub("Infraestrutura Urbana", "INFRAURBANA", sp_in_MapBiomas_coverLand2$inputID)
      sp_in_MapBiomas_coverLand2$inputID <- sub("Silvicultura", "SILVICULTURA", sp_in_MapBiomas_coverLand2$inputID)
      sp_in_MapBiomas_coverLand2$inputID <- sub("Mineração", "MINERACAO", sp_in_MapBiomas_coverLand2$inputID)

      sp_in_MapBiomas_coverLand2$value <- sp_in_MapBiomas_coverLand2$Ameaça
      sp_in_MapBiomas_coverLand2$value <- sub("Mosaico Agricultura e Pastagem", "agricultura e pastagem consorciadas", sp_in_MapBiomas_coverLand2$value)
      sp_in_MapBiomas_coverLand2$value <- sub("Agricultura", "agricultura", sp_in_MapBiomas_coverLand2$value)
      sp_in_MapBiomas_coverLand2$value <- sub("Pastagem", "pecuária", sp_in_MapBiomas_coverLand2$value)
      sp_in_MapBiomas_coverLand2$value <- sub("Infraestrutura Urbana", "infraestrutura urbana", sp_in_MapBiomas_coverLand2$value)
      sp_in_MapBiomas_coverLand2$value <- sub("Silvicultura", "silvicultura", sp_in_MapBiomas_coverLand2$value)
      sp_in_MapBiomas_coverLand2$value <- sub("Mineração", "mineração", sp_in_MapBiomas_coverLand2$value)

      sp_in_MapBiomas_coverLand2_n <- 1:nrow(sp_in_MapBiomas_coverLand2)

      if(

        exists("BaixaConversaoUsoDoSoloAOOEOO") == T &
        BaixaConversaoUsoDoSoloAOOEOO == T

      ){

        cat("\n\n")
        cat('<input id="BaixaConversao1" type="checkbox" style="float: left;" value="')
        cat(valueBaixaConversaoAOOEOO1)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueBaixaConversaoAOOEOO1)
        cat("')")
        cat('"><label for="BaixaConversao1" style="display: flex; margin-bottom: -12px;">')
        cat(labelBaixaConversaoAOOEOO1)
        cat('</label><br>\n')
        cat('<input id="BaixaConversaoAOOEOO" type="checkbox" style="float: left;" value="')
        cat(valueBaixaConversaoAOOEOO2)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueBaixaConversaoAOOEOO2)
        cat("', 'BaixaConversaoAOOEOO')")
        cat('"><label for="BaixaConversaoAOOEOO" style="display: flex; margin-bottom: -12px;">')
        cat(labelBaixaConversaoAOOEOO2)
        cat('</label>\n<br>\n')

        for(

          l in sp_in_MapBiomas_coverLand2_n

        ){

          cat('<input id="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" type="checkbox" style="margin-left: 20px; float: left;" value="')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('" onclick="incluirAmeacaNaFrase(\n')
          cat("'")
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat("', '")
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat("')")
          cat('"><label for="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" style="display: flex; margin-bottom: -12px;">')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('</label>\n<br>\n')

        }

      }

      if(

        exists("BaixaConversaoUsoDoSoloAOOEOOAmaz") == T &
        BaixaConversaoUsoDoSoloAOOEOOAmaz == T

      ){

        cat("\n\n")
        cat('<input id="BaixaConversaoAOOEOOAmaz1" type="checkbox" style="float: left;" value="')
        cat(valueBaixaConversaoAOOEOOAmaz1)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueBaixaConversaoAOOEOOAmaz1)
        cat("')")
        cat('"><label for="BaixaConversaoAOOEOOAmaz1" style="display: flex; margin-bottom: -12px;">')
        cat(labelBaixaConversaoAOOEOOAmaz1)
        cat('</label><br>\n')
        cat('<input id="BaixaConversaoAOOEOOAmaz2" type="checkbox" style="float: left;" value="')
        cat(valueBaixaConversaoAOOEOOAmaz2)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueBaixaConversaoAOOEOOAmaz2)
        cat("', 'BaixaConversaoAOOEOOAmaz2')")
        cat('"><label for="BaixaConversaoAOOEOOAmaz2" style="display: flex; margin-bottom: -12px;">')
        cat(labelBaixaConversaoAOOEOOAmaz2)
        cat('</label>\n<br>\n')

        for(

          l in sp_in_MapBiomas_coverLand2_n

        ){

          cat('<input id="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" type="checkbox" style="margin-left: 20px; float: left;" value="')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('" onclick="incluirAmeacaNaFrase(\n')
          cat("'")
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat("', '")
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat("')")
          cat('"><label for="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" style="display: flex; margin-bottom: -12px;">')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('</label>\n<br>\n')

        }

      }

      if(

        exists("ModeradaConversaoUsoDoSoloAOOEOO") == T &
        ModeradaConversaoUsoDoSoloAOOEOO == T

      ){

        cat('<input id="ModeradaConversaoAOOEOO" type="checkbox" style="float: left;" value="')
        cat(valueModeradaConversaoAOOEOO)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueModeradaConversaoAOOEOO)
        cat("', 'ModeradaConversaoAOOEOO')")
        cat('"><label for="ModeradaConversaoAOOEOO" style="display: flex; margin-bottom: -12px;">')
        cat(labelModeradaConversaoAOOEOO)
        cat('</label>\n<br>\n')

        for(

          l in sp_in_MapBiomas_coverLand2_n

        ){

          cat('<input id="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" type="checkbox" style="margin-left: 20px; float: left;" value="')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('" onclick="incluirAmeacaNaFrase(\n')
          cat("'")
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat("', '")
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat("')")
          cat('"><label for="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" style="display: flex; margin-bottom: -12px;">')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('</label>\n<br>\n')

        }

      }

      if(

        exists("ModeradaConversaoUsoDoSoloAOO") == T &
        ModeradaConversaoUsoDoSoloAOO == T

      ){

        cat('<input id="ModeradaConversaoAOO" type="checkbox" style="float: left;" value="')
        cat(valueModeradaConversaoAOO)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueModeradaConversaoAOO)
        cat("', 'ModeradaConversaoAOO')")
        cat('"><label for="ModeradaConversaoAOO" style="display: flex; margin-bottom: -12px;">')
        cat(labelModeradaConversaoAOO)
        cat('</label>\n<br>\n')

        for(

          l in sp_in_MapBiomas_coverLand2_n

        ){

          cat('<input id="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" type="checkbox" style="margin-left: 20px; float: left;" value="')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('" onclick="incluirAmeacaNaFrase(\n')
          cat("'")
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat("', '")
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat("')")
          cat('"><label for="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" style="display: flex; margin-bottom: -12px;">')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('</label>\n<br>\n')

        }

      }

      if(

        exists("AltaConversaoUsoDoSoloAOOEOO") == T &
        AltaConversaoUsoDoSoloAOOEOO == T

      ){

        cat('<input id="AltaConversaoAOOEOO" type="checkbox" style="float: left;" value="')
        cat(valueAltaConversaoAOOEOO)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueAltaConversaoAOOEOO)
        cat("', 'AltaConversaoAOOEOO')")
        cat('"><label for="AltaConversaoAOOEOO" style="display: flex; margin-bottom: -12px;">')
        cat(labelAltaConversaoAOOEOO)
        cat('</label>\n<br>\n')

        for(

          l in sp_in_MapBiomas_coverLand2_n

        ){

          cat('<input id="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" type="checkbox" style="margin-left: 20px; float: left;" value="')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('" onclick="incluirAmeacaNaFrase(\n')
          cat("'")
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat("', '")
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat("')")
          cat('"><label for="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" style="display: flex; margin-bottom: -12px;">')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('</label>\n<br>\n')

        }

      }

      if(

        exists("AltaConversaoUsoDoSoloAOO") == T &
        AltaConversaoUsoDoSoloAOO == T

      ){

        cat('<input id="AltaConversaoAOO" type="checkbox" style="float: left;" value="')
        cat(valueAltaConversaoAOO)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueAltaConversaoAOO)
        cat("', 'AltaConversaoAOO')")
        cat('"><label for="AltaConversaoAOO" style="display: flex; margin-bottom: -12px;">')
        cat(labelAltaConversaoAOO)
        cat('</label>\n<br>\n')

        for(

          l in sp_in_MapBiomas_coverLand2_n

        ){

          cat('<input id="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" type="checkbox" style="margin-left: 20px; float: left;" value="')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('" onclick="incluirAmeacaNaFrase(\n')
          cat("'")
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat("', '")
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat("')")
          cat('"><label for="')
          cat(sp_in_MapBiomas_coverLand2$inputID[l])
          cat('" style="display: flex; margin-bottom: -12px;">')
          cat(sp_in_MapBiomas_coverLand2$value[l])
          cat('</label>\n<br>\n')

        }

      }

    } else {

      if(

        NenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado == T

      ){

        cat("<b>Proporção de conversão para uso alternativo do solo (MapBiomas 2020):</b><br>\n\n")
        cat('<input id="NenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado" type="checkbox" style="float: left;" value="')
        cat(valueNenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueNenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado)
        cat("')")
        cat('"><label for="NenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado" style="display: flex; margin-bottom: -12px;">')
        cat(labelNenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado)
        cat('</label><br>\n')

      }

    }

    ### Definir script em javascript ####
    cat('<script>\n')
    cat("let frase = ['")
    cat(output$Texto)
    cat("'];\n\n")
    cat('const AOOcategoria = "')
    cat(if(AOO_category=="Não ameaçada"){"NA"} else {AOO_category})
    cat('"\n')
    cat('const EOOcategoria = "')
    cat(if(EOO_category=="Não ameaçada"){"NA"} else {EOO_category})
    cat('"\n\n')
    cat("function incluirNaFrase(frag, element){\n")
    cat('  if (element=="')
    if(

      ModeradaConversaoUsoDoSoloAOO == T

    ){

      cat('ModeradaConversaoAOO") {\n')

    }
    if(

      ModeradaConversaoUsoDoSoloAOOEOO == T

    ){

      cat('ModeradaConversaoAOOEOO") {\n')

    }
    if(

      AltaConversaoUsoDoSoloAOO == T

    ){

      cat('AltaConversaoAOO") {\n')

    }
    if(

      AltaConversaoUsoDoSoloAOOEOO == T

    ){

      cat('AltaConversaoAOOEOO") {\n')

    }
    if(

      BaixaConversaoUsoDoSoloAOOEOO == T

    ){

      cat('BaixaConversaoAOOEOO") {\n')

    }
    if(

      BaixaConversaoUsoDoSoloAOOEOOAmaz == T

    ){

      cat('BaixaConversaoAOOEOOAmaz2") {\n')

    }
    cat('    const LOCATIONSinTEXT = document.getElementById("LOCATIONSinTEXT").innerText;\n')
    cat('    const LOCATIONSn = document.getElementById("LOCATIONSn").innerText;\n')
    cat('    const LOCATIONSm = document.getElementById("LOCATIONSm").innerText;\n')
    cat('    const AMEACAS = document.getElementById("AMEACAS").innerText;\n')
    cat('    const LOCATIONSm_threatened = document.getElementById("LOCATIONSm_threatened").innerText;\n')
    cat('    const LOCATIONSm_threatened_n = document.getElementById("LOCATIONSm_threatened_n").innerText;\n')
    cat('    console.log(LOCATIONSinTEXT);\n')
    cat(paste0('    frag = `a ',
               if(BaixaConversaoUsoDoSoloAOOEOO == T){"baixa"},
               if(BaixaConversaoUsoDoSoloAOOEOOAmaz == T){"baixa"},
               if(ModeradaConversaoUsoDoSoloAOO == T){"moderada"},
               if(ModeradaConversaoUsoDoSoloAOOEOO == T){"moderada"},
               if(AltaConversaoUsoDoSoloAOO == T){"alta"},
               if(AltaConversaoUsoDoSoloAOOEOO == T){"alta"},
               if(BaixaConversaoUsoDoSoloAOOEOO == T |
                  BaixaConversaoUsoDoSoloAOOEOOAmaz == T){

                 ' proporção de áreas convertidas para uso alternativo do solo não apenas na AOO ('

               } else {

                 ' proporção de áreas convertidas para uso alternativo do solo na AOO ('

               },
               sp_in_MapBiomas_coverLand$`Área Ocupada da AOO (%)`[sp_in_MapBiomas_coverLand$Ameaça == "Total"],
               '%)',
               if(

                 BaixaConversaoUsoDoSoloAOOEOO == T |
                 BaixaConversaoUsoDoSoloAOOEOOAmaz == T

               ){

                 paste0(' e na EOO (',
                        if(

                          is.na(

                            sp_in_MapBiomas_coverLand %>%
                            dplyr::filter (Ameaça == "Total") %>%
                            dplyr::select(`Área Ocupada da EOO (%)`)

                          ) == F

                        ){
                          sp_in_MapBiomas_coverLand$`Área Ocupada da EOO (%)`[sp_in_MapBiomas_coverLand$Ameaça == "Total"]
                        } else {
                          "não calculado"
                        },
                        '%), mas também à nível regional')

               },
               if(ModeradaConversaoUsoDoSoloAOO == T){},
               if(

                 ModeradaConversaoUsoDoSoloAOOEOO == T

               ){

                 paste0(' e na EOO (',
                        sp_in_MapBiomas_coverLand$`Área Ocupada da EOO (%)`[sp_in_MapBiomas_coverLand$Ameaça == "Total"],
                        '%)')

               },
               if(AltaConversaoUsoDoSoloAOO == T){},
               if(

                 AltaConversaoUsoDoSoloAOOEOO == T

               ){

                 paste0(' e na EOO (',
                        sp_in_MapBiomas_coverLand$`Área Ocupada da EOO (%)`[sp_in_MapBiomas_coverLand$Ameaça == "Total"],
                        '%)')

               },
               ', em 2020, na qual foi possível contabilizar ${LOCATIONSinTEXT} ${LOCATIONSn} de ameaça ${LOCATIONSm_threatened_n} ${LOCATIONSm_threatened} ${LOCATIONSm} pela conversão de áreas para atividades de ${AMEACAS}`;\n'
    )
    )
    cat('  }\n')
    cat("  const indexDoTemNoArray = frase.indexOf(frag);\n")
    cat('  if(indexDoTemNoArray === -1){\n')
    cat('    frase.push(frag);\n')
    cat('  } else {\n')
    cat('    frase.splice(indexDoTemNoArray, 1);\n')
    cat('  }\n\n')
    cat("let novaFrase = '';\n")
    cat('  frase.forEach(function (item,index){\n\n')
    cat('      let indexDoUltimoItem = (frase.length)-1;\n\n')
    cat('      if(frase.length === 2 ){\n')
    cat("      novaFrase = frase.join(' ') + ';';\n\n")
    cat("      }")
    cat("    else if(index === indexDoUltimoItem-1){\n")
    cat("      novaFrase = novaFrase + item + '; e ';\n")
    cat("      }else if (index === indexDoUltimoItem){\n\n")
    cat("        novaFrase = novaFrase + item + ';';\n\n")
    cat("      }else if (index === 0){\n\n")
    cat("        novaFrase = novaFrase + item + ' ';\n\n")
    cat("      }else {\n\n")
    cat("        novaFrase = novaFrase + item + '; ';\n\n")
    cat("      }\n\n")
    cat("    });\n")
    cat('  document.getElementById("frase").innerHTML = novaFrase;\n')
    cat('  document.getElementById("fraseFinal").innerHTML = document.getElementById("frase").innerHTML + document.getElementById("frase2").innerHTML;\n\n')
    cat("  }\n\n")
    cat("let frase2 = '';\n")
    cat("function incluirNaFrase2(){\n")
    cat('  frase2 = " a espécie é categorizada como " + document.getElementById("CATEGORIA").innerHTML + ". " + document.getElementById("CONSIDCONCEADVERS").innerHTML + document.getElementById("CONSIDCONCEADVERSIncertDistEfetiv").innerHTML + document.getElementById("RECOMENDACOES").innerHTML + ".";\n')
    cat('  document.getElementById("frase2").innerHTML = frase2;\n\n')
    cat('  document.getElementById("fraseFinal").innerHTML = document.getElementById("frase").innerHTML + document.getElementById("frase2").innerHTML;\n')
    cat('}\n\n')
    cat("let AMEACAS = [''];\n\n")
    cat("function incluirAmeacaNaFrase(frag){\n")
    cat("  const indexDoTemNoArray = AMEACAS.indexOf(frag);\n")
    cat("  if(indexDoTemNoArray === -1){\n")
    cat("    AMEACAS.push(frag);\n")
    cat("  }else {\n")
    cat("    AMEACAS.splice(indexDoTemNoArray, 1);\n")
    cat("  }\n\n")
    cat("  let novaFrase = '';\n")
    cat("  AMEACAS.forEach(function (item,index){\n\n")
    cat("    let indexDoUltimoItem = (AMEACAS.length)-1;\n\n")
    cat("    if(AMEACAS.length === 2 ){\n")
    cat("      novaFrase = AMEACAS.join(' ');\n")
    cat("    } else if(index === indexDoUltimoItem-1){\n")
    cat("      novaFrase = novaFrase + item + ' e ';\n")
    cat("    } else if (index === indexDoUltimoItem){\n")
    cat("      novaFrase = novaFrase + item;\n")
    cat("    } else if (index === 0){\n")
    cat("      novaFrase = novaFrase + item + ' ';\n")
    cat("    } else {\n")
    cat("      novaFrase = novaFrase + item + ', ';\n")
    cat("    }\n\n")
    cat("  });\n")
    cat('  document.getElementById("AMEACAS").innerHTML = novaFrase;\n\n')
    cat(" }\n\n\n")

    cat('function LocationsCategory() {\n')
    cat('    let x = document.getElementById("locations").value;\n')
    cat('    let z = document.getElementById("locations").value;\n')
    cat('    let locations_threatened = document.getElementById("locations2").value;\n')
    cat('    let text;\n')
    cat('    if (x == 1) {\n')
    cat('      text = "<b style=')
    cat("'")
    cat("color:red; margin-left:10px'>CR</b>")
    cat('";\n')
    cat('    } else if (x > 1 && x < 6) {\n')
    cat('      text = "<b style=')
    cat("'")
    cat("color:red; margin-left:10px'>EN</b>")
    cat('";\n')
    cat('    } else if (x > 5 && x < 11) {\n')
    cat('      text = "<b style=')
    cat("'")
    cat("color:red; margin-left:10px'>VU</b>")
    cat('";\n')
    cat('    } else {\n')
    cat('      text = "<b style=')
    cat("'")
    cat('margin-left:10px')
    cat("'")
    cat('>Não ameaçada</b>";\n')
    cat('    }\n\n')
    cat('    if (z == 1){\n')
    cat('      z = "uma";\n')
    cat('    }\n')
    cat('    if (z == 2){\n')
    cat('    z = "duas";\n')
    cat('    }\n')
    cat('    if (z == 3){\n')
    cat('    z = "três";\n')
    cat('    }\n')
    cat('    if (z == 4){\n')
    cat('    z = "quatro";\n')
    cat('    }\n')
    cat('    if (z == 5){\n')
    cat('    z = "cinco";\n')
    cat('    }\n')
    cat('    if (z == 6){\n')
    cat('    z = "seis";\n')
    cat('    }\n')
    cat('    if (z == 7){\n')
    cat('    z = "sete";\n')
    cat('    }\n')
    cat('    if (z == 8){\n')
    cat('    z = "oito";\n')
    cat('    }\n')
    cat('    if (z == 9){\n')
    cat('    z = "nove";\n')
    cat('    }\n\n')
    cat('    if (x == 1){\n')
    cat('      y = "situação";\n')
    cat('    } else if (x > 1) {\n')
    cat('      y = "situações";\n')
    cat('    }\n\n')
    cat('    if (locations_threatened == 1){\n')
    cat('      w = "é afetada";\n')
    cat('    } else if (locations_threatened > 1) {\n')
    cat('      w = "são afetadas";\n')
    cat('    }\n\n')
    cat('    if (locations_threatened == 1){\n')
    cat('      locations_threatened_number = "uma ";\n')
    cat('    }\n')
    cat('    if (locations_threatened == 2){\n')
    cat('    locations_threatened_number = "duas ";\n')
    cat('    }\n')
    cat('    if (locations_threatened == 3){\n')
    cat('    locations_threatened_number = "três ";\n')
    cat('    }\n')
    cat('    if (locations_threatened == 4){\n')
    cat('    locations_threatened_number = "quatro ";\n')
    cat('    }\n')
    cat('    if (locations_threatened == 5){\n')
    cat('    locations_threatened_number = "cinco ";\n')
    cat('    }\n')
    cat('    if (locations_threatened == 6){\n')
    cat('    locations_threatened_number = "seis ";\n')
    cat('    }\n')
    cat('    if (locations_threatened == 7){\n')
    cat('    locations_threatened_number = "sete ";\n')
    cat('    }\n')
    cat('    if (locations_threatened == 8){\n')
    cat('    locations_threatened_number = "oito ";\n')
    cat('    }\n')
    cat('    if (locations_threatened == 9){\n')
    cat('    locations_threatened_number = "nove ";\n')
    cat('    }\n')
    cat('    if (locations_threatened > 9){\n')
    cat('    locations_threatened_number = locations_threatened + " ";\n')
    cat('    }\n')
    cat('    if (locations_threatened == x){\n')
    cat('    locations_threatened_number = "todas ";\n')
    cat('    }\n\n')
    cat('    if (x == 1){\n')
    cat('      locations_threatened_n = "da qual ";\n')
    cat('    } else if (x > 1) {\n')
    cat('      locations_threatened_n = "das quais ";\n')
    cat('    }\n\n')
    cat('    if (x == 1 && locations_threatened == 1){\n')
    cat('      locations_threatened_n = "";\n')
    cat('      locations_threatened_number = "";\n')
    cat('      w = "afetada";\n')
    cat('    }\n\n')
    cat('    if (x != 1 && locations_threatened != 1 && x == locations_threatened){\n')
    cat('      locations_threatened_n = "";\n')
    cat('      locations_threatened_number = "";\n')
    cat('      w = "afetadas";\n')
    cat('    }\n\n')
    cat('    document.getElementById("LOCATIONS").innerHTML = text;\n')
    cat('    document.getElementById("LOCATIONSinTEXT").innerHTML = z;\n')
    cat('    document.getElementById("LOCATIONSn").innerHTML = y;\n')
    cat('    document.getElementById("LOCATIONSm").innerHTML = w;\n')
    cat('    document.getElementById("LOCATIONSm_threatened").innerHTML = locations_threatened_number;\n')
    cat('    document.getElementById("LOCATIONSm_threatened_n").innerHTML = locations_threatened_n;\n')
    cat('\n\n')
    cat('    if(AOOcategoria == "CR" && text == "<b style=')
    cat("'color:red; margin-left:10px'>CR</b>")
    cat('"){\n')
    cat('      k = "CR";\n')
    cat('    } else if(AOOcategoria == "CR" && text == "<b style=')
    cat("'color:red; margin-left:10px'>EN</b>")
    cat('"){\n')
    cat('      k = "EN";\n')
    cat('    } else if(AOOcategoria == "CR" && text == "<b style=')
    cat("'color:red; margin-left:10px'>VU</b>")
    cat('"){\n')
    cat('      k = "VU";\n')
    cat('    } else if(AOOcategoria == "CR" && text == "<b style=')
    cat("'margin-left:10px'>Não ameaçada</b>")
    cat('"){\n')
    cat('      k = "<a style=')
    cat("'color:black;'> Não ameaçada <a>")
    cat('";\n')
    cat('    } else if(AOOcategoria == "EN" && text == "<b style=')
    cat("'color:red; margin-left:10px'>CR</b>")
    cat('"){\n')
    cat('      k = "EN";\n')
    cat('    } else if(AOOcategoria == "EN" && text == "<b style=')
    cat("'color:red; margin-left:10px'>CR</b>")
    cat('"){\n')
    cat('      k = "EN";\n')
    cat('    } else if(AOOcategoria == "EN" && text == "<b style=')
    cat("'color:red; margin-left:10px'>EN</b>")
    cat('"){\n')
    cat('      k = "EN";\n')
    cat('    } else if(AOOcategoria == "EN" && text == "<b style=')
    cat("'color:red; margin-left:10px'>VU</b>")
    cat('"){\n')
    cat('      k = "VU";\n')
    cat('    } else if(AOOcategoria == "EN" && text == "<b style=')
    cat("'margin-left:10px'>Não ameaçada</b>")
    cat('"){\n')
    cat('      k = "<a style=')
    cat("'color:black;'> Não ameaçada <a>")
    cat('";\n')
    cat('    } else if(AOOcategoria == "VU" && text == "<b style=')
    cat("'color:red; margin-left:10px'>CR</b>")
    cat('"){\n')
    cat('      k = "VU";\n')
    cat('    } else if(AOOcategoria == "VU" && text == "<b style=')
    cat("'color:red; margin-left:10px'>EN</b>")
    cat('"){\n')
    cat('      k = "VU";\n')
    cat('    } else if(AOOcategoria == "VU" && text == "<b style=')
    cat("'color:red; margin-left:10px'>VU</b>")
    cat('"){\n')
    cat('      k = "VU";\n')
    cat('    } else if(AOOcategoria == "VU" && text == "<b style=')
    cat("'margin-left:10px'>Não ameaçada</b>")
    cat('"){\n')
    cat('      k = "<a style=')
    cat("'color:black;'> Não ameaçada <a>")
    cat('";\n')
    cat('    } else if(AOOcategoria == "NA"){\n')
    cat('      k = "<a style=')
    cat("'color:black;'> Não ameaçada <a>")
    cat('";\n    }\n\n')

    cat('    if(EOOcategoria == "CR" && text == "<b style=')
    cat("'color:red; margin-left:10px'>CR</b>")
    cat('"){\n')
    cat('      kk ="CR";\n')
    cat('    } else if(EOOcategoria == "CR" && text == "<b style=')
    cat("'color:red; margin-left:10px'>EN</b>")
    cat('"){\n')
    cat('      kk ="EN";\n')
    cat('    } else if(EOOcategoria == "CR" && text == "<b style=')
    cat("'color:red; margin-left:10px'>VU</b>")
    cat('"){\n')
    cat('      kk ="VU";\n')
    cat('    } else if(EOOcategoria == "CR" && text == "<b style=')
    cat("'margin-left:10px'>Não ameaçada</b>")
    cat('"){\n')
    cat('      kk ="<a style=')
    cat("'color:black;'> Não ameaçada <a>")
    cat('";\n')
    cat('    } else if(EOOcategoria == "EN" && text == "<b style=')
    cat("'color:red; margin-left:10px'>CR</b>")
    cat('"){\n')
    cat('      kk ="EN";\n')
    cat('    } else if(EOOcategoria == "EN" && text == "<b style=')
    cat("'color:red; margin-left:10px'>EN</b>")
    cat('"){\n')
    cat('      kk ="EN";\n')
    cat('    } else if(EOOcategoria == "EN" && text == "<b style=')
    cat("'color:red; margin-left:10px'>VU</b>")
    cat('"){\n')
    cat('      kk ="VU";\n')
    cat('    } else if(EOOcategoria == "EN" && text == "<b style=')
    cat("'margin-left:10px'>Não ameaçada</b>")
    cat('"){\n')
    cat('      kk ="<a style=')
    cat("'color:black;'> Não ameaçada <a>")
    cat('";\n')
    cat('    } else if(EOOcategoria == "VU" && text == "<b style=')
    cat("'color:red; margin-left:10px'>CR</b>")
    cat('"){\n')
    cat('      kk ="VU";\n')
    cat('    } else if(EOOcategoria == "VU" && text == "<b style=')
    cat("'color:red; margin-left:10px'>EN</b>")
    cat('"){\n')
    cat('      kk ="VU";\n')
    cat('    } else if(EOOcategoria == "VU" && text == "<b style=')
    cat("'color:red; margin-left:10px'>VU</b>")
    cat('"){\n')
    cat('      kk ="VU";\n')
    cat('    } else if(EOOcategoria == "VU" && text == "<b style=')
    cat("'margin-left:10px'>Não ameaçada</b>")
    cat('"){\n')
    cat('      kk ="<a style=')
    cat("'color:black;'> Não ameaçada <a>")
    cat('";\n')
    cat('    } else if(EOOcategoria == "NA"){\n')
    cat('      kk ="<a style=')
    cat("'color:black;'> Não ameaçada <a>")
    cat('";\n    }\n\n')
    cat('    document.getElementById("CategoryFinalAOO").innerHTML = k;\n')
    cat('    document.getElementById("CategoryFinalEOO").innerHTML = kk;\n\n')
    cat('let kkk;\n')
    cat('if(k == "CR" || kk == "CR"){\n')
    cat('  kkk = "CR";\n')
    cat('}\n')
    cat('if((k == "EN" || kk == "EN") && (k != "CR" || kk != "CR")){\n')
    cat('  kkk = "EN";\n')
    cat('}\n')
    cat('if((k == "VU" || kk == "VU") && ((k != "CR" || "EN") || (kk != "CR" || "EN"))){\n')
    cat('  kkk = "VU";\n')
    cat('}\n')
    cat('if(k == "')
    cat("<a style='color:black;'")
    cat('> Não ameaçada <a>" && kk == "')
    cat("<a style='color:black;'")
    cat('> Não ameaçada <a>"){\n')
    cat('  kkk = "')
    cat("<a style='color:black;'")
    cat('> Não ameaçada <a>";\n')
    cat('}\n\n')
    cat('switch (kkk){\n')
    cat('  case "CR":\n')
    cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "yellow";\n')
    cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "black";\n')
    cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
    cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
    cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
    cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
    cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
    cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
    cat('    break;\n')
    cat('  case "EN":\n')
    cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
    cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
    cat('    document.getElementById("EmPerigoLabel").style.background = "yellow";\n')
    cat('    document.getElementById("EmPerigoLabel").style.color = "black";\n')
    cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
    cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
    cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
    cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
    cat('    break;\n')
    cat('  case "VU":\n')
    cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
    cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
    cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
    cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
    cat('    document.getElementById("VulneravelLabel").style.background = "yellow";\n')
    cat('    document.getElementById("VulneravelLabel").style.color = "black";\n')
    cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
    cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
    cat('    break;\n')
    cat('  case "')
    cat("<a style='color:black;'")
    cat('> Não ameaçada <a>":\n')
    cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
    cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
    cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
    cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
    cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
    cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
    cat('    document.getElementById("MenosPreocupanteLabel").style.background = "lightgreen";\n')
    cat('    document.getElementById("MenosPreocupanteLabel").style.color = "black";\n')
    cat('}\n')

    cat('  }\n\n')
    cat('</script>\n')

    if(

      NenhumaConversaoUsoDoSoloAOO_EOONaoAnalisado == F

    ){

      cat('<p style="margin-left: 20px;"><b>Número de <i>locations</i>:</b></p>\n\n')
      cat('<input type="number" min="1" max="100" id="locations" style="margin-left: 20px;">\n\n')
      cat('<button type="button" onclick="LocationsCategory()">Ok</button>\n\n')
      cat('<p id="LOCATIONS" style="margin-left: 30px;"></p>\n\n')
      cat('<p style="margin-left: 40px;"><b>Número de <i>locations</i> afetadas por ameaças:</b></p>\n')
      cat('<input type="number" min="1" max="100" id="locations2" style="margin-left: 40px;">\n')
      cat('<button type="button" onclick="LocationsCategory()">Ok</button>\n\n')
      cat('<br><br>\n\n')

      cat("<b>Após consideração do nº de <i>locations</i>:</b><br>\n")
      cat("<b>AOO:</b> <a id=")
      cat('"CategoryFinalAOO"></a>\n')
      cat("<br>\n")
      cat("<b>EOO:</b> <a id=")
      cat('"CategoryFinalEOO"></a>\n')
      cat("<br><br>\n\n")

    }

    ### Declínio de AOO e EOO ####
    cat("<b>Declínio de AOO e EOO (MapBiomas 1985-2020):</b><br>\n\n")

    #### AOO ####
    if(

      trendNatural_total_DOWN == T &
      trendThreat_total_NOTREND == F

    ){

      cat('<input id="TrendDeclinioNaturalAOO" type="checkbox" style="float: left;" value="')
      cat(valueTrendDeclinioNaturalAOO)
      cat('" onclick="Ameacada();incluirNaFrase(\n')
      cat("'")
      cat(valueTrendDeclinioNaturalAOO)
      cat("')")
      cat('"><label for="TrendDeclinioNaturalAOO" style="display: flex; margin-bottom: -12px;">')
      cat(labelTrendDeclinioNaturalAOO)
      cat('</label><br>\n\n')

    }

    if(

      trendNatural_total_UP == T

    ){

      cat('<input id="TrendIncrementoNaturalAOO" type="checkbox" style="float: left;" value="')
      cat(valueTrendIncrementoNaturalAOO)
      cat('" onclick="NaoAmeacada();incluirNaFrase(\n')
      cat("'")
      cat(valueTrendIncrementoNaturalAOO)
      cat("')")
      cat('"><label for="TrendIncrementoNaturalAOO" style="display: flex; margin-bottom: -12px;">')
      cat(labelTrendIncrementoNaturalAOO)
      cat('</label>\n<br>\n\n')

    }

    if(

      trendNatural_total_NOTREND == T

    ){

      cat('<input id="NoTrendAOO" type="checkbox" style="float: left;" value="')
      cat(valueNoTrendAOO)
      cat('" onclick="NaoAmeacada();incluirNaFrase(\n')
      cat("'")
      cat(valueNoTrendAOO)
      cat("')")
      cat('"><label for="NoTrendAOO" style="display: flex; margin-bottom: -12px;">')
      cat(labelNoTrendAOO)
      cat('</label>\n<br>\n\n')

    }

    #### AOO LastTrend ####
    if(

      trendNatural_LastTrend_DOWN == T &
      trendNatural_LastTrend_NOTREND == F

    ){

      cat('<input id="TrendDeclinioNaturalAOOLastTrend" type="checkbox" style="margin-left: 10px; float: left;" value="')
      cat(valueTrendDeclinioNaturalAOOLastTrend)
      cat('" onclick="Ameacada();incluirNaFrase(\n')
      cat("'")
      cat(valueTrendDeclinioNaturalAOOLastTrend)
      cat("')")
      cat('"><label for="TrendDeclinioNaturalAOOLastTrend" style="display: flex; margin-bottom: -12px;">')
      cat(labelTrendDeclinioNaturalAOOLastTrend)
      cat('</label><br>\n\n')

    }

    if(

      trendNatural_LastTrend_UP == T &
      trendNatural_LastTrend_NOTREND == F

    ){

      cat('<input id="TrendIncrementoNaturalAOOLastTrend" type="checkbox" style="margin-left: 10px; float: left;" value="')
      cat(valueTrendIncrementoNaturalAOOLastTrend)
      cat('" onclick="NaoAmeacada();incluirNaFrase(\n')
      cat("'")
      cat(valueTrendIncrementoNaturalAOOLastTrend)
      cat("')")
      cat('"><label for="TrendIncrementoNaturalAOOLastTrend" style="display: flex; margin-bottom: -12px;">')
      cat(labelTrendIncrementoNaturalAOOLastTrend)
      cat('</label>\n<br>\n\n')

    }

    if(

      trendNatural_LastTrend_NOTREND == T

    ){

      cat('<input id="NoTrendAOOLastTrend" type="checkbox" style="margin-left: 10px; float: left;" value="')
      cat(valueNoTrendAOOLastTrend)
      cat('" onclick="incluirNaFrase(\n')
      cat("'")
      cat(valueNoTrendAOOLastTrend)
      cat("')")
      cat('"><label for="NoTrendAOOLastTrend" style="display: flex; margin-bottom: -12px;">')
      cat(labelNoTrendAOOLastTrend)
      cat('</label>\n<br>\n\n')

    }

    cat("<br>\n\n")

    #### AOOinEOObuffer ####
    if(

      trendAOOinEOObufferNatural_total_DOWN == T &
      trendAOOinEOObufferThreat_total_NOTREND == F

    ){

      cat('<input id="TrendDeclinioNaturalAOOinEOObuffer" type="checkbox" style="float: left;" value="')
      cat(valueTrendDeclinioNaturalAOOinEOObuffer)
      cat('" onclick="Ameacada();incluirNaFrase(\n')
      cat("'")
      cat(valueTrendDeclinioNaturalAOOinEOObuffer)
      cat("')")
      cat('"><label for="TrendDeclinioNaturalAOOinEOObuffer" style="display: flex; margin-bottom: -12px;">')
      cat(labelTrendDeclinioNaturalAOOinEOObuffer)
      cat('</label><br>\n\n')

    }

    if(

      trendAOOinEOObufferNatural_total_UP == T

    ){

      cat('<input id="TrendIncrementoNaturalAOOinEOObuffer" type="checkbox" style="float: left;" value="')
      cat(valueTrendIncrementoNaturalAOOinEOObuffer)
      cat('" onclick="NaoAmeacada();incluirNaFrase(\n')
      cat("'")
      cat(valueTrendIncrementoNaturalAOOinEOObuffer)
      cat("')")
      cat('"><label for="TrendIncrementoNaturalAOOinEOObuffer" style="display: flex; margin-bottom: -12px;">')
      cat(labelTrendIncrementoNaturalAOOinEOObuffer)
      cat('</label>\n<br>\n\n')

    }

    if(

      trendAOOinEOObufferNatural_total_NOTREND == T

    ){

      cat('<input id="NoTrendAOOinEOObuffer" type="checkbox" style="float: left;" value="')
      cat(valueNoTrendAOOinEOObuffer)
      cat('" onclick="NaoAmeacada();incluirNaFrase(\n')
      cat("'")
      cat(valueNoTrendAOOinEOObuffer)
      cat("')")
      cat('"><label for="NoTrendAOOinEOObuffer" style="display: flex; margin-bottom: -12px;">')
      cat(labelNoTrendAOOinEOObuffer)
      cat('</label>\n<br>\n\n')

    }

    #### AOOinEOObuffer LastTrend ####
    if(

      trendAOOinEOObufferNatural_LastTrend_DOWN == T &
      trendAOOinEOObufferThreat_LastTrend_NOTREND == F

    ){

      cat('<input id="TrendDeclinioNaturalAOOinEOObufferLastTrend" type="checkbox" style="margin-left: 10px; float: left;" value="')
      cat(valueTrendDeclinioNaturalAOOinEOObufferLastTrend)
      cat('" onclick="Ameacada();incluirNaFrase(\n')
      cat("'")
      cat(valueTrendDeclinioNaturalAOOinEOObufferLastTrend)
      cat("')")
      cat('"><label for="TrendDeclinioNaturalAOOinEOObufferLastTrend" style="display: flex; margin-bottom: -12px;">')
      cat(labelTrendDeclinioNaturalAOOinEOObufferLastTrend)
      cat('</label><br>\n\n')

    }

    if(

      trendAOOinEOObufferNatural_LastTrend_UP == T

    ){

      cat('<input id="TrendIncrementoNaturalAOOinEOObufferLastTrend" type="checkbox" style="margin-left: 10px; float: left;" value="')
      cat(valueTrendIncrementoNaturalAOOinEOObufferLastTrend)
      cat('" onclick="NaoAmeacada();incluirNaFrase(\n')
      cat("'")
      cat(valueTrendIncrementoNaturalAOOinEOObufferLastTrend)
      cat("')")
      cat('"><label for="TrendIncrementoNaturalAOOinEOObufferLastTrend" style="display: flex; margin-bottom: -12px;">')
      cat(labelTrendIncrementoNaturalAOOinEOObufferLastTrend)
      cat('</label>\n<br>\n\n')

    }

    if(

      trendAOOinEOObufferNatural_LastTrend_NOTREND == T

    ){

      cat('<input id="NoTrendAOOinEOObufferLastTrend" type="checkbox" style="margin-left: 10px; float: left;" value="')
      cat(valueNoTrendAOOinEOObufferLastTrend)
      cat('" onclick="incluirNaFrase(\n')
      cat("'")
      cat(valueNoTrendAOOinEOObufferLastTrend)
      cat("')")
      cat('"><label for="NoTrendAOOinEOObufferLastTrend" style="display: flex; margin-bottom: -12px;">')
      cat(labelNoTrendAOOinEOObufferLastTrend)
      cat('</label>\n<br>\n\n')

    }

    cat("<br>\n\n")

    #### EOO ####

    if(

      exists("output_EOO_trendline") == F

    ){

    } else {

      if(

        trendEOONatural_total_DOWN == T &
        trendEOOThreat_total_NOTREND == F

      ){

        cat('<input id="TrendDeclinioNaturalEOO" type="checkbox" style="float: left;" value="')
        cat(valueTrendDeclinioNaturalEOO)
        cat('" onclick="Ameacada();incluirNaFrase(\n')
        cat("'")
        cat(valueTrendDeclinioNaturalEOO)
        cat("')")
        cat('"><label for="TrendDeclinioNaturalEOO" style="display: flex; margin-bottom: -12px;">')
        cat(labelTrendDeclinioNaturalEOO)
        cat('</label><br>\n\n')

      }

      if(

        trendEOONatural_total_UP == T

      ){

        cat('<input id="TrendIncrementoNaturalEOO" type="checkbox" style="float: left;" value="')
        cat(valueTrendIncrementoNaturalEOO)
        cat('" onclick="NaoAmeacada();incluirNaFrase(\n')
        cat("'")
        cat(valueTrendIncrementoNaturalEOO)
        cat("')")
        cat('"><label for="TrendIncrementoNaturalEOO" style="display: flex; margin-bottom: -12px;">')
        cat(labelTrendIncrementoNaturalEOO)
        cat('</label>\n<br>\n\n')

      }

      if(

        trendEOONatural_total_NOTREND == T

      ){

        cat('<input id="NoTrendEOO" type="checkbox" style="float: left;" value="')
        cat(valueNoTrendEOO)
        cat('" onclick="NaoAmeacada();incluirNaFrase(\n')
        cat("'")
        cat(valueNoTrendEOO)
        cat("')")
        cat('"><label for="NoTrendEOO" style="display: flex; margin-bottom: -12px;">')
        cat(labelNoTrendEOO)
        cat('</label>\n<br>\n\n')

      }

      #### EOO LastTrend ####
      if(

        trendEOONatural_LastTrend_DOWN == T &
        trendEOOThreat_LastTrend_NOTREND == F

      ){

        cat('<input id="TrendDeclinioNaturalEOOLastTrend" type="checkbox" style="margin-left: 10px; float: left;" value="')
        cat(valueTrendDeclinioNaturalEOOLastTrend)
        cat('" onclick="Ameacada();incluirNaFrase(\n')
        cat("'")
        cat(valueTrendDeclinioNaturalEOOLastTrend)
        cat("')")
        cat('"><label for="TrendDeclinioNaturalEOOLastTrend" style="display: flex; margin-bottom: -12px;">')
        cat(labelTrendDeclinioNaturalEOOLastTrend)
        cat('</label><br>\n\n')

      }

      if(

        trendEOONatural_LastTrend_UP == T

      ){

        cat('<input id="TrendIncrementoNaturalEOOLastTrend" type="checkbox" style="margin-left: 10px; float: left;" value="')
        cat(valueTrendIncrementoNaturalEOOLastTrend)
        cat('" onclick="NaoAmeacada();incluirNaFrase(\n')
        cat("'")
        cat(valueTrendIncrementoNaturalEOOLastTrend)
        cat("')")
        cat('"><label for="TrendIncrementoNaturalEOOLastTrend" style="display: flex; margin-bottom: -12px;">')
        cat(labelTrendIncrementoNaturalEOOLastTrend)
        cat('</label>\n<br>\n\n')

      }

      if(

        trendEOONatural_LastTrend_NOTREND == T

      ){

        cat('<input id="NoTrendEOOLastTrend" type="checkbox" style="margin-left: 10px; float: left;" value="')
        cat(valueNoTrendEOOLastTrend)
        cat('" onclick="incluirNaFrase(\n')
        cat("'")
        cat(valueNoTrendEOOLastTrend)
        cat("')")
        cat('"><label for="NoTrendEOOLastTrend" style="display: flex; margin-bottom: -12px;">')
        cat(labelNoTrendEOOLastTrend)
        cat('</label>\n<br>\n\n')

      }

    }

    #### Menu dos gráficos: Sobreposição & Tendência ####

    cat('<br>\n\n')

    cat('<div class="container-fluid">\n')
    cat('    <ul class="nav nav-tabs">\n')
    cat('      <li class="active"><a href="#menu1_StackedAreaMapBiomasAOO">AOO: Gráfico</a></li>\n')
    cat('      <li><a data-toggle="tab" href="#menu2_AOOtrends">AOO: Tendências</a></li>\n')
    cat('      <li><a data-toggle="tab" href="#menu3_StackedAreaMapBiomasAOOinEOO">AOO in EOO buffer: Gráfico</a></li>\n')
    cat('      <li><a data-toggle="tab" href="#menu4_AOOinEOOtrends">AOO in EOO buffer: Tendências</a></li>\n')
    cat('      <li><a data-toggle="tab" href="#menu5_StackedAreaMapBiomasEOO">EOO: Gráfico</a></li>\n')
    cat('      <li><a data-toggle="tab" href="#menu6_EOOtrends">EOO: Tendências</a></li>\n')
    cat("    </ul>\n\n")
    cat('    <div class="tab-content">\n')
    cat('      <div id="menu1_StackedAreaMapBiomasAOO" class="tab-pane fade in active">\n')
    cat('<div class="container">\n')
    cat('  <ul class="nav nav-tabs">\n')
    cat('  <li class="active"><a data-toggle="tab" href="#feicoesIsoladas">Feições Isoladas</a></li>\n')
    cat('  <li><a data-toggle="tab" href="#feicoesAglutinadas">Feições Aglutinadas (Natural vs. Uso Alternativo)</a></li>\n')
    cat('  </ul>\n\n')
    cat('    <div class="tab-content">\n')
    cat('  <div id="feicoesIsoladas" class="tab-pane fade in active">\n')
    cat('        <img src="')
    cat(image_uri(paste0("StackedArea-", SPECIES, ".png")))
    cat('" width="1000" height="600"><br>\n')
    cat('  </div>\n')
    cat('  <div id="feicoesAglutinadas" class="tab-pane fade">\n')
    cat('        <img src="')
    cat(image_uri(paste0("StackedArea_NaturalvsThreat-", SPECIES, ".png")))
    cat('" width="1000" height="600"><br>\n')
    cat('    </div>\n')
    cat('  </div>\n')
    cat('  </div>\n')
    cat('      </div>\n')
    cat('      <div id="menu2_AOOtrends" class="tab-pane fade">\n')
    cat('        <div class="container">\n')
    cat('          <ul class="nav nav-tabs">\n')
    cat('          <li class="active"><a href="#trendTable">Tabela de tendências</a></li>\n')
    for(

      trend in 1:output_trendline_n

    ){
      cat('          <li><a data-toggle="tab" href="#')
      cat(paste0("trend_", trend))
      cat('">')
      cat(output_trendline$Feição[trend])
      cat('</a></li>\n')
    }
    cat('          <li><a data-toggle="tab" href="#trendfeicoesaglutinadasNatural">Feições Aglutinadas: Natural</a></li>\n')
    cat('          <li><a data-toggle="tab" href="#trendfeicoesaglutinadasUsoAlternativo">Feições Aglutinadas: Uso Alternativo</a></li>\n')
    cat('        </ul>\n')
    cat('<div class="tab-content">\n')
    cat('  <div id="trendTable" class="tab-pane fade in active">\n')
    cat(kable(output_trendline[,2:5]) %>%
          kable_styling(font_size = 16) %>%
          kable_paper("hover", full_width = F))
    cat('<br>\n')
    cat(kable(output_trendline_NaturalANDThreat) %>%
          kable_styling(font_size = 16) %>%
          kable_paper("hover", full_width = F))
    cat("</div>\n")
    for(

      trend in 1:output_trendline_n

    ){

      cat('<div id="')
      cat(paste0("trend_", trend))
      cat('" class="tab-pane fade">\n')
      cat('<div>\n')
      cat("  <b>Taxa anual:</b> ")
      cat(output_trendline[trend,3])
      cat(" %\n")
      cat("  <br>\n")
      cat("  <b>Início da última tendência:</b> ")
      cat(output_trendline[trend,4])
      cat("\n")
      cat("  <br>\n")
      cat("  <b>Taxa da última tendência:</b> ")
      cat(output_trendline[trend,5])
      cat(" %\n")
      cat("  <br>\n")
      cat('</div>\n')
      cat('        <img src="')
      cat(image_uri(paste0("TrendAOO-", SPECIES, "_", output_trendline$Feição[trend], ".png")))
      cat('" width="1000" height="500"><br>\n')
      cat('        <img src="')
      cat(image_uri(paste0("TrendAOO_detectTrends-", SPECIES, "_", output_trendline$Feição[trend], ".png")))
      cat('" width="1000" height="500"><br>\n')
      cat('        <img src="')
      cat(image_uri(paste0("TrendAOO_LastTrend-", SPECIES, "_", output_trendline$Feição[trend], ".png")))
      cat('" width="1000" height="500"><br>\n')
      cat("</div>\n")

    }
    cat('<div id="trendfeicoesaglutinadasNatural" class="tab-pane fade">\n')
    cat('<div>\n')
    cat("  <b>Taxa anual:</b> ")
    cat(output_trendline_Natural$Taxa_anual)
    cat(" %\n")
    cat("  <br>\n")
    cat("  <b>Início da última tendência:</b> ")
    cat(output_trendline_Natural$Inicio_ultima_tendencia)
    cat("\n")
    cat("  <br>\n")
    cat("  <b>Taxa da última tendência:</b> ")
    cat(output_trendline_Natural$Taxa_ultima_tendencia)
    cat(" %\n")
    cat("  <br>\n")
    cat('</div>\n')
    cat('        <img src="')
    cat(image_uri(paste0("TrendAOO-", SPECIES, "_", "Natural.png")))
    cat('" width="1000" height="500"><br>\n')
    cat('        <img src="')
    cat(image_uri(paste0("TrendAOO_detectTrends-", SPECIES, "_", "Natural.png")))
    cat('" width="1000" height="500"><br>\n')
    cat('        <img src="')
    cat(image_uri(paste0("TrendAOO_LastTrend-", SPECIES, "_", "Natural.png")))
    cat('" width="1000" height="500"><br>\n')
    cat("</div>\n")

    cat('<div id="trendfeicoesaglutinadasUsoAlternativo" class="tab-pane fade">\n')
    cat('<div>\n')
    cat("  <b>Taxa anual:</b> ")
    cat(output_trendline_Threat$Taxa_anual)
    cat(" %\n")
    cat("  <br>\n")
    cat("  <b>Início da última tendência:</b> ")
    cat(output_trendline_Threat$Inicio_ultima_tendencia)
    cat("\n")
    cat("  <br>\n")
    cat("  <b>Taxa da última tendência:</b> ")
    cat(output_trendline_Threat$Taxa_ultima_tendencia)
    cat(" %\n")
    cat("  <br>\n")
    cat('</div>\n')
    cat('        <img src="')
    cat(image_uri(paste0("TrendAOO-", SPECIES, "_", "Threat.png")))
    cat('" width="1000" height="500"><br>\n')
    cat('        <img src="')
    cat(image_uri(paste0("TrendAOO_detectTrends-", SPECIES, "_", "Threat.png")))
    cat('" width="1000" height="500"><br>\n')
    cat('        <img src="')
    cat(image_uri(paste0("TrendAOO_LastTrend-", SPECIES, "_", "Threat.png")))
    cat('" width="1000" height="500"><br>\n')
    cat("</div>\n")

    cat('      </div>\n')
    cat('      </div>\n')
    cat('      </div>\n')

    cat('      <div id="menu3_StackedAreaMapBiomasAOOinEOO" class="tab-pane fade">\n')
    cat('<div class="container">\n')
    cat('  <ul class="nav nav-tabs">\n')
    cat('  <li class="active"><a data-toggle="tab" href="#feicoesIsoladas_AOOinEOObuffer">Feições Isoladas</a></li>\n')
    cat('  <li><a data-toggle="tab" href="#feicoesAglutinadas_AOOinEOObuffer">Feições Aglutinadas (Natural vs. Uso Alternativo)</a></li>\n')
    cat('  </ul>\n\n')
    cat('    <div class="tab-content">\n')
    cat('  <div id="feicoesIsoladas_AOOinEOObuffer" class="tab-pane fade in active">\n')
    cat('        <img src="')
    cat(image_uri(paste0("StackedArea_AOOinEOObuffer-", SPECIES, ".png")))
    cat('" width="1000" height="600"><br>\n')
    cat('  </div>\n')
    cat('  <div id="feicoesAglutinadas_AOOinEOObuffer" class="tab-pane fade">\n')
    cat('        <img src="')
    cat(image_uri(paste0("StackedArea_NaturalvsThreat_AOOinEOObuffer-", SPECIES, ".png")))
    cat('" width="1000" height="600"><br>\n')
    cat('    </div>\n')
    cat('  </div>\n')
    cat('  </div>\n')
    cat('      </div>\n')
    cat('      <div id="menu4_AOOinEOOtrends" class="tab-pane fade">\n')
    cat('        <div class="container">\n')
    cat('          <ul class="nav nav-tabs">\n')
    cat('          <li class="active"><a href="#trendTable_AOOinEOObuffer">Tabela de tendências</a></li>\n')

    if(

      output_AOOinEOObuffer_trendline_n == 0

    ){

    } else {

      for(

        trend_AOOinEOObuffer in 1:output_AOOinEOObuffer_trendline_n

      ){

        cat('          <li><a data-toggle="tab" href="#')
        cat(paste0("trend_AOOinEOObuffer_", trend_AOOinEOObuffer))
        cat('">')
        cat(output_AOOinEOObuffer_trendline$Feição[trend_AOOinEOObuffer])
        cat('</a></li>\n')

      }

    }

    if(output_AOOinEOObuffer_trendline_n > 0){

      for(

        trend_AOOinEOObuffer in 1:output_AOOinEOObuffer_trendline_n

      ){

        cat('          <li><a data-toggle="tab" href="#')
        cat(paste0("trend_AOOinEOObuffer_", trend_AOOinEOObuffer))
        cat('">')
        cat(output_AOOinEOObuffer_trendline$Feição[trend_AOOinEOObuffer])
        cat('</a></li>\n')

      }

    }

    cat('          <li><a data-toggle="tab" href="#trendfeicoesaglutinadasNatural_AOOinEOObuffer">Feições Aglutinadas: Natural</a></li>\n')
    cat('          <li><a data-toggle="tab" href="#trendfeicoesaglutinadasUsoAlternativo_AOOinEOObuffer">Feições Aglutinadas: Uso Alternativo</a></li>\n')
    cat('        </ul>\n')
    cat('<div class="tab-content">\n')
    cat('  <div id="trendTable_AOOinEOObuffer" class="tab-pane fade in active">\n')

    if(output_AOOinEOObuffer_trendline_n == 0){

      cat("<a>Sem reversão de tendência</a>\n")
      cat('<br>\n')

    } else {

      cat(kable(output_AOOinEOObuffer_trendline[,2:5]) %>%
            kable_styling(font_size = 16) %>%
            kable_paper("hover", full_width = F))
      cat('<br>\n')
      cat(kable(output_trendline_NaturalANDThreat_AOOinEOObuffer) %>%
            kable_styling(font_size = 16) %>%
            kable_paper("hover", full_width = F))

    }

    cat("</div>\n")

    if(output_AOOinEOObuffer_trendline_n > 0){

      for(

        trend_AOOinEOObuffer in 1:output_AOOinEOObuffer_trendline_n

      ){

        cat('<div id="')
        cat(paste0("trend_AOOinEOObuffer_", trend_AOOinEOObuffer))
        cat('" class="tab-pane fade">\n')
        cat('<div>\n')
        cat("  <b>Taxa anual:</b> ")
        cat(output_AOOinEOObuffer_trendline[trend_AOOinEOObuffer,3])
        cat(" %\n")
        cat("  <br>\n")
        cat("  <b>Início da última tendência:</b> ")
        cat(output_AOOinEOObuffer_trendline[trend_AOOinEOObuffer,4])
        cat("\n")
        cat("  <br>\n")
        cat("  <b>Taxa da última tendência:</b> ")
        cat(output_AOOinEOObuffer_trendline[trend_AOOinEOObuffer,5])
        cat(" %\n")
        cat("  <br>\n")
        cat('</div>\n')
        cat('        <img src="')
        cat(image_uri(paste0("TrendAOOinEOObuffer-", SPECIES, "_", output_AOOinEOObuffer_trendline$Feição[trend_AOOinEOObuffer], ".png")))
        cat('" width="1000" height="500"><br>\n')
        cat('        <img src="')
        cat(image_uri(paste0("TrendAOOinEOObuffer_detectTrends-", SPECIES, "_", output_AOOinEOObuffer_trendline$Feição[trend_AOOinEOObuffer], ".png")))
        cat('" width="1000" height="500"><br>\n')
        cat('        <img src="')
        cat(image_uri(paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_", output_AOOinEOObuffer_trendline$Feição[trend_AOOinEOObuffer], ".png")))
        cat('" width="1000" height="500"><br>\n')
        cat("</div>\n")

      }

    }


    cat('<div id="trendfeicoesaglutinadasNatural_AOOinEOObuffer" class="tab-pane fade">\n')
    cat('<div>\n')
    cat("  <b>Taxa anual:</b> ")
    cat(output_trendline_Natural_AOOinEOObuffer$Taxa_anual)
    cat(" %\n")
    cat("  <br>\n")
    cat("  <b>Início da última tendência:</b> ")
    cat(output_trendline_Natural_AOOinEOObuffer$Inicio_ultima_tendencia)
    cat("\n")
    cat("  <br>\n")
    cat("  <b>Taxa da última tendência:</b> ")
    cat(output_trendline_Natural_AOOinEOObuffer$Taxa_ultima_tendencia)
    cat(" %\n")
    cat("  <br>\n")
    cat('</div>\n')

    if(output_AOOinEOObuffer_trendline_n > 0){

      cat('        <img src="')
      cat(image_uri(paste0("TrendAOOinEOObuffer-", SPECIES, "_", "Natural.png")))
      cat('" width="1000" height="500"><br>\n')
      cat('        <img src="')
      cat(image_uri(paste0("TrendAOOinEOObuffer_detectTrends-", SPECIES, "_", "Natural.png")))
      cat('" width="1000" height="500"><br>\n')
      cat('        <img src="')
      cat(image_uri(paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_", "Natural.png")))
      cat('" width="1000" height="500"><br>\n')

    } else {

      cat("        <a>Sem Reversão de tendência no AOOinEOObuffer</a>\n")

    }

    cat("</div>\n")
    cat('<div id="trendfeicoesaglutinadasUsoAlternativo_AOOinEOObuffer" class="tab-pane fade">\n')
    cat('<div>\n')
    cat("  <b>Taxa anual:</b> ")
    cat(output_trendline_Threat_AOOinEOObuffer$Taxa_anual)
    cat(" %\n")
    cat("  <br>\n")
    cat("  <b>Início da última tendência:</b> ")
    cat(output_trendline_Threat_AOOinEOObuffer$Inicio_ultima_tendencia)
    cat("\n")
    cat("  <br>\n")
    cat("  <b>Taxa da última tendência:</b> ")
    cat(output_trendline_Threat_AOOinEOObuffer$Taxa_ultima_tendencia)
    cat(" %\n")
    cat("  <br>\n")
    cat('</div>\n')
    cat('        <img src="')
    cat(image_uri(paste0("TrendAOOinEOObuffer-", SPECIES, "_", "Threat.png")))
    cat('" width="1000" height="500"><br>\n')
    cat('        <img src="')
    cat(image_uri(paste0("TrendAOOinEOObuffer_detectTrends-", SPECIES, "_", "Threat.png")))
    cat('" width="1000" height="500"><br>\n')
    cat('        <img src="')
    cat(image_uri(paste0("TrendAOOinEOObuffer_LastTrend-", SPECIES, "_", "Threat.png")))
    cat('" width="1000" height="500"><br>\n')
    cat("</div>\n")

    cat('      </div>\n')
    cat('      </div>\n')
    cat('      </div>\n')

    cat('      <div id="menu5_StackedAreaMapBiomasEOO" class="tab-pane fade">\n')
    if(

      is.na(dataPie$Area_EOO_sites_km2[1]) == T

    ){

      cat("Análise de EOO dispensada\n")

    } else {

      if(

        dataPie$Area_EOO_sites_km2[1] != "Análise dispensada"

      ){

        cat('<div class="container">\n')
        cat('  <ul class="nav nav-tabs">\n')
        cat('  <li class="active"><a data-toggle="tab" href="#feicoesIsoladas_EOO">Feições Isoladas</a></li>\n')
        cat('  <li><a data-toggle="tab" href="#feicoesAglutinadas_EOO">Feições Aglutinadas (Natural vs. Uso Alternativo)</a></li>\n')
        cat('  </ul>\n\n')
        cat('    <div class="tab-content">\n')
        cat('  <div id="feicoesIsoladas_EOO" class="tab-pane fade in active">\n')
        cat('        <img src="')
        cat(image_uri(paste0("StackedArea_EOO-", SPECIES, ".png")))
        cat('" width="1000" height="600"><br>\n')
        cat('  </div>\n')
        cat('  <div id="feicoesAglutinadas_EOO" class="tab-pane fade">\n')
        cat('        <img src="')
        cat(image_uri(paste0("StackedArea_NaturalvsThreat_EOO-", SPECIES, ".png")))
        cat('" width="1000" height="600"><br>\n')
        cat('    </div>\n')
        cat('  </div>\n')
        cat('  </div>\n')

      } else {cat("Análise de EOO dispensada\n")}

    }
    cat('      </div>\n')
    cat('      <div id="menu6_EOOtrends" class="tab-pane fade">\n')
    if(

      is.na(dataPie$Area_EOO_sites_km2[1]) == T

    ){

      cat("Análise dispensada\n")

    } else {

      if(

        dataPie$Area_EOO_sites_km2[1] != "Análise dispensada"

      ){

        cat('        <div class="container">\n')
        cat('          <ul class="nav nav-tabs">\n')
        cat('          <li class="active"><a href="#trendTable_EOO">Tabela de tendências</a></li>\n')
        for(

          trend_EOO in 1:output_EOO_trendline_n

        ){

          cat('          <li><a data-toggle="tab" href="#')
          cat(paste0("trend_EOO_", trend_EOO))
          cat('">')
          cat(output_EOO_trendline$Feição[trend_EOO])
          cat('</a></li>\n')

        }
        cat('          <li><a data-toggle="tab" href="#trend_EOO_feicoesaglutinadasNatural">Feições Aglutinadas: Natural</a></li>\n')
        cat('          <li><a data-toggle="tab" href="#trend_EOO_feicoesaglutinadasUsoAlternativo">Feições Aglutinadas: Uso Alternativo</a></li>\n')
        cat('        </ul>\n')
        cat('<div class="tab-content">\n')
        cat('  <div id="trendTable_EOO" class="tab-pane fade in active">\n')
        cat(kable(output_EOO_trendline[,2:5]) %>%
              kable_styling(font_size = 16) %>%
              kable_paper("hover", full_width = F))
        cat('<br>\n')
        cat(kable(output_EOO_trendline_NaturalANDThreat) %>%
              kable_styling(font_size = 16) %>%
              kable_paper("hover", full_width = F))
        cat("</div>\n")
        for(

          trend_EOO in 1:output_EOO_trendline_n

        ){

          cat('<div id="')
          cat(paste0("trend_EOO_", trend_EOO))
          cat('" class="tab-pane fade">\n')
          cat('<div>\n')
          cat("  <b>Taxa anual:</b> ")
          cat(output_EOO_trendline[trend_EOO,3])
          cat(" %\n")
          cat("  <br>\n")
          cat("  <b>Início da última tendência:</b> ")
          cat(output_EOO_trendline[trend_EOO,4])
          cat("\n")
          cat("  <br>\n")
          cat("  <b>Taxa da última tendência:</b> ")
          cat(output_EOO_trendline[trend_EOO,5])
          cat(" %\n")
          cat("  <br>\n")
          cat('</div>\n')
          cat('        <img src="')
          cat(image_uri(paste0("TrendEOO-", SPECIES, "_", output_EOO_trendline$Feição[trend_EOO], ".png")))
          cat('" width="1000" height="500"><br>\n')
          cat('        <img src="')
          cat(image_uri(paste0("TrendEOO_detectTrends-", SPECIES, "_", output_EOO_trendline$Feição[trend_EOO], ".png")))
          cat('" width="1000" height="500"><br>\n')
          cat('        <img src="')
          cat(image_uri(paste0("TrendEOO_LastTrend-", SPECIES, "_", output_EOO_trendline$Feição[trend_EOO], ".png")))
          cat('" width="1000" height="500"><br>\n')
          cat("</div>\n")

        }
        cat('<div id="trend_EOO_feicoesaglutinadasNatural" class="tab-pane fade">\n')
        cat('<div>\n')
        cat("  <b>Taxa anual:</b> ")
        cat(output_EOO_trendline_Natural$Taxa_anual)
        cat(" %\n")
        cat("  <br>\n")
        cat("  <b>Início da última tendência:</b> ")
        cat(output_EOO_trendline_Natural$Inicio_ultima_tendencia)
        cat("\n")
        cat("  <br>\n")
        cat("  <b>Taxa da última tendência:</b> ")
        cat(output_EOO_trendline_Natural$Taxa_ultima_tendencia)
        cat(" %\n")
        cat("  <br>\n")
        cat('</div>\n')
        cat('        <img src="')
        cat(image_uri(paste0("TrendEOO-", SPECIES, "_", "Natural.png")))
        cat('" width="1000" height="500"><br>\n')
        cat('        <img src="')
        cat(image_uri(paste0("TrendEOO_detectTrends-", SPECIES, "_", "Natural.png")))
        cat('" width="1000" height="500"><br>\n')
        cat('        <img src="')
        cat(image_uri(paste0("TrendEOO_LastTrend-", SPECIES, "_", "Natural.png")))
        cat('" width="1000" height="500"><br>\n')
        cat("</div>\n")

        cat('<div id="trend_EOO_feicoesaglutinadasUsoAlternativo" class="tab-pane fade">\n')
        cat('<div>\n')
        cat("  <b>Taxa anual:</b> ")
        cat(output_EOO_trendline_Threat$Taxa_anual)
        cat(" %\n")
        cat("  <br>\n")
        cat("  <b>Início da última tendência:</b> ")
        cat(output_EOO_trendline_Threat$Inicio_ultima_tendencia)
        cat("\n")
        cat("  <br>\n")
        cat("  <b>Taxa da última tendência:</b> ")
        cat(output_EOO_trendline_Threat$Taxa_ultima_tendencia)
        cat(" %\n")
        cat("  <br>\n")
        cat('</div>\n')
        cat('        <img src="')
        cat(image_uri(paste0("TrendEOO-", SPECIES, "_", "Threat.png")))
        cat('" width="1000" height="500"><br>\n')
        cat('        <img src="')
        cat(image_uri(paste0("TrendEOO_detectTrends-", SPECIES, "_", "Threat.png")))
        cat('" width="1000" height="500"><br>\n')
        cat('        <img src="')
        cat(image_uri(paste0("TrendEOO_LastTrend-", SPECIES, "_", "Threat.png")))
        cat('" width="1000" height="500"><br>\n')
        cat("</div>\n")

        cat('      </div>\n')
        cat('      </div>\n')
        cat('      </div>\n')

      } else {cat("Análise dispensada\n")}

    }
    cat('      </div>\n')
    cat('   </div>\n\n')
    cat('</div>\n')
    cat('</div>\n')
    cat('</div>\n\n')

    cat('<script>\n')
    cat('  $(document).ready(function(){\n')
    cat('    $(".nav-tabs a").click(function(){\n')
    cat("      $(this).tab('show');\n")
    cat("    });\n")
    cat("  });\n")
    cat('</script>\n\n')

    cat("<br><br>\n\n")

    ### Presença em Unidades de Conservação ####
    cat("<b style='margin-left: 15px'>Presença em Unidades de Conservação:</b><br>\n\n")

    cat('<input id="myCheck3" type="checkbox" style="margin-left: 15px; float: left;" value="')
    cat(UCs_text)
    cat('" onclick="incluirNaFrase(\n')
    cat("'")
    cat(UCs_text)
    cat("')")
    cat('"><label for="myCheck3" style="display: flex; margin-bottom: -12px;">')
    cat(UCs_text)
    cat('</label>\n<br><br>\n\n')

    cat("\n<hr>\n")

    ## Categoria final ####
    cat('<h4 style="margin-left: 15px;"><b>Categoria Final:</b></h4>\n')
    ### Radio buttons
    cat('<input id="CriticamenteEmPerigo" type="radio" style="margin-left: 15px;" name="CategoryFinal" class="CategoryFinal" value="Criticamente em Perigo (CR)" onclick="SemConsideracaoConcessiva()"> <label id="CriticamenteEmPerigoLabel" for="CriticamenteEmPerigo"> Criticamente em Perigo (CR)</label>\n')
    cat('<br>\n')
    cat('<input id="EmPerigo" type="radio" style="margin-left: 15px;" name="CategoryFinal" class="CategoryFinal" value="Em Perigo (EN)" onclick="SemConsideracaoConcessiva()"> <label id="EmPerigoLabel" for="EmPerigo"> Em Perigo (EN)</label>\n')
    cat('<br>\n')
    cat('<input id="Vulneravel" type="radio" style="margin-left: 15px;"name="CategoryFinal" class="CategoryFinal" value="Vulnerável (VU)" onclick="SemConsideracaoConcessiva()"> <label id="VulneravelLabel" for="Vulneravel"> Vulnerável (VU)</label>\n')
    cat('<br>\n')
    cat('<input id="MenosPreocupante" type="radio" style="margin-left: 15px;"name="CategoryFinal" class="CategoryFinal" value="Menos Preocupante (LC)" onclick="ConsideracaoConcessiva()"> <label id="MenosPreocupanteLabel" for="MenosPreocupante"> Menos Preocupante (LC)</label>\n')
    cat('<br>\n')
    cat('<input id="QuaseAmeacada" type="radio" style="margin-left: 15px;"name="CategoryFinal" class="CategoryFinal" value="Quase Ameaçada (NT)"> <label for="QuaseAmeacada"> Quase Ameaçada (NT)</label>\n')
    cat('<br>\n')
    cat('<input id="DadosInsuficientes" type="radio" style="margin-left: 15px;"name="CategoryFinal" class="CategoryFinal" value="Dados Insuficientes (DD)"> <label for="DadosInsuficientes"> Dados Insuficientes (DD)</label>\n')
    cat('<br>\n')
    cat('<script type="text/javascript">\n\n')
    cat('  $(".CategoryFinal").change(function(){\n')
    cat('    var CategoryFinalval = $("input[name=')
    cat("'CategoryFinal'")
    cat(']:checked").val();\n')
    cat('    document.getElementById("CATEGORIA").innerHTML = CategoryFinalval;\n')
    cat("    incluirNaFrase2();\n")
    cat("    incluirCriterio();\n")
    cat('  });\n\n')
    cat('</script>\n')
    cat('<div id="CATEGORIA" style="display: none;"></div>\n')
    cat('\n<hr>\n')


    ## Orações subordinadas concessivas ####

    cat('<h4 style="margin-left: 15px;"><b>Considerações concessivas:</b></h4>\n')


    ### AOO/EOO como ameaçada - locations
    if(

      AOOenquadraEOOenquadra == T

    ){

      cat('<input id="valueApesarAOOenquadraEOOenquadra_locations" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueApesarAOOenquadraEOOenquadra_locations)
      cat('"> <label id="valueApesarAOOenquadraEOOenquadra_locationsLabel" for="valueApesarAOOenquadraEOOenquadra_locations" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelApesarAOOenquadraEOOenquadra_locations)
      cat('</label>\n')
      cat('<br>\n')

    }

    if(

      AOOenquadraEOOnaoenquadra == T

    ){

      cat('<input id="valueApesarAOOenquadraEOOnaoenquadra_locations" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueApesarAOOenquadraEOOnaoenquadra_locations)
      cat('"> <label id="valueApesarAOOenquadraEOOnaoenquadra_locationsLabel" for="valueApesarAOOenquadraEOOnaoenquadra_locations" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelApesarAOOenquadraEOOnaoenquadra_locations)
      cat('</label>\n')
      cat('<br>\n')

    }

    if(

      AOOnaoenquadraEOOenquadra == T

    ){

      cat('<input id="valueApesarAOOnaoenquadraEOOenquadra_locations" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueApesarAOOnaoenquadraEOOenquadra_locations)
      cat('"> <label id="valueApesarAOOnaoenquadraEOOenquadra_locationsLabel" for="valueApesarAOOnaoenquadraEOOenquadra_locations" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelApesarAOOnaoenquadraEOOenquadra_locations)
      cat('</label>\n')
      cat('<br>\n')

    }

    ### AOO/EOO como ameaçada - locations
    if(

      AOOenquadraEOOenquadra == T

    ){

      cat('<input id="valueApesarAOOenquadraEOOenquadra_locationsNaoAme" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueApesarAOOenquadraEOOenquadra_locationsNaoAme)
      cat('"> <label id="valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel" for="valueApesarAOOenquadraEOOenquadra_locationsNaoAme" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelApesarAOOenquadraEOOenquadra_locationsNaoAme)
      cat('</label>\n')
      cat('<br>\n')

    }

    if(

      AOOenquadraEOOnaoenquadra == T

    ){

      cat('<input id="valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme)
      cat('"> <label id="valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel" for="valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAme" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelApesarAOOenquadraEOOnaoenquadra_locationsNaoAme)
      cat('</label>\n')
      cat('<br>\n')

    }

    if(

      AOOnaoenquadraEOOenquadra == T

    ){

      cat('<input id="valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme)
      cat('"> <label id="valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel" for="valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAme" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelApesarAOOnaoenquadraEOOenquadra_locationsNaoAme)
      cat('</label>\n')
      cat('<br>\n')

    }

    ### AOO/EOO como ameaçada - declínio contínuo ausente
    if(

      AOOenquadraEOOenquadra == T

    ){

      cat('<input id="valueApesarAOOenquadraEOOenquadra_locations_declinioAusente" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueApesarAOOenquadraEOOenquadra_locations_declinioAusente)
      cat('"> <label id="valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel" for="valueApesarAOOenquadraEOOenquadra_locations_declinioAusente" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelApesarAOOenquadraEOOenquadra_locations_declinioAusente)
      cat('</label>\n')
      cat('<br>\n')

    }

    if(

      AOOenquadraEOOnaoenquadra == T

    ){

      cat('<input id="valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusente" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusente)
      cat('"> <label id="valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel" for="valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusente" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelApesarAOOenquadraEOOnaoenquadra_locations_declinioAusente)
      cat('</label>\n')
      cat('<br>\n')

    }

    if(

      AOOnaoenquadraEOOenquadra == T

    ){

      cat('<input id="valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusente" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusente)
      cat('"> <label id="valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel" for="valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusente" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelApesarAOOnaoenquadraEOOenquadra_locations_declinioAusente)
      cat('</label>\n')
      cat('<br>\n')

    }


    ### AOO/EOO como ameaçada - Área Pouco Estudada - incerteza da distribuição efetiva

    if(

      AOOenquadraEOOenquadra_IncertDistEfetiv == T |
      AOOenquadraEOOnaoenquadra_IncertDistEfetiv == T |
      AOOnaoenquadraEOOenquadra_IncertDistEfetiv == T

    ){

      cat('<h5 style="margin-left: 25px;"><b>Incerteza da distribuição efetiva</b></h5>\n')

      cat('<input id="valueIncertDistEfetiv" type="radio" style="margin-left: 25px; float: left;" name="ConsidConceAdversIncertDistEfetiv" class="ConsidConceAdversIncertDistEfetiv" value="')
      cat(valueIncertDistEfetiv)
      cat('"> <label for="valueIncertDistEfetiv" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelIncertDistEfetiv)
      cat('</label>\n')

      cat("<br>\n")

      cat('<input id="valueAreaPoucoEstudada_incertDistEfetiv" type="radio" style="margin-left: 25px; float: left;" name="ConsidConceAdversIncertDistEfetiv" class="ConsidConceAdversIncertDistEfetiv" value="')
      cat(valueAreaPoucoEstudada_incertDistEfetiv)
      cat('"> <label for="valueAreaPoucoEstudada_incertDistEfetiv" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelAreaPoucoEstudada_incertDistEfetiv)
      cat('</label>\n')

      cat('<br>\n')

    }


    ### CR Quase DD

    if(

      exists("valueCRQuaseDDBemAmostradaAmeacadaAntropiz") == T &
      onlyAmazonia == F

    ){

      cat('<input id="valueCRQuaseDDBemAmostradaAmeacadaAntropiz" type="radio" style="margin-left: 15px; float: left;" name="ConsidConceAdvers" class="ConsidConceAdvers" value="')
      cat(valueCRQuaseDDBemAmostradaAmeacadaAntropiz)
      cat('"> <label for="valueCRQuaseDDBemAmostradaAmeacadaAntropiz" style="display: flex; margin-bottom: -12px;"> ')
      cat(labelCRQuaseDDBemAmostradaAmeacadaAntropiz)
      cat('</label>\n')
      cat('<br>\n')

    }

    cat('<script type="text/javascript">\n\n')
    cat('  $(".ConsidConceAdvers").change(function(){\n')
    cat('    var ConsidConceAdversval = $(')
    cat('"input[name=')
    cat("'ConsidConceAdvers']:checked")
    cat('").val();\n')
    cat('    document.getElementById("CONSIDCONCEADVERS").innerHTML = ConsidConceAdversval;\n')
    cat("    incluirNaFrase2();\n")
    cat('  });\n\n')
    cat('</script>\n')
    cat('  <div id="CONSIDCONCEADVERS" style="display: none;"></div>\n')

    cat('<br>\n')

    cat('<script type="text/javascript">\n\n')
    cat('  $(".ConsidConceAdversIncertDistEfetiv").change(function(){\n')
    cat('    var ConsidConceAdversIncertDistEfetivval = $(')
    cat('"input[name=')
    cat("'ConsidConceAdversIncertDistEfetiv']:checked")
    cat('").val();\n')
    cat('    document.getElementById("CONSIDCONCEADVERSIncertDistEfetiv").innerHTML = ConsidConceAdversIncertDistEfetivval;\n')
    cat("    incluirNaFrase2();\n")
    cat('  });\n\n')
    cat('</script>\n')
    cat('  <div id="CONSIDCONCEADVERSIncertDistEfetiv" style="display: none;"></div>\n')

    cat('\n<hr>\n')

    ## Recomendações ####
    cat('<h4 style="margin-left: 15px;"><b>Recomendações:</b></h4>\n')

    cat('<input id="labelRecomEstudosPopul" type="radio" style="margin-left: 15px; float: left;" name="Recomendacoes" class="Recomendacoes" value="')
    cat("Recomendam-se estudos populacionais e monitoramento da espécie")
    cat('"> <label for="labelRecomEstudosPopul" style="display: flex; margin-bottom: -12px;"> ')
    cat(labelRecomEstudosPopul)
    cat('</label>\n')
    cat('<br>\n')

    cat('<input id="labelRecomNecesMaisColetas" type="radio" style="margin-left: 15px; float: left;" name="Recomendacoes" class="Recomendacoes" value="')
    cat("Recomendam-se trabalhos de campo direcionados para recoletá-la em sua distribuição conhecida e em outras localidades próximas, visando ampliar o conhecimento de sua distribuição, e para coletar dados populacionais")
    cat('"> <label for="labelRecomNecesMaisColetas" style="display: flex; margin-bottom: -12px;"> ')
    cat(labelRecomNecesMaisColetas)
    cat('</label>\n')
    cat('<br>\n')

    cat('<input id="labelRecomNecesMaisColetasNaoRelacUsoSolo" type="radio" style="margin-left: 15px; float: left;" name="Recomendacoes" class="Recomendacoes" value="')
    cat("Recomendam-se trabalhos de campo direcionados para recoletá-la em sua distribuição conhecida e em outras localidades próximas, visando ampliar o conhecimento de sua distribuição, para coletar dados populacionais, e verificar possíveis ameaças diretas não relacionadas ao uso alternativo do solo")
    cat('"> <label for="labelRecomNecesMaisColetasNaoRelacUsoSolo" style="display: flex; margin-bottom: -12px;"> ')
    cat(labelRecomNecesMaisColetasNaoRelacUsoSolo)
    cat('</label>\n')
    cat('<br>\n')

    cat('<input id="labelAposEsforcosCRouEX" type="radio" style="margin-left: 15px; float: left;" name="Recomendacoes" class="Recomendacoes" value="')
    cat("Recomendam-se trabalhos de campo direcionados para recoletá-la em sua distribuição conhecida e em outras localidades próximas, visando ampliar o conhecimento de sua distribuição, e para coletar dados populacionais. Após esforços consistentes direcionados à encontrá-la na natureza, seria possível considerá-la como Criticamente em Perigo (CR) ou Exinta (EX)")
    cat('"> <label for="labelAposEsforcosCRouEX" style="display: flex; margin-bottom: -12px;"> ')
    cat(labelAposEsforcosCRouEX)
    cat('</label>\n')
    cat('<br>\n')

    cat('<script type="text/javascript">\n\n')
    cat('  $(".Recomendacoes").change(function(){\n')
    cat('    var Recomendacoesval = $(')
    cat('"input[name=')
    cat("'Recomendacoes']:checked")
    cat('").val();\n')
    cat('    document.getElementById("RECOMENDACOES").innerHTML = Recomendacoesval;\n')
    cat("    incluirNaFrase2();\n")
    cat('  });\n\n')
    cat('</script>\n\n')
    cat('<div id="RECOMENDACOES" style="display: none;"></div>\n')

    ## Funções Finais #######
    cat('<div id="frase" style="display: none;"></div>\n')
    cat('<div id="frase2" style="display: none;"></div>\n')
    cat('<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>\n')
    cat('<div id="Criterio" style="position: fixed; left: 950; top: 0; right: 0; width: 100%; font-size: 16; color: black;"></div><br><br>\n')
    cat('<div id="fraseFinal" style="position: fixed; left: 0; bottom: 0; right: 0; width: 100%; font-size: 16; background-color: green; color: white;"></div>\n')
    cat('<br><br>\n\n')

    ## Subcritérios e (sub)condições
    cat('<script>\n')
    cat("function incluirCriterio(){\n")
    cat("let criterioNotacao = '';\n")
    cat("let criterioCATEGORIA = '';\n")
    cat("let criterioLocations = '';\n")
    cat("let criterioDeclinio = '';\n")
    cat("let condicaoDeclinioEOO = '';\n")
    cat("let condicaoDeclinioAOO = '';\n\n")
    cat('  if(document.getElementById("CATEGORIA").innerHTML == "Criticamente em Perigo (CR)"){\n')
    cat("    criterioCATEGORIA = 'CR';\n")
    cat('  }\n')
    cat('  if(document.getElementById("CATEGORIA").innerHTML == "Em Perigo (EN)"){\n')
    cat("    criterioCATEGORIA = 'EN';\n")
    cat('  }\n')
    cat('  if(document.getElementById("CATEGORIA").innerHTML == "Vulnerável (VU)"){\n')
    cat("    criterioCATEGORIA = 'VU';\n")
    cat("  }\n\n")
    cat('if(document.getElementById("CATEGORIA").innerHTML == "Menos Preocupante (LC)" ||\n')
    cat('   document.getElementById("CATEGORIA").innerHTML == "Quase Ameaçada (NT)" ||\n')
    cat('   document.getElementById("CATEGORIA").innerHTML == "Dados Insuficientes (DD)"){\n')
    cat("  criterioCATEGORIA = 'NA';\n")
    cat("}\n\n")
    cat('  if(EOOcategoria == "CR" || "EN" || "VU"){\n')
    cat("    criterioB1 = 'B1';\n")
    cat("  }\n")
    cat('  if(AOOcategoria == "CR" || "EN" || "VU"){\n')
    cat("    criterioB2 = 'B2';\n")
    cat("  }\n")
    cat('if(EOOcategoria == "NA"){\n')
    cat("  criterioB1 = '';\n")
    cat("}\n")
    cat('if(AOOcategoria == "NA"){\n')
    cat("  criterioB2 = '';\n")
    cat("}\n\n")
    cat('  if(document.getElementById("')
    if(

      BaixaConversaoUsoDoSoloAOO == T

    ){

      cat("BaixaConversaoAOO")

    }
    if(

      BaixaConversaoUsoDoSoloEOO == T

    ){

      cat("BaixaConversaoEOO")

    }
    if(

      BaixaConversaoUsoDoSoloAOOEOO == T

    ){

      cat("BaixaConversaoAOOEOO")

    }
    if(

      BaixaConversaoUsoDoSoloAOOEOOAmaz == T

    ){

      cat("BaixaConversaoAOOEOOAmaz2")
    }
    if(

      ModeradaConversaoUsoDoSoloAOO == T

    ){
      cat("ModeradaConversaoAOO")

    }
    if(

      ModeradaConversaoUsoDoSoloEOO == T

    ){

      cat("ModeradaConversaoEOO")

    }
    if(

      ModeradaConversaoUsoDoSoloAOOEOO == T

    ){

      cat("ModeradaConversaoAOOEOO")

    }
    if(

      AltaConversaoUsoDoSoloAOO == T

    ){

      cat("AltaConversaoAOO")

    }
    if(

      AltaConversaoUsoDoSoloEOO == T

    ){

      cat("AltaConversaoEOO")

    }
    if(

      AltaConversaoUsoDoSoloAOOEOO == T

    ){

      cat("AltaConversaoAOOEOO")

    }
    cat('").checked == true){\n')
    cat("    criterioLocations = 'a';\n")
    cat("  }\n")
    if(

      trendNatural_total_DOWN == T &
      trendThreat_total_NOTREND == F

    ){

      cat('  if(document.getElementById("TrendDeclinioNaturalAOO").checked == true){\n')
      cat("    criterioDeclinio = 'b';\n")
      cat("    condicaoDeclinioAOO = 'ii';\n")
      cat("  }\n\n")

    }
    if(

      trendNatural_LastTrend_DOWN == T &
      trendNatural_LastTrend_NOTREND == F

    ){

      cat('  if(document.getElementById("TrendDeclinioNaturalAOOLastTrend").checked == true){\n')
      cat("    criterioDeclinio = 'b';\n")
      cat("    condicaoDeclinioAOO = 'ii';\n")
      cat("  }\n\n")

    }
    if(

      trendAOOinEOObufferNatural_total_DOWN == T &
      trendAOOinEOObufferThreat_total_NOTREND == F

    ){

      cat('  if(document.getElementById("TrendDeclinioNaturalAOOinEOObuffer").checked == true){\n')
      cat("    criterioDeclinio = 'b';\n")
      cat("    condicaoDeclinioEOO = 'i';\n")
      cat("  }\n\n")

    }
    if(

      trendAOOinEOObufferNatural_LastTrend_DOWN == T &
      trendAOOinEOObufferThreat_LastTrend_NOTREND == F

    ){

      cat('  if(document.getElementById("TrendDeclinioNaturalAOOinEOObufferLastTrend").checked == true){\n')
      cat("    criterioDeclinio = 'b';\n")
      cat("    condicaoDeclinioEOO = 'i';\n")
      cat("  }\n\n")

    }
    if(

      trendEOONatural_total_DOWN == T &
      trendEOOThreat_total_NOTREND == F

    ){

      cat('  if(document.getElementById("TrendDeclinioNaturalEOO").checked == true){\n')
      cat("    criterioDeclinio = 'b';\n")
      cat("    condicaoDeclinioEOO = 'i';\n")
      cat("  }\n\n")

    }
    if(

      trendAOOinEOObufferNatural_LastTrend_DOWN == T &
      trendAOOinEOObufferThreat_LastTrend_NOTREND == F

    ){

      cat('  if(document.getElementById("TrendDeclinioNaturalEOOLastTrend").checked == true){\n')
      cat("    criterioDeclinio = 'b';\n")
      cat("    condicaoDeclinioEOO = 'i';\n")
      cat("  }\n\n")

    }
    cat("\n")
    cat("if(criterioB1 == 'B1' && criterioB1 == 'B2'){\n")
    cat("  if(criterioCATEGORIA == 'NA'){\n")
    cat("    criterioNotacao = '';\n")
    cat("  } else {\n")
    cat("    criterioNotacao = criterioCATEGORIA + ' ' + criterioB1 + criterioLocations + criterioDeclinio + ' + ' + criterioB2 + criterioLocations + criterioDeclinio;\n")
    cat("  }\n")
    cat("} else {\n")
    cat("  if(criterioB1 == 'B1'){\n")
    cat("    if(criterioCATEGORIA == 'NA'){\n")
    cat("      criterioNotacao = '';\n")
    cat("    } else {\n")
    cat("      criterioNotacao = criterioCATEGORIA + ' ' + criterioB1 + criterioLocations + criterioDeclinio;\n")
    cat("    }\n")
    cat("  } else {\n")
    cat("    if(criterioCATEGORIA == 'NA'){\n")
    cat("      criterioNotacao = '';\n")
    cat("    } else {\n")
    cat("      criterioNotacao = criterioCATEGORIA + ' ' + criterioB2 + criterioLocations + criterioDeclinio;\n")
    cat("    }\n")
    cat("  }\n")
    cat("}\n\n")
    cat("if(condicaoDeclinioAOO == 'ii' && condicaoDeclinioEOO == 'i'){\n")
    cat("  criterioNotacao = criterioNotacao + '(i,ii,iii)';\n")
    cat("  criterioNotacao = criterioNotacao.replace(' +', '(i,ii,iii) +') ;\n")
    cat('}\n\n')
    cat("if(condicaoDeclinioAOO == 'ii' && condicaoDeclinioEOO == ''){\n")
    cat("  criterioNotacao = criterioNotacao + '(ii,iii)';\n")
    cat("  criterioNotacao = criterioNotacao.replace(' +', '(ii,iii) +') ;\n")
    cat('}\n')
    cat("if(condicaoDeclinioAOO == '' && condicaoDeclinioEOO == 'i'){\n")
    cat("  criterioNotacao = criterioNotacao.replace(' +', '(i,iii) +') ;\n")
    cat("  criterioNotacao = criterioNotacao + '(i,iii)';\n")
    cat("}\n\n")
    cat('if(document.getElementById("MenosPreocupante").checked == true ||\n')
    cat('   document.getElementById("QuaseAmeacada").checked == true ||\n')
    cat('   document.getElementById("DadosInsuficientes").checked == true){\n')
    cat("  criterioNotacao = '';\n")
    cat("}\n\n")
    cat('if(document.getElementById("')
    if(

      BaixaConversaoUsoDoSoloAOO == T

    ){

      cat("BaixaConversaoAOO")

    }
    if(

      BaixaConversaoUsoDoSoloEOO == T

    ){

      cat("BaixaConversaoEOO")

    }
    if(

      BaixaConversaoUsoDoSoloAOOEOO == T

    ){

      cat("BaixaConversaoAOOEOO")

    }
    if(

      BaixaConversaoUsoDoSoloAOOEOOAmaz == T

    ){

      cat("BaixaConversaoAOOEOOAmaz2")

    }
    if(

      ModeradaConversaoUsoDoSoloAOO == T

    ){

      cat("ModeradaConversaoAOO")

    }
    if(

      ModeradaConversaoUsoDoSoloEOO == T

    ){

      cat("ModeradaConversaoEOO")

    }
    if(

      ModeradaConversaoUsoDoSoloAOOEOO == T

    ){

      cat("ModeradaConversaoAOOEOO")

    }
    if(

      AltaConversaoUsoDoSoloAOO == T

    ){

      cat("AltaConversaoAOO")

    }
    if(

      AltaConversaoUsoDoSoloEOO == T

    ){

      cat("AltaConversaoEOO")

    }
    if(

      AltaConversaoUsoDoSoloAOOEOO == T

    ){

      cat("AltaConversaoAOOEOO")

    }
    cat('").checked == false){\n')
    cat("  criterioNotacao = '';\n")
    cat("}\n\n")
    cat('  document.getElementById("Criterio").innerHTML = criterioNotacao;\n\n')
    cat("switch (criterioCATEGORIA){\n")
    cat('  case "CR":\n')
    cat('    document.getElementById("Criterio").style.background = "red";\n')
    cat("    break;\n")
    cat('  case "EN":\n')
    cat('    document.getElementById("Criterio").style.background = "orange";\n')
    cat('    break;\n')
    cat('  case "VU":\n')
    cat('    document.getElementById("Criterio").style.background = "yellow";\n')
    cat('    break;\n')
    cat('}\n')
    cat("}\n")

    ### Função Ameacada() #####
    ConversaoUsoDoSolo <- sub("UsoDoSolo", "", colnames(ConversaoUsoDoSolo))
    cat("function Ameacada(){\n")

    if(

      trendNatural_total_DOWN == T &
      trendThreat_total_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOO").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "black";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      trendNatural_LastTrend_DOWN == T &
      trendNatural_LastTrend_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOOLastTrend").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "black";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      trendAOOinEOObufferNatural_total_DOWN == T &
      trendAOOinEOObufferThreat_total_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOOinEOObuffer").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "black";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      trendAOOinEOObufferNatural_LastTrend_DOWN == T &
      trendAOOinEOObufferThreat_LastTrend_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOOinEOObufferLastTrend").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "black";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      exists("output_EOO_trendline") == F

    ){

    } else {

      if(

        trendEOONatural_total_DOWN == T &
        trendEOOThreat_total_NOTREND == F

      ){

        cat('  if(document.getElementById("')
        cat(ConversaoUsoDoSolo)
        cat('").checked == true &&\n')
        cat('     document.getElementById("TrendDeclinioNaturalEOO").checked == true &&\n')
        cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
        cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR"\n')
        cat('      )\n')
        cat('    ){\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "lightgreen";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "black";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat("  }\n")

      }

      if(

        trendEOONatural_LastTrend_DOWN == T &
        trendEOOThreat_LastTrend_NOTREND == F

      ){

        cat('  if(document.getElementById("')
        cat(ConversaoUsoDoSolo)
        cat('").checked == true &&\n')
        cat('     document.getElementById("TrendDeclinioNaturalEOOLastTrend").checked == true &&\n')
        cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
        cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR"\n')
        cat('      )\n')
        cat('    ){\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "lightgreen";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "black";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat("  }\n")

      }

    }

    if(

      trendNatural_total_DOWN == T &
      trendThreat_total_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOO").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "black";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      trendNatural_LastTrend_DOWN == T &
      trendNatural_LastTrend_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOOLastTrend").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "black";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      trendAOOinEOObufferNatural_total_DOWN == T &
      trendAOOinEOObufferThreat_total_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOOinEOObuffer").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "black";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      trendAOOinEOObufferNatural_LastTrend_DOWN == T &
      trendAOOinEOObufferThreat_LastTrend_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOOinEOObufferLastTrend").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "black";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      exists("output_EOO_trendline") == F

    ){

    } else {

      if(

        trendEOONatural_total_DOWN == T &
        trendEOOThreat_total_NOTREND == F

      ){

        cat('  if(document.getElementById("')
        cat(ConversaoUsoDoSolo)
        cat('").checked == true &&\n')
        cat('     document.getElementById("TrendDeclinioNaturalEOO").checked == true &&\n')
        cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
        cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN"\n')
        cat('      )\n')
        cat('    ){\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "lightgreen";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "black";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat("  }\n")

      }

      if(

        trendEOONatural_LastTrend_DOWN == T &
        trendEOOThreat_LastTrend_NOTREND == F

      ){

        cat('  if(document.getElementById("')
        cat(ConversaoUsoDoSolo)
        cat('").checked == true &&\n')
        cat('     document.getElementById("TrendDeclinioNaturalEOOLastTrend").checked == true &&\n')
        cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
        cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN"\n')
        cat('      )\n')
        cat('    ){\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "lightgreen";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "black";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat("  }\n")

      }

    }

    if(

      trendNatural_total_DOWN == T &
      trendThreat_total_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOO").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "black";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      trendNatural_LastTrend_DOWN == T &
      trendNatural_LastTrend_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOOLastTrend").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "black";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      trendAOOinEOObufferNatural_total_DOWN == T &
      trendAOOinEOObufferThreat_total_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOOinEOObuffer").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "black";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      trendAOOinEOObufferNatural_LastTrend_DOWN == T &
      trendAOOinEOObufferThreat_LastTrend_NOTREND == F

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendDeclinioNaturalAOOinEOObufferLastTrend").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU"\n')
      cat('      )\n')
      cat('    ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "black";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
      cat("  }\n")

    }

    if(

      exists("output_EOO_trendline") == F

    ){

    } else {

      if(

        trendEOONatural_total_DOWN == T &
        trendEOOThreat_total_NOTREND == F

      ){

        cat('  if(document.getElementById("')
        cat(ConversaoUsoDoSolo)
        cat('").checked == true &&\n')
        cat('     document.getElementById("TrendDeclinioNaturalEOO").checked == true &&\n')
        cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
        cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU"\n')
        cat('      )\n')
        cat('    ){\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "lightgreen";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "black";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat("  }\n")

      }

      if(

        trendEOONatural_LastTrend_DOWN == T &
        trendEOOThreat_LastTrend_NOTREND == F

      ){

        cat('  if(document.getElementById("')
        cat(ConversaoUsoDoSolo)
        cat('").checked == true &&\n')
        cat('     document.getElementById("TrendDeclinioNaturalEOOLastTrend").checked == true &&\n')
        cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
        cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU"\n')
        cat('      )\n')
        cat('    ){\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "lightgreen";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "black";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat("  }\n")

      }

    }

    TrendDeclinioNatural <- data.frame(TrendDeclinioNaturalAOO = trendNatural_total_DOWN == T & trendThreat_total_NOTREND == F,
                                       TrendDeclinioNaturalAOOLastTrend = trendNatural_LastTrend_DOWN == T & trendNatural_LastTrend_NOTREND == F,
                                       TrendDeclinioNaturalAOOinEOObuffer = trendAOOinEOObufferNatural_total_DOWN == T & trendAOOinEOObufferThreat_total_NOTREND == F,
                                       TrendDeclinioNaturalAOOinEOObufferLastTrend = trendAOOinEOObufferNatural_LastTrend_DOWN == T & trendAOOinEOObufferThreat_LastTrend_NOTREND == F,
                                       TrendDeclinioNaturalEOO = trendEOONatural_total_DOWN == T & trendEOOThreat_total_NOTREND == F,
                                       TrendDeclinioNaturalEOOLastTrend = trendEOONatural_LastTrend_DOWN == T & trendEOONatural_LastTrend_NOTREND == F
    )

    lengthTrendDeclinioNatural <- length(which(TrendDeclinioNatural[1,]==T))

    TrendIncrementoNatural <- data.frame(TrendIncrementoNaturalAOO = trendNatural_total_UP == T,
                                         TrendIncrementoNaturalAOOLastTrend = trendNatural_LastTrend_UP == T,
                                         TrendIncrementoNaturalAOOinEOObuffer = trendAOOinEOObufferNatural_total_UP == T,
                                         TrendIncrementoNaturalAOOinEOObufferLastTrend = trendAOOinEOObufferNatural_LastTrend_UP == T,
                                         TrendIncrementoNaturalEOO = trendEOONatural_total_UP == T,
                                         TrendIncrementoNaturalEOOLastTrend = trendEOONatural_LastTrend_UP == T
    )

    lengthTrendIncrementoNatural <- length(which(TrendIncrementoNatural[1,]==T))

    if(

      lengthTrendIncrementoNatural > 0

    ){

      cat('if(document.getElementById("')

      if(

        BaixaConversaoUsoDoSoloAOO == T

      ){

        cat("BaixaConversaoAOO")

      }

      if(

        BaixaConversaoUsoDoSoloEOO == T

      ){

        cat("BaixaConversaoEOO")

      }
      if(

        BaixaConversaoUsoDoSoloAOOEOO == T

      ){

        cat("BaixaConversaoAOOEOO")

      }

      if(

        BaixaConversaoUsoDoSoloAOOEOOAmaz == T

      ){

        cat("BaixaConversaoAOOEOOAmaz2")

      }

      if(

        ModeradaConversaoUsoDoSoloAOO == T

      ){

        cat("ModeradaConversaoAOO")

      }

      if(

        ModeradaConversaoUsoDoSoloEOO == T

      ){

        cat("ModeradaConversaoEOO")

      }

      if(

        ModeradaConversaoUsoDoSoloAOOEOO == T

      ){

        cat("ModeradaConversaoAOOEOO")

      }

      if(

        ModeradaConversaoUsoDoSoloAOOEOO == T

      ){

        cat("ModeradaConversaoAOOEOO")

      }

      if(

        AltaConversaoUsoDoSoloAOO == T

      ){

        cat("AltaConversaoAOO")

      }

      if(

        AltaConversaoUsoDoSoloEOO == T

      ){

        cat("AltaConversaoEOO")

      }

      if(

        AltaConversaoUsoDoSoloAOOEOO == T

      ){

        cat("AltaConversaoAOOEOO")

      }

      cat('").checked == true &&\n')

      if(

        lengthTrendIncrementoNatural == 1

      ){

        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[1]]))
        cat('").checked == true &&\n')

      }

      if(

        lengthTrendIncrementoNatural == 2

      ){

        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[1]]))
        cat('").checked == true ||\n')
        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[2]]))
        cat('").checked == true &&\n')

      }

      if(

        lengthTrendIncrementoNatural == 3

      ){

        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[1]]))
        cat('").checked == true ||\n')
        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[2]]))
        cat('").checked == true ||\n')
        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[3]]))
        cat('").checked == true &&\n')

      }

      if(

        lengthTrendIncrementoNatural == 4

      ){

        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[1]]))
        cat('").checked == true ||\n')
        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[2]]))
        cat('").checked == true ||\n')
        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[3]]))
        cat('").checked == true ||\n')
        cat('   document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[4]]))
        cat('").checked == true &&\n')

      }

      cat('   (document.getElementById("CategoryFinalAOO").textContent == " Não ameaçada " &&\n')
      cat('    document.getElementById("CategoryFinalEOO").textContent == " Não ameaçada "\n')
      cat('   )\n')
      cat('){\n')
      cat('  document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('  document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('  document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('  document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('  document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('  document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('  document.getElementById("MenosPreocupanteLabel").style.background = "lightgreen";\n')
      cat('  document.getElementById("MenosPreocupanteLabel").style.color = "black";\n')
      cat('}\n\n')
    }

    if(

      lengthTrendDeclinioNatural > 0

    ){

      if(

        lengthTrendDeclinioNatural == 1

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false)\n')
        cat('{\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 2

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false)\n')
        cat('{\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 3

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false)\n')
        cat('{\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 4

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == false)\n')
        cat('  {\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 5

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[5]]))
        cat('").checked == false)\n')
        cat('  {\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 6

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[5]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[6]]))
        cat('").checked == false)\n')
        cat('  {\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('    document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('  }\n')

      }

    }

    cat("}\n\n")


    ### Função NaoAmeacada() ####

    cat('function NaoAmeacada(){\n')

    if(

      trendNatural_total_NOTREND == T ||
      trendNatural_total_UP == T

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendIncrementoNaturalAOO").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").textContent == " Não ameaçada " ||\n')
      cat('      document.getElementById("CategoryFinalEOO").textContent == " Não ameaçada "\n')
      cat('     )\n')
      cat('  ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "black";\n')
      cat('  }\n')

    }

    if(

      trendNatural_LastTrend_UP == T

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendIncrementoNaturalAOOLastTrend").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").textContent == " Não ameaçada " ||\n')
      cat('      document.getElementById("CategoryFinalEOO").textContent == " Não ameaçada "\n')
      cat('     )\n')
      cat('  ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "black";\n')
      cat('  }\n')

    }

    if(

      trendAOOinEOObufferNatural_total_NOTREND == T ||
      trendAOOinEOObufferNatural_total_UP == T

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendIncrementoNaturalAOOinEOObuffer").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").textContent == " Não ameaçada " ||\n')
      cat('      document.getElementById("CategoryFinalEOO").textContent == " Não ameaçada "\n')
      cat('     )\n')
      cat('  ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "black";\n')
      cat('  }\n')

    }

    if(

      trendAOOinEOObufferNatural_LastTrend_UP == T

    ){

      cat('  if(document.getElementById("')
      cat(ConversaoUsoDoSolo)
      cat('").checked == true &&\n')
      cat('     document.getElementById("TrendIncrementoNaturalAOOinEOObufferLastTrend").checked == true &&\n')
      cat('     (document.getElementById("CategoryFinalAOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "CR" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "EN" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalEOO").innerHTML == "VU" ||\n')
      cat('      document.getElementById("CategoryFinalAOO").textContent == " Não ameaçada " ||\n')
      cat('      document.getElementById("CategoryFinalEOO").textContent == " Não ameaçada "\n')
      cat('     )\n')
      cat('  ){\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
      cat('    document.getElementById("EmPerigoLabel").style.color = "gray";\n')
      cat('    document.getElementById("VulneravelLabel").style.background = "transparent";\n')
      cat('    document.getElementById("VulneravelLabel").style.color = "gray";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.background = "lightgreen";\n')
      cat('    document.getElementById("MenosPreocupanteLabel").style.color = "black";\n')
      cat('  }\n')

    }

    TrendIncrementoNatural <- data.frame(TrendIncrementoNaturalAOO = trendNatural_total_UP == T & trendThreat_total_NOTREND == F,
                                         TrendIncrementoNaturalAOOLastTrend = trendNatural_LastTrend_UP == T & trendNatural_LastTrend_NOTREND == F,
                                         TrendIncrementoNaturalAOOinEOObuffer = trendAOOinEOObufferNatural_total_UP == T & trendAOOinEOObufferThreat_total_NOTREND == F,
                                         TrendIncrementoNaturalAOOinEOObufferLastTrend = trendAOOinEOObufferNatural_LastTrend_UP == T & trendAOOinEOObufferThreat_LastTrend_NOTREND == F,
                                         TrendIncrementoNaturalEOO = trendEOONatural_total_UP == T & trendEOOThreat_total_NOTREND == F,
                                         TrendIncrementoNaturalEOOLastTrend = trendEOONatural_LastTrend_UP == T & trendEOONatural_LastTrend_NOTREND == F)

    lengthTrendIncrementoNatural <- length(which(TrendIncrementoNatural[1,]==T))

    if(

      lengthTrendIncrementoNatural > 0

    ){

      if(

        lengthTrendIncrementoNatural == 1

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[1]]))
        cat('").checked == false)\n')
        cat('{\n')
        cat('  document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('  document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('  document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('  document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('  document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('  document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('  document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('  document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('}\n')

      }

      if(

        lengthTrendIncrementoNatural == 2

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[2]]))
        cat('").checked == false)\n')
        cat('{\n')
        cat('  document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('  document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('  document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('  document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('  document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('  document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('  document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('  document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('}\n')

      }

      if(

        lengthTrendIncrementoNatural == 3

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[3]]))
        cat('").checked == false)\n')
        cat('{\n')
        cat('  document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('  document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('  document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('  document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('  document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('  document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('  document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('  document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('}\n')

      }

      if(

        lengthTrendIncrementoNatural == 4

      ){

        cat('  if(document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendIncrementoNatural[which(TrendIncrementoNatural[1,]==T)[4]]))
        cat('").checked == false)\n')
        cat('{\n')
        cat('  document.getElementById("CriticamenteEmPerigoLabel").style.background = "transparent";\n')
        cat('  document.getElementById("CriticamenteEmPerigoLabel").style.color = "gray";\n')
        cat('  document.getElementById("EmPerigoLabel").style.background = "transparent";\n')
        cat('  document.getElementById("EmPerigoLabel").style.color = "gray";\n')
        cat('  document.getElementById("VulneravelLabel").style.background = "transparent";\n')
        cat('  document.getElementById("VulneravelLabel").style.color = "gray";\n')
        cat('  document.getElementById("MenosPreocupanteLabel").style.background = "transparent";\n')
        cat('  document.getElementById("MenosPreocupanteLabel").style.color = "gray";\n')
        cat('}\n')

      }

    }

    cat('}\n\n')


    ### Função ConsideracaoConcessiva() ####

    cat("function ConsideracaoConcessiva(){\n")

    if(

      lengthTrendDeclinioNatural > 0

    ){

      cat('  if(document.getElementById("locations").value > 10 &&\n')

      if(

        lengthTrendDeclinioNatural == 1

      ){

        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == true){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value < 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value > 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 2

      ){

        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == true){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value < 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value > 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 3

      ){

        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == true){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value < 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value > 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 4

      ){

        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == true){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value < 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value > 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 5

      ){

        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[5]]))
        cat('").checked == true){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value < 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[5]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value > 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[5]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        cat('  }\n')

      }

      if(

        lengthTrendDeclinioNatural == 6

      ){

        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[5]]))
        cat('").checked == true ||\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[6]]))
        cat('").checked == true){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value < 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[5]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[6]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locationsNaoAmeLabel").style.color = "black";\n')

        }

        cat('  }\n')
        cat('  if(document.getElementById("locations").value > 10 &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[1]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[2]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[3]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[4]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[5]]))
        cat('").checked == false &&\n')
        cat('     document.getElementById("')
        cat(colnames(TrendDeclinioNatural[which(TrendDeclinioNatural[1,]==T)[6]]))
        cat('").checked == false){\n')

        if(

          AOOenquadraEOOnaoenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOnaoenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOnaoenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        if(

          AOOenquadraEOOenquadra == T

        ){

          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.background = "lightgreen";\n')
          cat('    document.getElementById("valueApesarAOOenquadraEOOenquadra_locations_declinioAusenteLabel").style.color = "black";\n')

        }

        cat('  }\n')

      }

    }

    cat('}\n\n')


    ### Função SemConsideracaoConcessiva() #####

    cat("function SemConsideracaoConcessiva(){\n")
    cat('  if(document.getElementById("CriticamenteEmPerigo").checked == true){\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.background = "transparent";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.color = "gray";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.background = "transparent";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.color = "gray";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.background = "transparent";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.color = "gray";\n')
    cat('  }\n')
    cat('  if(document.getElementById("EmPerigo").checked == true){\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.background = "transparent";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.color = "gray";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.background = "transparent";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.color = "gray";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.background = "transparent";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.color = "gray";\n')
    cat('  }\n')
    cat('  if(document.getElementById("Vulneravel").checked == true){\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.background = "transparent";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsLabel").style.color = "gray";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.background = "transparent";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locationsNaoAmeLabel").style.color = "gray";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.background = "transparent";\n')
    cat('    document.getElementById("valueApesarAOOenquadraEOOnaoenquadra_locations_declinioAusenteLabel").style.color = "gray";\n')
    cat('  }\n')
    cat('}\n\n')
    cat('</script>\n')
    cat('</html>\n')
    sink()

    sink_output <- readtext("output.txt")
    options("encoding" = "UTF-8")
    sink_output$text <- gsub("Ã§", "ç", sink_output$text)
    sink_output$text <- gsub("Ã£", "ã", sink_output$text)
    sink_output$text <- gsub("Ã", "Á", sink_output$text)
    sink_output$text <- gsub("Ã³", "ó", sink_output$text)
    sink_output$text <- gsub("Á³", "ó", sink_output$text)
    sink_output$text <- gsub("Ã³", "á", sink_output$text)
    sink_output$text <- gsub("Á¡", "á", sink_output$text)
    sink_output$text <- gsub("Á©", "é", sink_output$text)
    sink_output$text <- gsub("Á¢", "â", sink_output$text)
    sink_output$text <- gsub("Ã´", "ô", sink_output$text)
    sink_output$text <- gsub("Á´", "ô", sink_output$text)
    sink_output$text <- gsub("Ãr", "Ár", sink_output$text)
    sink_output$text <- gsub("Áº", "ú", sink_output$text)
    sink_output$text <- gsub("Áª", "ê", sink_output$text)

    write(

      sink_output$text,
      paste0(

        "output assessment ",
        i,
        ".html"

      )

    )

  }

}
