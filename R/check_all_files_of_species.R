check_all_files_of_species <- function(){


  # Load package ####

  library(data.table)
  library(dplyr)
  library(kableExtra)
  library(stringr)
  library(lubridate)
  library(googlesheets4)

  # Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <-
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/check_all_files_of_species.csv"

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

  # Import the list of species file from local path ####

  message("Importing the list of species file...")

  listOfSpecies <- fread(

    listOfSpecies_localPath,
    header = F,
    sep = ";",
    encoding = "UTF-8"

  )

  message("List of species file imported.")


  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")

  followUpTable <- read_sheet(ss, sheet = 1)

  followUpTable <- followUpTable %>% filter(NameFB_semAutor %in% listOfSpecies$V1)

  followUpTable <- followUpTable %>%
    summarise(

      V1 = NameFB_semAutor,
      flow = `PA/PNA`

    )

  listOfSpecies <- left_join(listOfSpecies, followUpTable)

  listOfSpecies$flow[is.na(listOfSpecies$flow) == T] <- "NotDefined"

  df <- NULL

  for(i in 1:nrow(listOfSpecies)){

    species <- listOfSpecies$V1[i]
    flow <- listOfSpecies$flow[i]

    df_ <- data.frame(

      occurrenceRecords = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/inputs/occurrences/oldSystem/", species, ".html")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/inputs/occurrences/oldSystem/", species, ".html")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      intersectPANs = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/PANs/", species, ".csv")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/PANs/", species, ".csv")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      intersectTERs = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/TERs/", species, ".csv")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/TERs/", species, ".csv")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      intersectUCs = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/UCs/", species, ".csv")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/UCs/", species, ".csv")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      overlayMapBiomasTodosOsAnos = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis results/TodosOsAnos/", species, ".csv")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis results/TodosOsAnos/", species, ".csv")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = if(flow == "PA"){

            "red"

          } else {

            if(flow == "PNA"){

              "lightyellow"

            }

          },
          color = if(flow == "PA"){

            "white"

          } else {

            if(flow == "PNA"){

              "lightyellow"

            }

          },
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          }

        )

      },

      overlayMapBiomasFire = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire results/", species, ".csv")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire results/", species, ".csv")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = if(flow == "PA"){

            "red"

          } else {

            if(flow == "PNA"){

              "lightyellow"

            }

          },
          color = if(flow == "PA"){

            "white"

          } else {

            if(flow == "PNA"){

              "lightyellow"

            }

          },
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          }

        )

      },

      HTMLprofile = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/profileOfSpeciesHTML results/", species, ".html")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/profileOfSpeciesHTML results/", species, ".html")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      filledProfile_from_oldSystem = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/inputs/speciesProfile_filled/oldSystem/", species, ".html")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/profileOfSpeciesHTML results/", species, ".html")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      overlayMapBiomasQuadOfGrid = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/inputs/speciesProfile_filled/oldSystem/", species, ".html")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/inputs/speciesProfile_filled/oldSystem/", species, ".html")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      overlayMapBiomasAOOinEOObuffer = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis results/AOOinEOObuffer/", species, ".csv")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis results/AOOinEOObuffer/", species, ".csv")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      trendQuadOfGrid = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/trendAnalysis results/QuadOfGrid/", species, ".csv")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/trendAnalysis results/QuadOfGrid/", species, ".csv")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      shapefilePoints = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/points/", species, ".shp")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/points/", species, ".shp")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      shapefileAOO = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/AOO/", species, ".shp")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/AOO/", species, ".shp")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      shapefileAOO_MapBiomas = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/AOO_MapBiomas/", species, ".shp")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/AOO_MapBiomas/", species, ".shp")

                  )$mtime)
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      graphicPie_AOO = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/graphics/pie_AOO/", species, ".png")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/graphics/pie_AOO/", species, ".png")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      graphicStackedArea_AOO = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/graphics/stackedArea_AOO/", species, ".png")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/graphics/stackedArea_AOO/", species, ".png")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      graphicTrend_AOO = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/graphics/trend_AOO/", species, "_Natural.png")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/graphics/trend_AOO/", species, "_Natural.png")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      shapefileEOO = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/EOO/", species, ".shp")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/EOO/", species, ".shp")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      shapefileEOO_MapBiomas = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/EOO_MapBiomas/", species, ".shp")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/EOO_MapBiomas/", species, ".shp")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      graphicPie_EOO = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/graphics/pie_EOO/", species, ".png")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/graphics/pie_EOO/", species, ".png")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      graphicStackedArea_EOO = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/graphics/stackedArea_EOO/", species, ".png")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/graphics/stackedArea_EOO/", species, ".png")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      graphicTrend_EOO = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/graphics/trend_EOO/", species, "_Natural.png")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/graphics/trend_EOO/", species, "_Natural.png")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      shapefileAOOinEOObuffer = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/AOOinEOObuffer/", species, ".shp")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/AOOinEOObuffer/", species, ".shp")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      graphicPie_AOOinEOObuffer = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/graphics/pie_AOOinEOObuffer/", species, ".png")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/graphics/pie_AOOinEOObuffer/", species, ".png")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      graphicStackedArea_AOOinEOObuffer = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/graphics/stackedArea_AOOinEOObuffer/", species, ".png")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/graphics/stackedArea_AOOinEOObuffer/", species, ".png")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      graphicTrend_AOOinEOObuffer = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/graphics/AOOinEOObuffer/", species, "_Natural.png")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/graphics/AOOinEOObuffer/", species, "_Natural.png")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      },

      HTMLassessment = if(file.exists(

        paste0(getwd(), "/CNCFlora_data/outputs/assessmentHTML results/", species, ".html")

      ) == T){

        text_spec(

          "TRUE",
          tooltip = format(
            ymd(
              sub("\\s.*$",
                  "",
                  file.info(

                    paste0(getwd(), "/CNCFlora_data/outputs/assessmentHTML results/", species, ".html")

                  )$mtime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white",
          extra_css = if(flow == "PA"){

            "border-bottom: 5px solid gray;"

          } else {

            if(flow == "PNA"){

              "border-bottom: 5px solid green;"

            }

          }

        )

      }

    )

    df_ <- as.data.frame(t(df_))
    colnames(df_) <- species

    df <- bind_cols(df, df_)

  }

  df2 <- df %>%

    kbl("html", escape = FALSE, align = "c") %>%
    kable_styling(full_width = F) %>%
    row_spec(1:6, background = "lightyellow") %>%
    row_spec(7, background = "lightgreen") %>%
    row_spec(8:14, background = "lightyellow") %>%
    row_spec(18:19, background = "lightyellow") %>%
    row_spec(23, background = "lightyellow") %>%
    row_spec(27, background = "lightgreen")

  # Consultar cores em https://htmlcolorcodes.com

  print(df2)

  return(invisible(df))

}