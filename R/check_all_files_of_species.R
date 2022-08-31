check_all_files_of_species <- function(){

  # Define conditional functions ####

  conditionally <- function(fun){

    function(

      first_arg, ..., execute

    ){

      if(execute) return(fun(first_arg, ...))
      else return(first_arg)

    }

  }

  cond_column_spec <- conditionally(column_spec)


  # Load package ####

  library(data.table)
  library(dplyr)
  library(kableExtra)
  library(stringr)
  library(lubridate)

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


  df <- NULL

  for(species in listOfSpecies$V1){

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

        )

      },

      filledProfile_from_oldSystem = if(file.exists(

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime)
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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

                  )$ctime
              )
            ),
            "%d/%m/%Y"
          ),
          background = "lightgreen",
          color = "black"

        )

      } else {

        text_spec(

          "FALSE",
          background = "red",
          color = "white"

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
