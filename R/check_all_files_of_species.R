check_all_files_of_species <- function(){

# Load package ####

library(data.table)
library(dplyr)
library(kableExtra)
library(stringr)

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

    occurrenceRecords = file.exists(

      paste0(getwd(), "/CNCFlora_data/inputs/occurrences/oldSystem/", species, ".html")

    ),

    intersectPANs = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/PANs/", species, ".csv")

    ),
    intersectTERs = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/TERs/", species, ".csv")

    ),

    intersectUCs = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/UCs/", species, ".csv")

    ),

    overlayMapBiomasTodosOsAnos = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis results/TodosOsAnos/", species, ".csv")

    ),

    overlayMapBiomasFire = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire results/", species, ".csv")

    ),

    HTMLprofile = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/profileOfSpeciesHTML results/", species, ".html")

    ),

    overlayMapBiomasQuadOfGrid = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis results/QuadOfGrid/", species, ".csv")

    ),

    overlayMapBiomasAOOinEOObuffer = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/overlayAnalysis results/AOOinEOObuffer/", species, ".csv")

    ),

    trendQuadOfGrid = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/trendAnalysis results/QuadOfGrid/", species, ".csv")

    ),

    shapefilePoints = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/points/", species, ".shp")

    ),

    shapefileAOO = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/AOO/", species, ".shp")

    ),

    shapefileAOO_MapBiomas = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/AOO_MapBiomas/", species, ".shp")

    ),

    graphicPie_AOO = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/graphics/pie_AOO/", species, ".png")

    ),

    graphicStackedArea_AOO = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/graphics/stackedArea_AOO/", species, ".png")

    ),

    graphicTrend_AOO = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/graphics/trend_AOO/", species, "_Natural.png")

    ),

    shapefileEOO = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/EOO/", species, ".shp")

    ),

    shapefileEOO_MapBiomas = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/EOO_MapBiomas/", species, ".shp")

    ),

    graphicPie_EOO = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/graphics/pie_EOO/", species, ".png")

    ),

    graphicStackedArea_EOO = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/graphics/stackedArea_EOO/", species, ".png")

    ),

    graphicTrend_EOO = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/graphics/trend_EOO/", species, "_Natural.png")

    ),

    shapefileAOOinEOObuffer = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/shapefiles results/AOOinEOObuffer/", species, ".shp")

    ),

    graphicPie_AOOinEOObuffer = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/graphics/pie_AOOinEOObuffer/", species, ".png")

    ),

    graphicStackedArea_AOOinEOObuffer = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/graphics/stackedArea_AOOinEOObuffer/", species, ".png")

    ),

    graphicTrend_AOOinEOObuffer = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/graphics/AOOinEOObuffer/", species, "_Natural.png")

    ),

    HTMLassessment = file.exists(

      paste0(getwd(), "/CNCFlora_data/outputs/assessmentHTML results/", species, ".html")

    )

  )

  df_ <- as.data.frame(t(df_))
  colnames(df_) <- species

  df <- bind_cols(df, df_)

}

df %>%
  kbl("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = F) %>%
  #column_spec(2:(length(df) + 1), background = ifelse(df[, 1] == T, "lightgreen", "red"))

# Consultar cores em https://htmlcolorcodes.com

}
