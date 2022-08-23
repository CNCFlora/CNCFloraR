overlayAnalysis_QuadOfGrid_check_files <- function(){

  library(data.table)
  library(colorDF)
  library(dplyr)
  library(crayon)


  # Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <-
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_landCover-MapBiomas.csv"

    )


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
  for(element in listOfSpecies$V1){

    df_scripts <-
      file.info(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/outputs/overlayAnalysis scripts/QuadOfGrid/",
          element,
          " - script.R"

        )

      )

    df_scripts <-
      data.frame(

        Species = element,
        scpt = file.exists(

          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/outputs/overlayAnalysis scripts/QuadOfGrid/",
            element,
            " - script.R"

          )

        ),
        script_ModTim = df_scripts$mtime

      )

    df_results <-
      file.info(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/outputs/overlayAnalysis results/QuadOfGrid/",
          element,
          ".csv"

        )

      )

    df_results <-
      data.frame(

        Species = element,
        rslt = file.exists(

          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/outputs/overlayAnalysis results/QuadOfGrid/",
            element,
            ".csv"

          )

        ),
        result_ModTim = df_results$mtime

      )

    df_ <-
      dplyr::left_join(df_scripts, df_results, by = "Species")

    df <- rbind(df, df_)

  }

  cat("\014")

  cat(

    blue(bgWhite(bold(underline(

      "Overlay Analysis by QuadOfGrid with MapBiomas Land Cover 1985-2020 (Collection 6)\n"

    ))))

  )

  options(colorDF_n = Inf)

  colorDF(
    df,
    theme = "dark"
  )


}
