intersect_PANs_TERs_UCs_check_files <- function(){

  # Load package ####

  library(data.table)
  library(colorDF)
  library(dplyr)
  library(crayon)


  # Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <-
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_intersect_UCs_PANs_TERs.csv"

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
          "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs scripts/",
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
            "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs scripts/",
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
          "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/PANs/",
          element,
          ".csv"

        )

      )

    df_results <-
      data.frame(

        Species = element,
        rslt = all(

          c(

            file.exists(

              paste0(

                sub("Packages/CNCFloraR", "", getwd()),
                "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/PANs/",
                element,
                ".csv"

              )

            ),
            file.exists(

              paste0(

                sub("Packages/CNCFloraR", "", getwd()),
                "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/TERs/",
                element,
                ".csv"

              )

            ),
            file.exists(

              paste0(

                sub("Packages/CNCFloraR", "", getwd()),
                "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs results/UCs/",
                element,
                ".csv"

              )

            )

          ) == T

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

      "Intersection between occurrence records and shapefiles of PANs, TERs and UCs\n"

    ))))

  )

  options(colorDF_n = Inf)

  colorDF(
    df,
    theme="dark"
  )

}
