update_shapefiles_from_cloud  <-  function(){

  library(googledrive)
  library(purrr)
  library(filesstrings)

  # Download from GoogleDrive ####

  ## PANs ####
  PANs_folder <- drive_get(as_id("142Tjkl3pkML6kfiam0et6V4UzHyioMA7"))
  PANs_files <- drive_ls(PANs_folder)

  walk(PANs_files$id, ~ drive_download(as_id(.x), overwrite = T))

  for(file in PANs_files$name){

    file.move(

      paste0(paste0(getwd(), "/", file)),
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/shapefiles/PANs"

        ),
      overwrite = TRUE

    )

  }


  ## TERs ####

  TERs_folder <- drive_get(as_id("1_40YBN1ilGgINB5s_-PKgtOJBumGoAWz"))
  TERs_files <- drive_ls(TERs_folder)

  walk(TERs_files$id, ~ drive_download(as_id(.x), overwrite = T))

  for(file in TERs_files$name){

    file.move(

      paste0(paste0(getwd(), "/", file)),
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/shapefiles/TERs"

        ),
      overwrite = TRUE

    )

  }


  ## UCs ####

  UCs_folder <- drive_get(as_id("14mdwmaNQzfevS2eTAzn9Wxd2sFOjEhnu"))
  UCs_files <- drive_ls(UCs_folder)

  walk(UCs_files$id, ~ drive_download(as_id(.x)))

  for(file in UCs_files$name){

    file.move(

      paste0(paste0(getwd(), "/", file)),
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/shapefiles/UCs"

        ),
      overwrite = TRUE

    )

  }

  message("All done!")

}

