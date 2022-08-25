#' Crop MapBiomas land cover last year (2020) by AOO
#'
#' Cortar o mapa do último ano (2020) do MapBiomas com base no AOO

menu_crop_MapBiomas_by_AOO <- function(){


  cat("\nCrop MapBiomas land cover last year (2020) by AOO:\n(press '0' to exit)\n\n")

  switch(

    menu(

      # Screen of console
      c(

        "Create scripts for each species",
        "Execute scripts of each species",
        "Check metadata of generated script and result files"

      )

    ) + 1,

    # Actions
    cat("Nothing done\n"),
    crop_MapBiomas_by_AOO_create_scripts(),
    crop_MapBiomas_by_AOO_execute_scripts(),
    crop_MapBiomas_by_AOO_check_files()

  )

}
