#' Menu for overlay analysis between occurrence records and MapBiomas Fire 2020
#'
#' Menu para condução da análise de sobreposição entre os registros de ocorrências e o MapBiomas Fogo - Cobertura do Solo 2020

menu_overlayAnalysis_Fire <- function(){


  cat("\nMenu for overlay analysis between occurrence records and MapBiomas Fire Land Cover 2020 conduction:\n(press '0' to exit)\n\n")

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
    overlayAnalysis_Fire_create_scripts(),
    overlayAnalysis_Fire_execute_scripts(),
    overlayAnalysis_Fire_check_files()

  )

}
