#' Menu for overlay analysis between occurrence records and MapBiomas
#'
#' Menu para condução da análise de sobreposição entre os registros de ocorrências e o MapBiomas Cobertura do Solo 1985-2020

menu_overlayAnalysis <- function(){


  cat("\nMenu for overlay analysis between occurrence records and MapBiomas Land Cover 1985-2020 conduction:\n(press '0' to exit)\n\n")

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
    overlayAnalysis_create_scripts(),
    overlayAnalysis_execute_scripts(),
    overlayAnalysis_check_files()

  )

}
