#' Menu for overlay analysis by AOOinEOObuffer between occurrence records and MapBiomas Land Cover 1985-2020
#'
#' Menu para condução da análise de sobreposição por AOOinEOObuffer entre os registros de ocorrências e o MapBiomas Fogo - Cobertura do Solo 1985-2020

menu_overlayAnalysis_AOOinEOObuffer <- function(){


  cat("\nMenu for overlay analysis by AOOinEOObuffer between occurrence records and MapBiomas Land Cover 1985-2020 conduction:\n(press '0' to exit)\n\n")

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
    overlayAnalysis_AOOinEOObuffer_create_scripts(),
    overlayAnalysis_AOOinEOObuffer_execute_scripts(),
    overlayAnalysis_AOOinEOObuffer_check_files()

  )

}
