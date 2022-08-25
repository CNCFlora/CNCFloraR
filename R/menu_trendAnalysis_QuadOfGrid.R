#' Menu for trend analysis by QuadOfGrid between occurrence records and MapBiomas Land Cover 1985-2020
#'
#' Menu para condução da análise de sobreposição por QuadOfGrid entre os registros de ocorrências e o MapBiomas Fogo - Cobertura do Solo 1985-2020

menu_trendAnalysis_QuadOfGrid <- function(){


  cat("\nMenu for trend analysis by QuadOfGrid between occurrence records and MapBiomas Land Cover 1985-2020 conduction:\n(press '0' to exit)\n\n")

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
    trendAnalysis_QuadOfGrid_create_scripts(),
    trendAnalysis_QuadOfGrid_execute_scripts(),
    trendAnalysis_QuadOfGrid_check_files()

  )

}
