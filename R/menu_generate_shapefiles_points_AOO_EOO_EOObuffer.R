#' Menu for generate shapefiles of occurrence points, AOO, EOO, and EOObuffer
#'
#' Menu para geração dos shapefiles dos pontos de ocorrências, AOO, EOO e EOObuffer

menu_generate_shapefiles_points_AOO_EOO_EOObuffer <- function(){


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
