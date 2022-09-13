#' Menu for generate shapefiles of occurrence points, AOO, EOO, and EOObuffer
#'
#' Menu para geração dos shapefiles dos pontos de ocorrências, AOO, EOO e EOObuffer

menu_generate_shapefiles_points_AOO_EOO_EOObuffer <- function(){


  cat("\nMenu to generate shapefiles of points, AOO, EOO, and EOObuffer:\n(press '0' to exit)\n\n")

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
    generate_shapefiles_points_AOO_EOO_EOObuffer_create_scripts(),
    generate_shapefiles_points_AOO_EOO_EOObuffer_execute_scripts(),
    generate_shapefiles_points_AOO_EOO_EOObuffer_check_files()

  )

}
