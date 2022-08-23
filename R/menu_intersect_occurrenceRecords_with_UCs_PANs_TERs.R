#' Menu for intersection analysis conduction
#'
#' Menu para condução da análise de interseção entre os registros de ocorrências e os shapefiles PANs, TERs e UCs

menu_intersect_occurrenceRecords_with_PANs_TERs_UCs <- function(){


  cat("\nMenu for intersection analysis conduction:\n(press '0' to exit)\n\n")

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
    intersect_PANs_TERs_UCs_create_scripts(),
    intersect_PANs_TERs_UCs_execute_scripts(),
    intersect_PANs_TERs_UCs_check_files()

  )

}
