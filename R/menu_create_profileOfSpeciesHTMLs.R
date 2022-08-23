#' Menu to create the species profile HTML
#'
#' Menu para criação dos HTMLs dos perfis das espécies

menu_create_profileOfSpeciesHTMLs <- function(){


  cat("\nMenu to create the species profile HTML:\n(press '0' to exit)\n\n")

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
    create_profileOfSpeciesHTML_create_scripts(),
    create_profileOfSpeciesHTML_execute_scripts(),
    create_profileOfSpeciesHTML_check_files()

  )

}
