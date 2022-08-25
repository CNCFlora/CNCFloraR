#' Menu to create the species assessment panel HTML
#'
#' Menu para criação dos HTMLs dos painéis de avaliação

menu_create_assessmentHTMLs <- function(){


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
    create_assessmentHTML_create_scripts(),
    create_assessmentHTML_execute_scripts(),
    create_assessmentHTML_check_files()

  )

}
