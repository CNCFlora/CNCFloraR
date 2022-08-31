#' Menu to prepare the list of species files for processing
#'
#' Menu para preparação dos arquivos de listas de espécies para processamento

menu_prepare_listOfSpecies_files <- function(){

  cat("\014")
  cat("Menu to prepare the list of species files:\n(press '0' to exit)\n\n")

  switch(

    menu(

      # Screen of console
      c(

        "Prepare the list of species file for all methods",
        "Prepare the list of species file to get occurrences from old System (by AHK script)\n  based on the check for non-existent files"

      )

    ) + 1,

    # Actions
    cat("Nothing done."),
    prepare_listOfSpecies_files_for_all_methods(),
    prepare_listOfSpecies_files_to_getOccurrences()

  )

}
