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

        "For all methods",
        "to check all files of species from sheet 6 of the follow-up table",
        "To get occurrences from old System (by AHK script)\n   based on the check for non-existent files",
        "To build the follow-up table",
        "To check validation of occurrences\n   based on the check for all previous tasks done",
        "To check validation of occurrences\n   based on the check for all previous tasks done & only non-existent overlay analysis",
        "To intersect PANs, TERs, and UCs\n   based on the check for all previous tasks done",
        "To intersect PANs, TERs, and UCs\n   based on the check for all previous tasks done & only non-existent files",
        "To overlay analysis of MapBiomas - Land Cover 1985-2020\n   based on the check for all previous tasks done",
        "To overlay analysis of MapBiomas - Land Cover 1985-2020\n   based on the check for all previous tasks done & only non-existent files",
        "To overlay analysis of MapBiomas - Fire 2020\n   based on the check for all previous tasks done",
        "To overlay analysis of MapBiomas - Fire 2020\n   based on the check for all previous tasks done & only non-existent files",
        "To build the profile of species\n   based on the check for all previous tasks done",
        "To build the profile of species\n   based on the check for all previous tasks done & only non-existent files"

      )

    ) + 1,

    # Actions
    cat("Nothing done."),
    prepare_listOfSpecies_files_for_all_methods(),
    prepare_listOfSpecies_from_followUpTable_sheet6(),
    prepare_listOfSpecies_files_to_getOccurrences(),
    prepare_listOfSpecies_files_to_build_followUpTable(),
    prepare_listOfSpecies_files_to_validationOccurrences(onlyNonExistentOverlayAnalysis = F),
    prepare_listOfSpecies_files_to_validationOccurrences(onlyNonExistentOverlayAnalysis = T),
    prepare_listOfSpecies_files_to_intersectPANsTERsUCs(onlyNonExistentFile = F),
    prepare_listOfSpecies_files_to_intersectPANsTERsUCs(onlyNonExistentFile = T),
    prepare_listOfSpecies_files_to_overlayAnalysis(onlyNonExistentAnalysis = F),
    prepare_listOfSpecies_files_to_overlayAnalysis(onlyNonExistentAnalysis = T),
    prepare_listOfSpecies_files_to_overlayAnalysis_Fire(onlyNonExistentAnalysis = F),
    prepare_listOfSpecies_files_to_overlayAnalysis_Fire(onlyNonExistentAnalysis = T),
    prepare_listOfSpecies_files_to_build_profileOfSpeciesHTMLs(onlyNonExistentProfile = F),
    prepare_listOfSpecies_files_to_build_profileOfSpeciesHTMLs(onlyNonExistentProfile = T)

  )

}
