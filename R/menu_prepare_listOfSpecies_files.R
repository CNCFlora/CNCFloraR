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
        "to check all files of species from sheet 'List_for_HTML_profile' of the follow-up table",
        "To get occurrences from old System (by AHK script) based on non-existent files",
        "To build the follow-up table",
        "To check validation of occurrences based on all previous tasks done",
        "To check validation of occurrences based on all previous tasks done & only non-existent overlay analysis",
        "To intersect PANs, TERs, and UCs based all previous tasks done",
        "To intersect PANs, TERs, and UCs based all previous tasks done & only non-existent files",
        "To overlay analysis of MapBiomas (Land Cover 1985-2020) based on all previous tasks done",
        "To overlay analysis of MapBiomas (Land Cover 1985-2020) based on all previous tasks done & only non-existent files",
        "To overlay analysis of MapBiomas (Fire 2020) based on all previous tasks done",
        "To overlay analysis of MapBiomas (Fire 2020) based on all previous tasks done & only non-existent files",
        "To build the profile of species based on all previous tasks done",
        "To build the profile of species based on all previous tasks done & only non-existent files",
        "to get the list of species of sheet 'List_for_assessment' in the follow-up table for build the assessment HTMLs",
        "to get the list of species of sheet 'List_for_assessment' in the follow-up table for build the assessment HTMLs based on only non-existent files",
        "To get the filled profile of species",
        "To get the filled profile of species based on non-existent files",
        "To overlay analysis of MapBiomas (Land Cover 1985-2020) by QuadOfGrid based on all previous tasks done",
        "To overlay analysis of MapBiomas (Land Cover 1985-2020) by QuadOfGrid based on all previous tasks done & only non-existent overlay analysis",
        "To overlay analysis of MapBiomas (Land Cover 1985-2020) by AOOinEOObuffer based on all previous tasks done",
        "To overlay analysis of MapBiomas (Land Cover 1985-2020) by AOOinEOObuffer based on all previous tasks done & only non-existent overlay analysis",
        "To trend analysis of each quad of AOO grid",
        "To trend analysis of each quad of AOO grid based on non-existent files",
        "To generate shapefiles of points, AOO, EOO, and EOObuffer",
        "To generate shapefiles of points, AOO, EOO, and EOObuffer based on non-existent files",
        "To crop MapBiomas - Land Use 2020 - by EOO",
        "To crop MapBiomas - Land Use 2020 - by EOO based on non-existent files",
        "To crop MapBiomas - Land Use 2020 - by AOO",
        "To crop MapBiomas - Land Use 2020 - by AOO based on non-existent files",
        "To build the assessment HTMLs",
        "To build the assessment HTMLs based on non-existent files"

      )

    ) + 1,

    # Actions
    cat("Nothing done."),
    prepare_listOfSpecies_files_for_all_methods(),
    prepare_listOfSpecies_from_followUpTable_sheet_List_for_HTML_profile(),
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
    prepare_listOfSpecies_files_to_build_profileOfSpeciesHTMLs(onlyNonExistentProfile = T),
    prepare_listOfSpecies_from_followUpTable_sheet6(onlyNonExistentAssessment = F),
    prepare_listOfSpecies_from_followUpTable_sheet6(onlyNonExistentAssessment = T),
    prepare_listOfSpecies_files_to_get_filledProfileOfSpecies_from_oldSystem(onlyNonExistentProfile = F),
    prepare_listOfSpecies_files_to_get_filledProfileOfSpecies_from_oldSystem(onlyNonExistentProfile = T),
    prepare_listOfSpecies_files_to_overlayAnalysis_QuadOfGrid(onlyNonExistentAnalysis = F),
    prepare_listOfSpecies_files_to_overlayAnalysis_QuadOfGrid(onlyNonExistentAnalysis = T),
    prepare_listOfSpecies_files_to_overlayAnalysis_AOOinEOObuffer(onlyNonExistentAnalysis = F),
    prepare_listOfSpecies_files_to_overlayAnalysis_AOOinEOObuffer(onlyNonExistentAnalysis = T),
    prepare_listOfSpecies_files_to_trendAnalysis_AOO_QuadOfGrid(onlyNonExistentAnalysis = F),
    prepare_listOfSpecies_files_to_trendAnalysis_AOO_QuadOfGrid(onlyNonExistentAnalysis = T),
    prepare_listOfSpecies_files_to_generate_shapefiles_points_AOO_EOO_EOObuffer(onlyNonExistentFiles = F),
    prepare_listOfSpecies_files_to_generate_shapefiles_points_AOO_EOO_EOObuffer(onlyNonExistentFiles = T),
    prepare_listOfSpecies_files_to_crop_MapBiomas_by_EOO(onlyNonExistentFiles = F),
    prepare_listOfSpecies_files_to_crop_MapBiomas_by_EOO(onlyNonExistentFiles = T),
    prepare_listOfSpecies_files_to_crop_MapBiomas_by_AOO(onlyNonExistentFiles = F),
    prepare_listOfSpecies_files_to_crop_MapBiomas_by_AOO(onlyNonExistentFiles = T),
    prepare_listOfSpecies_files_to_build_assessmentHTMLs(onlyNonExistentAssessment = F),
    prepare_listOfSpecies_files_to_build_assessmentHTMLs(onlyNonExistentAssessment = T),

  )

}
