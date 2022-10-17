#' Main menu of CNCFlora
#'
#' Menu principal do CNCFlora

menu_CNCFlora <- function(){

  cat("\014")
  cat("Main menu of CNCFlora:\n(press '0' to exit)\n\n")

  switch(

    menu(

      # Screen of console
      c(

        "Flow to build the follow-up table on the local computer by updating from cloud",
        "Check all files for creation of profile and assessment panels",
        "Prepare the list of species files",
        "Get occurrence records from (old) system (build AHK script)",
        "Flow to create the profile of Possibly Not Threatened species HTMLs",
        "Flow to create the profile of Possibly Threatened species HTMLs",
        "Menu to create the profile of species HTMLs",
        "Give access of profile of species HTMLs to everyone with the link",
        "Fill the follow-up table in cloud with filling of the profile of species completed",
        "Flow to create the assessment panel",
        "Update shapefiles of UCs, PANs, and TERs from cloud",
        "Create AHK scripts"

      )

    ) + 1,

    # Actions
    cat("Nothing done\n"),
    flow_to_build_followUpTable(),
    check_all_files_of_species(),
    menu_prepare_listOfSpecies_files(),
    AHKscript_to_download_occurrenceRecords_from_oldSystem(),
    flow_to_create_profileOfSpeciesHTMLs_PNTs(),
    flow_to_create_profileOfSpeciesHTMLs_PTs(),
    menu_create_profileOfSpeciesHTMLs(),
    give_access_of_profileOfSpeciesHTMLs_to_anyone_with_the_link(),
    fill_followUpTable_with_profileOfSpecies_completed_filling(),
    flow_to_create_assessmentHTMLs(),
    update_shapefiles_from_cloud(),
    menu_create_AHKscripts()

  )

}
