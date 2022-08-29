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
        "Flow to create the profile of Possibly Not Threatened species HTMLs",
        "Flow to create the profile of Possibly Threatened species HTMLs",
        "Menu to create the profile of species HTMLs",
        "Flow to create the assessment panel",
        "Check all files for creation of profile and assessment panels",
        "Update shapefiles of UCs, PANs, and TERs from cloud",
        "Create AHK scripts"

      )

    ) + 1,

    # Actions
    cat("Nothing done\n"),
    flow_to_build_followUpTable(),
    flow_to_create_profileOfSpeciesHTMLs_PNTs(),
    flow_to_create_profileOfSpeciesHTMLs_PTs(),
    menu_create_profileOfSpeciesHTMLs(),
    flow_to_create_assessmentHTMLs(),
    check_all_files_of_species(),
    update_shapefiles_from_cloud(),
    create_AHKscripts()

  )

}
