#' create AHK scripts
#'
#' Criar scripts AHK

menu_create_AHKscripts <- function(){

  cat("\nCreate AHK scripts:\n(press '0' to exit)\n\n")

  switch(

    menu(

      # Screen of console
      c(

        "Get occurrence records from old system",
        "Get the profile IDs of the species in old system",
        "Store the profile IDs of the species in old system",
        "Read basic informations from the profile of species HTML",
        "Read Actions from the profile of species HTML",
        "Read Threats from the profile of species HTML",
        "Fill basic informations within old system",
        "Fill Actions within old system",
        "Fill Threats within old system",
        "Get the filled profile of species from old system"

      )

    ) + 1,

    # Actions
    "Nothing done.",
    AHKscript_to_download_occurrenceRecords_from_oldSystem(),
    get_profileIDs_from_oldSystem(),
    store_profileIDs(),
    read_infoBasic_from_profileOfSpeciesHTML(),
    read_Actions_from_profileOfSpeciesHTML(),
    read_Threats_from_profileOfSpeciesHTML(),
    fill_infoBasic(),
    fill_threats(),
    fill_actions(),
    cat("Por enquanto, nada.\n")

  )

}
