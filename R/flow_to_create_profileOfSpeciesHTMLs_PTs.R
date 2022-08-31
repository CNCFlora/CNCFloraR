#' Flow to create the HTMLs of the profile of PT species
#'
#' Fluxo para criar os HTMLs dos perfis de espécies Possivelmente Ameaçadas

flow_to_create_profileOfSpeciesHTMLs_PTs <- function(){


  cat("\nFlow to create the HTMLs of the profile of Possibly Threatened species:\n(press '0' to exit)\n\n")

  switch(

    menu(

      # Screen of console
      c(

        "Get occurrence records from (old) system (build AHK script)",
        "Check records validation and SIG revision",
        "Check geographic coordinates for overlay analysis",
        "Overlay analysis between occurrence records and MapBiomas land cover 1985-2020",
        "Overlay analysis between occurrence records and MapBiomas Fire 2020",
        "Intersection analysis between occurrence records and shapefiles of UCs, PANs, and TERs",
        "Build the profile of species HTMLs"

      )

    ) + 1,

    # Actions
    cat("Nothing done\n"),
    AHKscript_to_download_occurrenceRecords_from_oldSystem(),
    validationOccurrences(),
    validationCoordinates(), # Corrigir conforme sugestão da Gláucia para incluir o controle de fluxo (PA/PNA/validation/SIG)
    menu_overlayAnalysis(),
    menu_overlayAnalysis_Fire(),
    menu_intersect_occurrenceRecords_with_PANs_TERs_UCs(),
    menu_create_profileOfSpeciesHTMLs_PTs()

  )

}
