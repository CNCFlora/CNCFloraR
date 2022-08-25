#' Flow to create assessment HTMLs
#'
#' Fluxo para criar painéis de avaliação em HTML

flow_to_create_assessmentHTMLs <- function(){

  cat("\nFlow to create assessment HTMLs:\n(press '0' to exit)\n\n")

  switch(

    menu(

      # Screen of console
      c(

        "Overlay analysis between occurrence records and MapBiomas cover land (1985-2020) by AOOQuadOfGrid",
        "Overlay analysis between occurrence records and MapBiomas cover land (1985-2020) by AOOinEOObuffer",
        "Trend analysis of QuadOfGrid (MapBiomas cover land 1985-2020)",
        "Generate AOO and EOO shapefiles",
        "Crop MapBiomas land cover last year (2020) by AOO",
        "Crop MapBiomas land cover last year (2020) by EOO",
        "Get the filled species profile from old system (by AHK script)",
        "Create the assessment HTML"

      )

    ) + 1,

    # Actions
    cat("Nothing done\n"),
    menu_overlayAnalysis_QuadOfGrid(),
    menu_overlayAnalysis_AOOinEOObuffer(),
    menu_trendAnalysis_QuadOfGrid(),
    menu_generate_shapefiles_points_AOO_EOO_EOObuffer(),
    menu_crop_MapBiomas_by_AOO(),
    menu_crop_MapBiomas_by_EOO(),
    cat("Not working... Working on it... Get filled species profile"),
    menu_create_assessmentHTMLs()

  )

}
