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
        "Crop MapBiomas land cover last year (2020) by AOO",
        "Crop MapBiomas land cover last year (2020) by EOO",
        "Create AOO and EOO shapefiles",
        "Create the assessment HTMLs"

      )

    ) + 1,

    # Actions
    cat("Nothing done\n"),
    cat("Por enquanto, nada.\n"),
    cat("Por enquanto, nada.\n"),
    cat("Por enquanto, nada.\n"),
    cat("Por enquanto, nada.\n"),
    cat("Por enquanto, nada.\n"),
    cat("Por enquanto, nada.\n"),
    cat("Por enquanto, nada.\n")

  )

}
