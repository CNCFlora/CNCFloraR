#' Flow to build follow-up table
#'
#' Fluxo para carregar dados da tabela de acompanhamento, buscar obras princeps e coletar citação da Flora e Funga do Brasil


flow_to_build_followUpTable <- function(){

  cat("\nFlow to build follow-up table:\n(press '0' to exit)\n\n")

  switch(

    menu(

      # Screen of console
      c(

        "Get data of species from follow-up table in the cloud",
        "Get obras princeps",
        "Fill the follow-up table with the revised obras princeps",
        "Get citation from Flora e Funga do Brasil by scraping",
        "Fill the follow-up table with the revised citations from Flora e Funga do Brasil"

      )

    ) + 1,

    # Actions
    cat("Nothing done\n"),
    get_species_from_followUpTable(),
    get_obraPrinceps_from_Tropicos_IPNI(),
    fill_followUpTable_with_obrasPrinceps(),
    get_citations_from_FloraFungaBrasil(),
    fill_followUpTable_with_citations_from_FloraFungaBrasil()

  )

}
