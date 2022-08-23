check_if_all_species_are_in_OverlayAnalysesOutputs <- function(species, OverlayAnalyses_ID){

  `%notin%` <- Negate(`%in%`)

  # Check if all species are in SIG_land_use_TodosOsAnos_output

  SIG_land_use_TodosOsAnos_output <-
    fread(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "SIG_land_use_TodosOsAnos_output(", OverlayAnalyses_ID, ").csv"

      ),
      header = T
      #, encoding = "UTF-8"

    )

  message("  Checking if all species are found in SIG_land_use_TodosOsAnos_output...")

  if(

    length(species[species %notin% unique(SIG_land_use_TodosOsAnos_output$Species)]) != 0

  ){

    check_if_all_species_are_in_TodosOsAnos_output <- FALSE

    message("    Species not found:")

    for(

      sp in species[species %notin% unique(SIG_land_use_TodosOsAnos_output$Species)]

      ){

      message(paste0("      ", sp))

    }

  } else {

    message("    All species were found in SIG_land_use_TodosOsAnos_output.")

    check_if_all_species_are_in_TodosOsAnos_output <- TRUE

  }


  # Check if SIG_land_use_TodosOsAnos_AOOinEOObuffer_output exists

  message("  Checking if all species are found in SIG_land_use_TodosOsAnos_AOOinEOObuffer_output...")

  SIG_land_use_TodosOsAnos_AOOinEOObuffer_output <-
    fread(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "SIG_land_use_TodosOsAnos_AOOinEOObuffer_output(", OverlayAnalyses_ID, ").csv"

      ),
      header = T
      #, encoding = "UTF-8"

    )

  if(

    length(species[species %notin% unique(SIG_land_use_TodosOsAnos_AOOinEOObuffer_output$Species)]) != 0

  ){

    check_if_all_species_are_in_TodosOsAnos_AOOinEOObuffer_output <- FALSE

    message("    Species not found:")

    for(

      sp in species[species %notin% unique(SIG_land_use_TodosOsAnos_AOOinEOObuffer_output$Species)]

    ){

      message(paste0("      ", sp))

    }

  } else {

    message("    All species were found in SIG_land_use_TodosOsAnos_AOOinEOObuffer_output.")

    check_if_all_species_are_in_TodosOsAnos_AOOinEOObuffer_output <- TRUE

  }


  # Check if SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output_output exists

  message("  Checking if all species are found in SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output...")

  SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output <-
    fread(

      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output(", OverlayAnalyses_ID, ").csv"

      ),
      header = T
      #, encoding = "UTF-8"

    )

  if(

    length(species[species %notin% unique(SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output$Species)]) != 0

  ){

    check_if_all_species_are_in_TodosOsAnos_AOObyQuadOfGrid_output <- FALSE

    message("    Species not found:")

    for(

      sp in species[species %notin% unique(SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output$Species)]

    ){

      message(paste0("      ", sp))

    }

  } else{

    message("    All species were found in SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output.")

    check_if_all_species_are_in_TodosOsAnos_AOObyQuadOfGrid_output <- TRUE

  }


  if(

    check_if_all_species_are_in_TodosOsAnos_output == T &
    check_if_all_species_are_in_TodosOsAnos_AOOinEOObuffer_output == T &
    check_if_all_species_are_in_TodosOsAnos_AOObyQuadOfGrid_output == T

  ){

    check_if_all_species_are_in_OverlayAnalysesOutputs <- TRUE

    return(check_if_all_species_are_in_OverlayAnalysesOutputs)

  } else {

    check_if_all_species_are_in_OverlayAnalysesOutputs <- FALSE

    return(check_if_all_species_are_in_OverlayAnalysesOutputs)

  }

}
