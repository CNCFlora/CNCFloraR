checkOverlayAnalysesOutputs <- function(OverlayAnalyses_ID){

  # Check if SIG_land_use_TodosOsAnos_output exists

  SIG_land_use_TodosOsAnos_output_Exists <- file.exists(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "SIG_land_use_TodosOsAnos_output(", OverlayAnalyses_ID, ").csv"

    )

  )

  if(SIG_land_use_TodosOsAnos_output_Exists == T){

    message("  SIG_land_use_TodosOsAnos_output FOUND")

  } else {

    message("  SIG_land_use_TodosOsAnos_output NOT FOUND")

  }


  # Check if SIG_land_use_TodosOsAnos_AOOinEOObuffer_output exists

  SIG_land_use_TodosOsAnos_AOOinEOObuffer_output_Exists <- file.exists(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "SIG_land_use_TodosOsAnos_AOOinEOObuffer_output(", OverlayAnalyses_ID, ").csv"

    )

  )

  if(SIG_land_use_TodosOsAnos_AOOinEOObuffer_output_Exists == T){

    message("  SIG_land_use_TodosOsAnos_AOOinEOObuffer_output FOUND")

  } else {

    message("  SIG_land_use_TodosOsAnos_AOOinEOObuffer_output NOT FOUND")

  }


  # Check if SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output_output exists

  SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output_Exists <- file.exists(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output(", OverlayAnalyses_ID, ").csv"

    )

  )

  if(SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output_Exists == T){

    message("  SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output FOUND")

  } else {

    message("  SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output NOT FOUND")

  }


  if(

    SIG_land_use_TodosOsAnos_output_Exists == T &
    SIG_land_use_TodosOsAnos_AOOinEOObuffer_output_Exists == T &
    SIG_land_use_TodosOsAnos_AOObyQuadOfGrid_output_Exists == T

  ){

    checkOverlayAnalysesOutputs_OK <- TRUE

    return(checkOverlayAnalysesOutputs_OK)

  } else {

    checkOverlayAnalysesOutputs_OK <- FALSE

    return(checkOverlayAnalysesOutputs_OK)

  }

}
