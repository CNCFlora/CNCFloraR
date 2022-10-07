get_sheet_obraPrinceps_from_infoSpeciesTable_in_cloud <- function(){

  ss_infoSpeciesTable <- gs4_get(ss_infoSpeciesTable_URL)
  obraPrinceps_infoSpeciesTable <- read_sheet(

    ss_infoSpeciesTable,
    sheet = which(ss_infoSpeciesTable$sheets$name == "obrasPrinceps")

    )

  return(obraPrinceps_infoSpeciesTable)

}
