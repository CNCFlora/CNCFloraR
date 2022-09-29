get_sheet_Acomp_spp_from_infoSpeciesTable_in_cloud <- function(){

  ss_infoSpeciesTable <- gs4_get(ss_infoSpeciesTable_URL)
  Acomp_spp_infoSpeciesTable <- read_sheet(ss_infoSpeciesTable, sheet = which(ss_infoSpeciesTable$sheets$name == "Acomp_spp"))

  return(Acomp_spp_infoSpeciesTable)

}
