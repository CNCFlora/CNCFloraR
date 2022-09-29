get_sheet_Acomp_spp_from_followUpTable_in_cloud <- function(){

  ss_followUpTable <- gs4_get(ss_followUpTable_URL)
  Acomp_spp_followUpTable <- read_sheet(

    ss_followUpTable,
    sheet = which(ss_followUpTable$sheets$name == "Acomp_spp")

  )

  return(Acomp_spp_followUpTable)

}
