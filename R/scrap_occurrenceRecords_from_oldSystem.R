scrap_occurrenceRecords_from_oldSystem <- function(input){


  # Scraping: Validation of occurrence records ####

  registros <- input %>% html_nodes(".label-valid") %>% html_text()
  registros <- str_extract(registros, "\\w+")


  # Scraping: SIG validation of occurrence records ####

  validation_SIG <- input %>% html_nodes(".label-sig") %>% html_text()
  validation_SIG <- str_extract(validation_SIG, "\\w+\\s\\w+")


  # Scraping: URNs IDs of occurrence records ####

  URNs <- input %>% html_nodes("a") %>% html_attr("name")
  URNs <- as.data.frame(URNs)
  URNs <- URNs %>% drop_na()
  URNs <- as.data.frame(URNs[-1,])
  toDelete <- seq(2, nrow(URNs), 2)
  URNs <- as.data.frame(URNs[toDelete,])


  # Scraping: States of occurrence records ####

  ESTADOS <- input %>% html_nodes("#stateProvince") %>% html_attr("value")
  ESTADOS <- toupper(ESTADOS)
  ESTADOS <- sub("AMAPA", "AMAPÁ", ESTADOS)
  ESTADOS <- sub("CEARA", "CEARÁ", ESTADOS)
  ESTADOS <- sub("ESPIRITO SANTO", "ESPÍRITO SANTO", ESTADOS)
  ESTADOS <- sub("GOIAS", "GOIÁS", ESTADOS)
  ESTADOS <- sub("MARANHAO", "MARANHÃO", ESTADOS)
  ESTADOS <- sub("PARAIBA", "PARAÍBA", ESTADOS)
  ESTADOS <- sub("PARANA", "PARANÁ", ESTADOS)
  ESTADOS <- sub("PARA", "PARÁ", ESTADOS)
  ESTADOS <- sub("PARÁNÁ", "PARANÁ", ESTADOS)
  ESTADOS <- sub("PIAUI", "PIAUÍ", ESTADOS)
  ESTADOS <- sub("RONDONIA", "RONDÔNIA", ESTADOS)
  ESTADOS <- sub("SAO PAULO", "SÃO PAULO", ESTADOS)
  ESTADOS <- str_trim(ESTADOS, side = c("both"))


  # Scraping: Municipalities of occurrence records ####

  MUNICIPIOS <- input %>%
    html_nodes("#municipality") %>%
    html_attr("value")
  MUNICIPIOS <- str_trim(MUNICIPIOS, side = c("both"))
  MUNICIPIOS <- toupper(MUNICIPIOS)


  # Scraping: Geographic coordinates of occurrence records ####

  coords <- input %>%
    html_elements(".col-md-6") %>%
    html_elements("input") %>%
    html_attr("value")
  coords2 <- matrix(coords, ncol = 6, byrow = TRUE)
  coords_df <- as.data.frame(coords2)
  coords_df <- coords_df[,-6]
  coords_df <- coords_df[,-5]
  coords_df <- coords_df %>%
    dplyr::select(V2, V1, V3, V4)

  coords_df <- data.frame(
    coords_df,
    URNs,
    registros,
    validation_SIG,
    ESTADOS,
    MUNICIPIOS
  )

  colnames(coords_df) <-
    c(
      "lon",
      "lat",
      "precision",
      "protocol",
      "URNs",
      "Validade",
      "Validade_SIG",
      "ESTADOS",
      "MUNICIPIOS"
    )

  return(coords_df)

}
