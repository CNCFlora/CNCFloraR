#' Scraping of data on filled profile of species from CNCFlora old System
#'
#' Obtém os dados dos perfis das espécies preenchidos do sistema antigo do CNCFlora


scrap_data_of_filledSpeciesProfile_from_oldSystem <- function(filledSpeciesProfile){

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(rvest)
        library(stringr)
        library(stringi)
        library(readr)
        library(serpstatr)

      })
    })
  })


  speciesProfileEnviron <- new.env(parent = .GlobalEnv)

  speciesProfile <- read_html(filledSpeciesProfile)

  speciesProfile1 <- speciesProfile %>%
    html_nodes("p") %>%
    html_text()
  speciesProfile1 <- as.data.frame(speciesProfile1)

  speciesProfile2 <- speciesProfile %>%
    html_nodes("li") %>%
    html_text()
  speciesProfile2 <- as.data.frame(speciesProfile2)

  biomes <- NULL
  biome_MA <- speciesProfile2 %>% dplyr::filter(speciesProfile2 == "Mata Atlântica")
  biomes <- rbind(biomes, if(exists("biome_MA") ==T){biome_MA})
  biome_AM <- speciesProfile2 %>% dplyr::filter(speciesProfile2 == "Amazônia")
  biomes <- rbind(biomes, if(exists("biome_AM") ==T){biome_AM})
  biome_CE <- speciesProfile2 %>% dplyr::filter(speciesProfile2 == "Cerrado")
  biomes <- rbind(biomes, if(exists("biome_CE") ==T){biome_CE})
  biome_CA <- speciesProfile2 %>% dplyr::filter(speciesProfile2 == "Caatinga")
  biomes <- rbind(biomes, if(exists("biome_CA") ==T){biome_CA})
  biome_PAM <- speciesProfile2 %>% dplyr::filter(speciesProfile2 == "Pampa")
  biomes <- rbind(biomes, if(exists("biome_PAM") ==T){biome_PAM})
  biome_PAN <- speciesProfile2 %>% dplyr::filter(speciesProfile2 == "Pantanal")
  biomes <- rbind(biomes, if(exists("biome_PAN") ==T){biome_PAN})

  speciesProfileEnviron$biomes <- biomes$speciesProfile2

  biomes_n <- nrow(biomes)

  speciesProfileEnviron$biomes_n <- biomes_n

  endemicToBrazil <-
    speciesProfile1 %>%
    dplyr::filter(

      str_detect(

        speciesProfile1,
        "\\s\\s\\s*Endêmica do Brasil"

      )

    )

  endemicToBrazil <- nrow(endemicToBrazil)

  if(

    endemicToBrazil == 1

  ){

    endemicToBrazil <- TRUE

  } else {

    endemicToBrazil <- FALSE

  }

  speciesProfileEnviron$endemicToBrazil <- endemicToBrazil

  distribution <-
    speciesProfile1 %>%
    dplyr::filter(

      str_detect(

        speciesProfile1,
        " distribuição: "

      )

    )

  speciesProfileEnviron$distribution <- distribution$speciesProfile1

  if(

    endemicToBrazil == F

  ){

    distribution_out_of_Brazil <- sub("Resumo\\:", "", distribution)
    distribution_out_of_Brazil <- sub("\\s\\(.*", "", distribution_out_of_Brazil)
    distribution_out_of_Brazil <- sub("\\..*", "", distribution_out_of_Brazil)

  }

  distribution <- str_extract(distribution$speciesProfile1, " distribuição:.*?\\.")
  distribution <- sub(" distribuição: ", "", distribution)
  distribution <- sub(" no Distrito Federal ", " no estado do Distrito Federal ", distribution)
  distribution <- str_split(distribution, " no estado d")
  distribution <- distribution[[1]]
  distribution <- sub("no estado d", "", distribution)
  distribution <- sub("^e\\s", "",distribution)
  distribution <- sub("^a\\s", "",distribution)
  distribution <- sub("^o\\s", "",distribution)
  distribution <- sub("\\s$", "", distribution)
  distribution <- sub("\\.$", "", distribution)
  distribution <- sub("nos municípios ", "", distribution)
  distribution <- sub("no município ", "", distribution)
  distribution <- sub("e Abreu e Lima", ", Abreu e Lima", distribution)
  distribution <- sub("e Passa e Fica", ", Passa e Fica", distribution)
  distribution <- sub("e Pontes e Lacerda", ", Pontes e Lacerda", distribution)
  distribution <- sub(" e$", "", distribution)
  distribution <- sub(" e ", ", ", distribution)
  distribution <- sub(paste0(" ", paste("\U02014"), ", "), "", distribution)

  distribution_in_BrazilianStates <- str_extract(

    distribution,
    paste0(

      ".*\\s",
      paste("\U02014"),
      "\\s"

    )

  )

  distribution_in_BrazilianStates <- sub(

    paste0(

      "\\s",
      paste("\U02014"),
      "\\s"

    ),
    "",
    distribution_in_BrazilianStates

  )

  distribution_in_BrazilianStates <- sub("Acre", "AC", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Alagoas", "AL", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Amapá", "AP", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Amazonas", "AM", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Bahia", "BA", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Ceará", "CE", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Distrito Federal", "DF", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Espírito Santo", "ES", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Goiás", "GO", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Maranhão", "MA", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Mato Grosso do Sul", "MS", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Mato Grosso", "MT", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Minas Gerais", "MG", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Pará", "PA", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Paraíba", "PB", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Paraná", "PR", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Pernambuco", "PE", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Piauí", "PI", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Rio de Janeiro", "RJ", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Rio Grande do Norte", "RN", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Rio Grande do Sul", "RS", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Rondônia", "RO", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Roraima", "RR", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Santa Catarina", "SC", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("São Paulo", "SP", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Sergipe", "SE", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("Tocantins", "TO", distribution_in_BrazilianStates)

  speciesProfileEnviron$distribution_in_BrazilianStates <- distribution_in_BrazilianStates

  distribution_in_BrazilianStates_n <- length(distribution_in_BrazilianStates)

  speciesProfileEnviron$distribution_in_BrazilianStates_n <- distribution_in_BrazilianStates_n

  distribution_in_BrazilianMunicipalities <- str_extract(distribution, paste0("\\s", paste0("\U02014"), "\\s.*"))
  distribution_in_BrazilianMunicipalities <- sub(paste0("\\s", paste("\U02014"),"\\s"), "", distribution_in_BrazilianMunicipalities)
  distribution_in_BrazilianMunicipalities <- gsub(paste0("\\s", paste("\U02014"),"\\,"), "", distribution_in_BrazilianMunicipalities)
  distribution_in_BrazilianMunicipalities <- str_split(distribution_in_BrazilianMunicipalities, "\\, ")

  distribution_in_BrazilianMunicipalities2 <- suppressWarnings(sst_lists_to_df(distribution_in_BrazilianMunicipalities))
  distribution_in_BrazilianMunicipalities2 <- as.matrix(distribution_in_BrazilianMunicipalities2)

  distribution_in_BrazilianStatesAndMunicipalities <- paste(distribution_in_BrazilianMunicipalities2, " (", distribution_in_BrazilianStates, ")", sep = "")
  distribution_in_BrazilianStatesAndMunicipalities <- as.data.frame(distribution_in_BrazilianStatesAndMunicipalities)
  distribution_in_BrazilianStatesAndMunicipalities <- unique(distribution_in_BrazilianStatesAndMunicipalities)

  speciesProfileEnviron$distribution_in_BrazilianStatesAndMunicipalities <- distribution_in_BrazilianStatesAndMunicipalities$distribution_in_BrazilianStatesAndMunicipalities

  distribution_in_BrazilianMunicipalities_n <- nrow(distribution_in_BrazilianStatesAndMunicipalities)

  speciesProfileEnviron$distribution_in_BrazilianMunicipalities_n <- distribution_in_BrazilianMunicipalities_n

  distribution_in_BrazilianStates <- sub("AC", "Acre", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("AL", "Alagoas", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("AP", "Amapá", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("AM", "Amazonas", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("BA", "Bahia", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("CE", "Ceará", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("DF", "Distrito Federal", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("ES", "Espírito Santo", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("GO", "Goiás", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("MA", "Maranhão", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("MS", "Mato Grosso do Sul", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("MT", "Mato Grosso", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("MG", "Minas Gerais", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("PA", "Pará", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("PB", "Paraíba", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("PR", "Paraná", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("PE", "Pernambuco", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("PI", "Piauí", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("RJ", "Rio de Janeiro", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("RN", "Rio Grande do Norte", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("RS", "Rio Grande do Sul", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("RO", "Rondônia", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("RR", "Roraima", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("SC", "Santa Catarina", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("SP", "São Paulo", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("SE", "Sergipe", distribution_in_BrazilianStates)
  distribution_in_BrazilianStates <- sub("TO", "Tocantins", distribution_in_BrazilianStates)

  speciesProfileEnviron$distribution_in_BrazilianStates <- distribution_in_BrazilianStates

  vegetations <- speciesProfile2[speciesProfile2$speciesProfile2 %in%
                                  c("Área antrópica",
                                    "Caatinga (stricto sensu)",
                                    "Campinarana",
                                    "Campo de Altitude",
                                    "Campo de Várzea",
                                    "Campo Limpo",
                                    "Campo Rupestre",
                                    "Carrasco",
                                    "Cerrado (lato sensu)",
                                    "Floresta Ciliar e/ou Galeria",
                                    "Floresta de Igapó",
                                    "Floresta de Terra-Firme",
                                    "Floresta de Várzea",
                                    "Floresta Estacional Decidual",
                                    "Floresta Estacional Perenifólia",
                                    "Floresta Estacional Semidecidual",
                                    "Floresta Ombrófila (Floresta Pluvial)",
                                    "Floresta Ombrófila Mista",
                                    "Manguezal",
                                    "Palmeiral",
                                    "Restinga",
                                    "Savana Amazônica",
                                    "Vegetação Aquática",
                                    "Vegetação sobre afloramentos rochosos"), ]
  vegetations <- sub("Área antrópica", "área antrópica", vegetations)
  vegetations <- sub("Caatinga \\(stricto sensu\\)", "caatinga (stricto sensu)", vegetations)
  vegetations <- sub("Campinarana", "campinarana", vegetations)
  vegetations <- sub("Campo de Altitude", "campos de altitude", vegetations)
  vegetations <- sub("Campo de Várzea", "campos de várzea", vegetations)
  vegetations <- sub("Campo Limpo", "campo limpo", vegetations)
  vegetations <- sub("Campo Rupestre", "campos rupestres", vegetations)
  vegetations <- sub("Carrasco", "carrasco", vegetations)
  vegetations <- sub("Cerrado \\(lato sensu\\)", "cerrado (lato sensu)", vegetations)
  vegetations <- sub("Floresta Ciliar e/ou Galeria", "floresta ciliar e/ou galeria", vegetations)
  vegetations <- sub("Floresta de Igapó", "floresta de igapó", vegetations)
  vegetations <- sub("Floresta de Terra-Firme", "floresta de terra-firme", vegetations)
  vegetations <- sub("Floresta de Várzea", "floresta de várzea", vegetations)
  vegetations <- sub("Floresta Estacional Decidual", "floresta estacional decidual", vegetations)
  vegetations <- sub("Floresta Estacional Perenifólia", "floresta estacional perenifólia", vegetations)
  vegetations <- sub("Floresta Estacional Semidecidual", "floresta estacional semidecidual", vegetations)
  vegetations <- sub("Floresta Ombrófila \\(Floresta Pluvial\\)", "floresta ombrófila densa", vegetations)
  vegetations <- sub("Floresta Ombrófila Mista", "floresta ombrófila mista", vegetations)
  vegetations <- sub("Manguezal", "manguezal", vegetations)
  vegetations <- sub("Palmeiral", "palmeiral", vegetations)
  vegetations <- sub("Restinga", "restinga", vegetations)
  vegetations <- sub("Savana Amazônica", "savana amazônica", vegetations)
  vegetations <- sub("Vegetação Aquática", "vegetação aquática", vegetations)
  vegetations <- sub("Vegetação sobre afloramentos rochosos", "vegetação sobre afloramentos rochosos", vegetations)

  speciesProfileEnviron$vegetations <- vegetations

  EOO <- speciesProfile1 %>%
    dplyr::filter(str_detect(speciesProfile1, "Extent of occurrence.*"))

  EOO <- parse_number(EOO$speciesProfile1)

  speciesProfileEnviron$EOO <- EOO

  AOO <- speciesProfile1 %>%
    dplyr::filter(str_detect(speciesProfile1, "Area of occupancy.*"))

  AOO <- as.numeric(parse_number(AOO$speciesProfile1))

  speciesProfileEnviron$AOO <- AOO

  habit <- speciesProfile1 %>%
    dplyr::filter(

      str_detect(

        speciesProfile1,
        "Resumo\\:\\s(Árvore|Arbusto|Subarbusto|Palmeira|Liana|Trepadeira|Planta volúvel).*"

      )

    )

  habit <- sub("Resumo\\:\\s", "", habit)
  habit <- sub("\\s\\(.*", "", habit)
  habit <- sub("\\..*", "", habit)

  speciesProfileEnviron$habit <- habit

  UCs <- speciesProfile1 %>%
    dplyr::filter(

      str_detect(

        speciesProfile1,
        "A espécie foi registrada n(a|as) seguint(e|es) Unidad(e|es) de Conservação"

      )

    )

  UCs <- sub("A espécie foi registrada n(a|as) seguint(e|es) Unidad(e|es) de Conservação\\:\\s", "", UCs)
  UCs <- sub("\\.", "", UCs)
  UCs <- stri_replace_last(UCs, fixed = " e ", ", ")
  UCs <- str_split(UCs, ", ", simplify = T)

  speciesProfileEnviron$UCs <- UCs

  return(speciesProfileEnviron)

}
