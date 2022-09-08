fill_followUpTable_with_URLs_of_profileOfSpeciesHTMLs <- function(){

  library(googledrive)
  library(googlesheets4)
  library(stringr)



  files <- drive_ls(drive_get(id = "1B4XkGRyQJYVlYcqrbIWxBttbhaEGAoiB"))

  listOfSpecies <- sub("\\.html", "", files$name)


  ss <- gs4_get("https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=1874291321")
  read_ss_sheet1 <- read_sheet(ss, sheet = 1)

  species_without_profileOfSpeciesHTML_in_followUpTable_sheet1 <-
    read_ss_sheet1 %>%
    dplyr::filter(NameFB_semAutor %in% listOfSpecies) %>%
    dplyr::filter(is.na(HTMLs) == T) %>%
    dplyr::select(NameFB_semAutor)

  species_without_profileOfSpeciesHTML_in_followUpTable_sheet1 <-
    sub("$", ".html", species_without_profileOfSpeciesHTML_in_followUpTable_sheet1$NameFB_semAutor)

  URLs_to_fill_sheet1 <- files %>% dplyr::filter(name %in% species_without_profileOfSpeciesHTML_in_followUpTable_sheet1)

  URLs_to_fill_sheet1 <- data.frame(

    species = sub("\\.html", "", URLs_to_fill_sheet1$name),
    URL = paste0(

      '=HIPERLINK("',
      drive_link(URLs_to_fill_sheet1),
      '")'

    )

  )


  read_ss_sheet6 <- read_sheet(ss, sheet = 6)

  read_ss_sheet6.filtered <-
    read_ss_sheet6[read_ss_sheet6$hasHTMLprofile == F & is.na(read_ss_sheet6$Status) == T,]

  URLs_to_fill_sheet6 <- files %>% dplyr::filter(name %in% sub("$",".html", read_ss_sheet6.filtered$Espécie))

  URLs_to_fill_sheet6 <- data.frame(

    species = sub("\\.html", "", URLs_to_fill_sheet6$name),
    URL = paste0(

      '=HIPERLINK("',
      drive_link(URLs_to_fill_sheet6),
      '")'

    )

  )


  for(i in 1:nrow(URLs_to_fill_sheet1)){

    celula_HTML <- which(read_ss_sheet1$NameFB_semAutor == URLs_to_fill_sheet1$species[i])
    celula_HTML <- paste("BA", celula_HTML + 1, sep="")
    URL <- gs4_formula(URLs_to_fill_sheet1$URL[i])

    range_write(

      ss,
      data = data.frame((URL)),
      range = celula_HTML,
      col_names = F,
      sheet = 1

    )

    Sys.sleep(2)

  }

  for(i in 1:nrow(URLs_to_fill_sheet6)){

    celula_HTML <- which(read_ss_sheet6$Espécie == URLs_to_fill_sheet6$species[i])
    celula_HTML <- paste("G", celula_HTML + 1, sep="")

    range_write(

      ss,
      data = as.data.frame(as.character(paste0("Feito. ", format(Sys.time(), "%d/%m/%Y")))),
      range = celula_HTML,
      col_names = F,
      sheet = 6

    )

    Sys.sleep(2)

  }

}

