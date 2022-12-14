#' Verify the validation of occurrence records
#'
#' Verifica se o arquivo corresponde ao nome da espécie e se há registros não validados

validationOccurrences <- function(list = "") {

  # Load libraries ####

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(readtext)
        library(data.table)
        library(stringi)
        library(stringr)
        library(tibble)
        library(knitr)
        library(kableExtra)
        library(htmlTable)
        library(installr)
        library(purrr)
        library(magicfor)
        library(rvest)
        library(googledrive)
        library(colorDF)

      })
    })
  })

  if(list[1] == ""){

    # List of Species file (validationOccurrences.csv) ####

    ## Get local path of the downloaded list of species file ####

    listOfSpecies_localPath <- paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/listOfSpecies_for_processing/validationOccurrences.csv"

    )

    ## Ask to open the list of species file ####

    answer <- ""

    while(answer != "Y" |
          answer != "N" ){

      answer <-
        toupper(readline("Open the list of species file? (y/n): "))

      if(answer == "Y"){

        shell(listOfSpecies_localPath)

        answer2 <- ""

        while(answer2 != "Y"){

          answer2 <-
            toupper(readline("List of species file ready? (y): "))

          if(answer2 == "Y"){

            break

          }

        }

        break

      }

      if(answer == "N"){

        break

      }

    }

    ## Import the list of species file from local path ####

    listOfSpecies <- fread(

      listOfSpecies_localPath,
      header = F,
      sep = ";"

    )

  } else {

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()

    listOfSpecies <- List_for_HTML_profile_followUpTable %>%
      dplyr::filter(Espécie %in% list)

    listOfSpecies <- data.frame(

      V1 = listOfSpecies$Espécie,
      V2 = listOfSpecies$`PA/PNA`,
      V3 = listOfSpecies$Registros

    )

  }


  # Create an empty output variable for loop ####

  output <- NULL

  # Start loop ####

  for(Species in listOfSpecies$V1){

    Species <- Species

    ## Get local path of the downloaded occurrences file ####

    occurrences_localPath <- paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/occurrences/oldSystem/",
      Species,
      ".html"

    )

    ## Import the occurrences file from local path ####

    occurrences <- read_html(occurrences_localPath)

    ## Scraping the occurrences file ####

    ### Get species name in the file ####

    species_in_file <-
      occurrences %>%
      html_nodes(css=".col-md-6") %>%
      html_nodes("p") %>%
      html_text()

    species_in_file <- species_in_file[1]
    species_in_file <- sub("[A-Z]+\\s", "", species_in_file)
    species_in_file <- sub("\\s$", "", species_in_file)

    #### Verify if the species name is the same of the occurrences file ####

    is.species_the_same_in_file <- as.logical(species_in_file == Species)

    ### Occurrences valid? ####

    registros_validos <- occurrences %>% html_nodes(css = ".label-valid") %>% html_text()
    registros_validos <- str_extract(registros_validos, "\\w+")
    registros_validos <- data.frame(Species, registros_validos)

    ### SIG valid? ####

    SIG_valid <- occurrences %>% html_nodes(".label-sig") %>% html_text()
    SIG_valid <- substr(SIG_valid, 90, nchar(SIG_valid)-54)

    ### Output ####

    validacoes <- cbind(registros_validos,
                        SIG_valid,
                        is.species_the_same_in_file)
    output <- rbind(output, validacoes)

    assign("output", output, envir = globalenv())

  }

  # End of Loop ####

  registros <- output


  # Verifications ####

  ## Verify the number of records ####

  records_n <- registros %>%
    group_by(Species) %>%
    dplyr::summarise(

      n_records = dplyr::n()

    )

  ## Verify if the name of species in HTML is the same of the file ####

  are.species_the_same_in_file <- registros %>%
    dplyr::select(Species, is.species_the_same_in_file) %>% unique()
  colnames(are.species_the_same_in_file) <- c(

    "Species",
    "SpName_file_HTML"

  )

  ## Verify if there are invalid records ####

  invalid <- registros %>% dplyr::filter(registros_validos == "Inválido")
  speciesWithInvalidOccurrences <- unique(invalid$Species)

  result_invalid_n <- NULL
  for(i in speciesWithInvalidOccurrences){
    invalid_n <-
      dplyr::count(invalid %>%
              dplyr::filter(Species == i & registros_validos == "Inválido"))
    invalid_n <-
      data.frame(

        Species = i,
        invalidated = invalid_n$n

      )

    result_invalid_n <- rbind(result_invalid_n, invalid_n)

  }


  ## Verify if there are SIG not OK ####

  records_SIG_NOT_OK <- registros %>% dplyr::filter(SIG_valid == "SIG NOT OK")

  records_SIG_NOT_OK <- records_SIG_NOT_OK %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(Species, SIG_NOT_OK = dplyr::n())

  output <- left_join(are.species_the_same_in_file, records_n)
  output <- left_join(output, result_invalid_n)
  output <- left_join(output, records_SIG_NOT_OK)

  output <- unique(output)

  All_invalid <- output$n_records == output$invalidated

  All_invalid[is.na(All_invalid) == T] <- FALSE

  All_SIG_NOT_OK <- output$n_records == output$SIG_NOT_OK

  All_SIG_NOT_OK[is.na(All_SIG_NOT_OK) == T] <- FALSE

  output <- data.frame(output, All_invalid, All_SIG_NOT_OK)


  # Print results ####

  options(colorDF_n = Inf)

  colorDF(
    output,
    theme="dark"
  )

  invisible(return(output))

}
