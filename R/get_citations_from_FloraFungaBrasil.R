get_citations_from_FloraFungaBrasil <- function(){

  library(googledrive)
  library(RSelenium)
  library(dplyr)
  library(stringr)
  library(data.table)

  # Get list of species file (species_for_FloraFungaBrasil_citations.csv) ####

  ## Get local path of the downloaded list of species file ####

  message("Importing the list of species from the local computer...")

  listOfSpecies_localPath <- paste0(sub("Packages/CNCFloraR", "", getwd()), "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_FloraFungaBrasil_citations.csv")


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
    sep = ";",
    encoding = "UTF-8"

  )

  message("List of species imported.")


  # Get follow-up table (follow-up_table.csv)

  ## Get local path of the downloaded follow-up table file ####

  message("Importing the follow-up table from the local computer...")

  followUpTable_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/follow-up_table/follow-up_table.csv"

  )


  ## Import the list of species file from local path ####

  followUpTable <- fread(

    followUpTable_localPath,
    header = T,
    sep = ";"

  )

  message("Follow-up table imported.")

  # get ID of species in Flora e Funga do Brasil from followUpTable ####

  message("Getting the ID of species in Flora e Funga do Brasil from follow-up table...")

  followUpTable_n <- which(followUpTable$NameFB_semAutor %in% listOfSpecies$V1)
  data_from_followUpTable <- followUpTable[followUpTable_n,]
  data_from_followUpTable <- data.frame(

    NameFB_semAutor = data_from_followUpTable$NameFB_semAutor,
    taxonID = data_from_followUpTable$taxonID

  )

  data_from_followUpTable$genus <- stringr::word(data_from_followUpTable$NameFB_semAutor, 1)

  URLs <- NULL
  for(i in data_from_followUpTable$taxonID){

    URLs_ <- paste0(

      "http://floradobrasil.jbrj.gov.br/reflora/listaBrasil/FichaPublicaTaxonUC/FichaPublicaTaxonUC.do?id=FB", i

    )

    URLs <- rbind(URLs, URLs_)

  }
  URLs_n <- 1:length(URLs)

  message("IDs obtained.")


  # Create a container in Docker ####

  message("Starting docker...")

  #shell('docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1')

  tryCatch(

    expr = {

      rsDriver(verbose = FALSE)
      ERROR <- F

    },
    error = function(e){

      ERROR <- T

    },
    finally = {

      message('...')

    }

  )

  remDr <- remoteDriver(

    remoteServerAddr = "localhost",
    port = 4567L,
    browserName = "firefox"

  )

  remDr$open()

  message("Browser opened.")

  Sys.sleep(2)

  message("Scrapping citations...")

  citations <- NULL
  for(i in URLs_n){

    message(paste0(data_from_followUpTable$NameFB_semAutor[i]))

    Sys.sleep(3)
    remDr$navigate(URLs[i])
    Sys.sleep(3)
    element <- remDr$findElement(using = "class", "citacao")
    citation <- element$getElementText()
    citations_ <-
      data.frame(

        genus = data_from_followUpTable$genus[i],
        species = data_from_followUpTable$NameFB_semAutor[i],
        citation = as.vector(as.matrix(citation))

      )
    citations <- rbind(citations, citations_)

  }

  remDr$close()

  message("Browser closed.")


  # Fix string ####

  message("Correcting strings...")

  # Long citation

  citations$citation <- sub("^Citação\n", "", citations$citation)
  citations$citation <- sub(". Acesso em:", " (acesso em", citations$citation)
  citations$citation <- sub("\\. 2022$", " 2022)", citations$citation)
  citations$citation <- sub("\\. 2023$", " 2023)", citations$citation)
  citations$citation <- sub("\\sjan\\s", " de janeiro de ", citations$citation)
  citations$citation <- sub("\\sfev\\s", " de fevereiro de ", citations$citation)
  citations$citation <- sub("\\smar\\s", " de março de ", citations$citation)
  citations$citation <- sub("\\sabr\\s", " de abril de ", citations$citation)
  citations$citation <- sub("\\smai\\s", " de maio de ", citations$citation)
  citations$citation <- sub("\\sjun\\s", " de junho de ", citations$citation)
  citations$citation <- sub("\\sjul\\s", " de julho de ", citations$citation)
  citations$citation <- sub("\\sago\\s", " de agosto de ", citations$citation)
  citations$citation <- sub("\\sset\\s", " de setembro de ", citations$citation)
  citations$citation <- sub("\\sout\\s", " de outubro de ", citations$citation)
  citations$citation <- sub("\\snov\\s", " de novembro de ", citations$citation)
  citations$citation <- sub("\\sdez\\s", " de dezembro de ", citations$citation)
  citations$citation <- sub("Disponível em: <", " URL ", citations$citation)
  citations$citation <- sub(">", "", citations$citation)
  citations$citation <- sub(" in ", ". ", citations$citation)
  citations$citation <- sub(". (\\w+{1}). Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro.", "., 2022. \\1. Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro.", citations$citation)


  # Short citation

  index_no_author <- grep("^\\w*\\.\\sFlora e Funga do Brasil", t(citations$citation))
  index_2_authors <- which(str_count(citations$citation, ";") == 1)
  index_more_than_2_authors <- which(str_count(citations$citation, ";") > 1)
  index_more_than_1_no_author <- c(index_2_authors, index_more_than_2_authors)
  index_1_author <- citations$citation_length[-index_more_than_1_no_author]
  citations$citation <- gsub(";", ",", citations$citation)

  citations2 <- citations$citation

  citations2[index_no_author] <- "Flora e Funga do Brasil, 2022"

  citations2[index_more_than_2_authors] <-
    sub(

      "\\,.*\\.\\,.*",
      " et al., 2022",
      citations2[index_more_than_2_authors]

    )
  citations2[index_2_authors] <-
    sub(

      "\\,\\s.*\\.\\,\\s([A-Z]{1})",
      " e \\1",
      citations2[index_2_authors]

    )
  citations2[index_2_authors] <-
    sub(

      " e (\\w+{1})\\, .*\\, 2022.*",
      " e \\1, 2022",
      citations2[index_2_authors]

    )
  citations2[index_1_author] <-
    sub(

      "\\,\\s.*",
      ", 2022",
      citations2[index_1_author]

    )
  citations2[index_1_author] <-
    sub(

      ".*\\.\\sFlora e Funga do Brasil",
      "Flora e Funga do Brasil, 2022",
      citations2[index_1_author]

    )

  no_author_long_citation_index <-
    which(

      str_detect(

        citations$citation,
        "^\\w*\\.\\sFlora e Funga do Brasil"

      )

    )

  citations$citation[no_author_long_citation_index] <-
    paste(

      "Flora e Funga do Brasil, 2022. ",
      citations$citation[no_author_long_citation_index],
      sep=""

    )

  citations$citation <- as.data.frame(citations$citation)
  citations2 <- as.data.frame(citations2)

  citations3 <- cbind(citations$citation, citations2)
  colnames(citations3) <- c("zcitationFB","zcitationFB_short")

  message("Strings OK.")

  citationsFinal <-
    data.frame(

      species = data_from_followUpTable$NameFB_semAutor,
      citations3

    )

  message("Saving Follow-up table in local computer...")

  write.csv2(

    citationsFinal,
    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/outputs/citationsFloraFungaBrasil/citationsFloraFungaBrasil.csv"

    ),
    row.names = F,
    fileEncoding = "UTF-8"

  )

  message("Follow-up table saved.")
  message("Done!")
  message(

    paste0(

      "Check file: ",
      sub(

        "Packages/CNCFloraR",
        "",
        getwd()

      ),
      "/CNCFlora_data/outputs/citationsFloraFungaBrasil/citationsFloraFungaBrasil.csv"

    )

  )

}
