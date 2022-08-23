get_obraPrinceps_from_Tropicos_IPNI  <-  function(){

  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({

        library(httr)
        library(jsonlite)
        library(rlist)
        library(kewr)
        library(dplyr)
        library(tidyr)
        library(stringr)
        library(rapportools)
        library(data.table)

      })
    })
  })


  # Get list of species file (species_for_obraPrinceps.csv) ####

  ## Get local path of the downloaded list of species file ####

  listOfSpecies_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_obraPrinceps.csv"

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
    sep = ";",
    encoding = "UTF-8"

  )


  message("Starting...\nWait please...")


  # Fix some expressions ####

  listOfSpecies$V2 <- gsub("\\.\\s",".",listOfSpecies$V2)
  listOfSpecies$V2 <- gsub("var\\.","var. ",listOfSpecies$V2)
  listOfSpecies$V2 <- gsub("\\.\\&",". &",listOfSpecies$V2)
  listOfSpecies$V2 <- gsub("\\.\\(,",". (",listOfSpecies$V2)
  listOfSpecies$V2 <- gsub("\\.ex\\s",". ex ",listOfSpecies$V2)


  # Concatenate taxon and author ####

  data0 <- as.data.frame(str_split(listOfSpecies$V1, " ", simplify = TRUE))

  data1 <- data.frame(data0, listOfSpecies)
  data1_n <- 1:as.numeric(count(data1))

  data2 <- NULL
  for(i in data1_n){

    if(data1$V2[i]=="" & data1$V3[i]=="" & data1$V4[i]==""){

      data2[i] <-  paste(data1$V1[i], data1$V2.1[i])

    } else {

      if(data1$V2[i]==data1$V4[i]){

        data2[i] <-  paste(data1$V1[i], data1$V2[i], data1$V2.1[i], data1$V3[i], data1$V4[i])

      } else {

        if(data1$V4[i]!=""){

          data2[i] <-  paste(data1$V1[i], data1$V2[i], data1$V3[i], data1$V4[i], data1$V2.1[i])

        } else {

          data2[i] <-  paste(data1$V1[i], data1$V2[i], data1$V2.1[i])

        }

      }

    }

  }

  data2 <- as.data.frame(data2)
  colnames(data2) <- "TaxonAuthor"

  data <- data.frame(data2, listOfSpecies)

  data_for_Tropicos <- ""
  for(i in data1_n){

    if(data1$V2[i] == data1$V4[i]){

      data_for_Tropicos[i] <- paste(data1$V1[i], data1$V2[i])

    } else{

      if(data1$V3[i] %in% c("var.","subsp.")){

        data_for_Tropicos[i] <- paste(data1$V1[i], data1$V2[i], data1$V4[i])

      } else{

        if(data1$V3[i]=="" & data1$V4[i]==""){

          data_for_Tropicos[i] <- paste(data1$V1[i], data1$V2[i])

        }

      }

    }

  }

  data_for_Tropicos <- gsub(" ","%20", data_for_Tropicos)
  data_for_Tropicos <- gsub("\\%20$","", data_for_Tropicos)
  data_for_Tropicos <- as.data.frame(data_for_Tropicos)

  DATASEARCH_for_Tropicos <- ""
  for(i in data1_n){

    if(data1$V2[i] == data1$V4[i]){

      DATASEARCH_for_Tropicos[i] <- paste(data1$V1[i], data1$V2[i])

    } else{

      if(data1$V3[i] %in% c("var.","subsp.")){

        DATASEARCH_for_Tropicos[i] <- paste(data1$V1[i], data1$V2[i], data1$V3[i], data1$V4[i])

      } else{

        if(data1$V3[i]=="" & data1$V4[i]==""){

          DATASEARCH_for_Tropicos[i] <- paste(data1$V1[i], data1$V2[i])

        }

      }

    }

  }

  DATASEARCH_for_Tropicos <- sub("\\s$","",DATASEARCH_for_Tropicos)
  DATASEARCH_for_Tropicos <- as.data.frame(DATASEARCH_for_Tropicos)

  data_for_IPNI <- ""
  for(i in data1_n){

    if(data1$V2[i] == data1$V4[i]){

      data_for_IPNI[i] <- paste(data1$V1[i], data1$V2[i])

    } else{

      if(data1$V3[i] %in% c("var.","subsp.")){

        data_for_IPNI[i] <- paste(data1$V1[i], data1$V2[i], data1$V3[i], data1$V4[i])

      } else{

        if(data1$V3[i]=="" & data1$V4[i]==""){

          data_for_IPNI[i] <- paste(data1$V1[i], data1$V2[i])

        }

      }

    }

  }

  data_for_IPNI <- sub("\\s$","", data_for_IPNI)
  data_for_IPNI <- as.data.frame(data_for_IPNI)

  data <- data.frame(data, data_for_Tropicos,DATASEARCH_for_Tropicos, data_for_IPNI)
  data_n <- 1:as.numeric(count(data))

  Output <-
    data.frame(

      Taxon="",
      Author="",
      TaxonAuthor="",
      TaxonAuthor_Tropicos="",
      Author_Tropicos="",
      Status_Tropicos="",
      TaxonAuthor_IPNI="",
      Author_IPNI="",
      Status_IPNI="",
      ObraPrinceps_Tropicos="",
      NomenclatureStatus_Tropicos="",
      ObraPrinceps_IPNI="",
      Status=""

    )

  for (i in data_n){

    # Objetos gerais

    TaxonAuthor_data <- data$TaxonAuthor[i]
    Taxon_data <- data$V1[i]
    Author_data <- data$V2[i]
    data_for_Tropicos <- data$data_for_Tropicos[i]
    DATA_for_Tropicos <- data$DATASEARCH_for_Tropicos[i]
    DATA_for_IPNI <- data$data_for_IPNI[i]

    # Tropicos

    data_Tropicos <- GET(paste("http://services.tropicos.org/Name/Search?name=", data_for_Tropicos, "&apikey=b611d7e4-9a84-4024-9a84-892b7e8f0ec8&format=json&exact=exact", sep=""))
    parsed_Tropicos <- fromJSON(content(data_Tropicos, "text", encoding = "UTF-8"), simplifyVector = FALSE)

    if(parsed_Tropicos[[1]][1] == "No names were found"){

      Tropicos_result <-
        data.frame(

          Taxon_Tropicos="",
          Author_Tropicos="",
          TaxonAuthor_Tropicos="",
          ObraPrinceps_Tropicos="",
          NomenclatureStatus_Tropicos="",
          Status_Tropicos=""

        )

    } else {

      usefull_data_Tropicos <- list.select(parsed_Tropicos,NomenclatureStatusName,ScientificName,Author,ScientificNameWithAuthors,DisplayReference,DisplayDate);usefull_data_Tropicos <- do.call(rbind.data.frame, usefull_data_Tropicos)
      usefull_data_Tropicos$Author <- gsub("\\.\\s",".",usefull_data_Tropicos$Author)
      usefull_data_Tropicos$Author <- gsub("var\\.","var. ",usefull_data_Tropicos$Author)
      usefull_data_Tropicos$Author <- gsub("\\.\\&",". &",usefull_data_Tropicos$Author)
      usefull_data_Tropicos$Author <- gsub("\\.\\(,",". (",usefull_data_Tropicos$Author)
      usefull_data_Tropicos$Author <- gsub("\\.ex\\s",". ex ",usefull_data_Tropicos$Author)
      usefull_data_Tropicos$ScientificNameWithAuthors <- gsub("\\.\\s",".",usefull_data_Tropicos$ScientificNameWithAuthors)
      usefull_data_Tropicos$ScientificNameWithAuthors <- gsub("var\\.","var. ",usefull_data_Tropicos$ScientificNameWithAuthors)
      usefull_data_Tropicos$ScientificNameWithAuthors <- gsub("\\.\\&",". &",usefull_data_Tropicos$ScientificNameWithAuthors)
      usefull_data_Tropicos$ScientificNameWithAuthors <- gsub("\\.\\(,",". (",usefull_data_Tropicos$ScientificNameWithAuthors)
      usefull_data_Tropicos$ScientificNameWithAuthors <- gsub("\\.ex\\s",". ex ",usefull_data_Tropicos$ScientificNameWithAuthors)

      ##Filtragens

      if(

        count(

          usefull_data_Tropicos %>%
          dplyr::filter(ScientificName == DATA_for_Tropicos & Author == Author_data)

        ) == 1

      ){

        Tropicos_result <-
          usefull_data_Tropicos %>%
          dplyr::filter(ScientificName == DATA_for_Tropicos & Author == Author_data)

        Tropicos_result <- data.frame(Tropicos_result, Status_Tropicos = "Authors matched")

      }

      if(

        count(

          usefull_data_Tropicos %>%
          dplyr::filter(ScientificName == DATA_for_Tropicos & Author == Author_data)

        ) > 1

      ){

        Tropicos_result <- usefull_data_Tropicos %>% filter(ScientificName == DATA_for_Tropicos & Author == Author_data);Tropicos_result <- data.frame(Tropicos_result,Status_Tropicos="Authors matched")} else{
          if(count(usefull_data_Tropicos %>% filter(ScientificName == DATA_for_Tropicos & Author == Author_data))==0){
            if(count(Tropicos_result <- usefull_data_Tropicos %>% filter(ScientificName == DATA_for_Tropicos))>0){Tropicos_result <- usefull_data_Tropicos %>% filter(ScientificName == DATA_for_Tropicos);Tropicos_result <- data.frame(Tropicos_result,Status_Tropicos="Authors not matched")} else {
              if(as.numeric(count(Tropicos_result))==0){
                if(agrep(DATA_for_Tropicos, usefull_data_Tropicos$ScientificName, 0.01)>0){
                  agrep_name_index <- agrep(DATA_for_Tropicos, usefull_data_Tropicos$ScientificName, 0.01)
                  if(count(usefull_data_Tropicos %>% filter(ScientificName == usefull_data_Tropicos$ScientificName[agrep_name_index] & Author == Author_data))>0){
                    Tropicos_result <- usefull_data_Tropicos %>% filter(ScientificName == usefull_data_Tropicos$ScientificName[agrep_name_index] & Author == Author_data);Tropicos_result <- data.frame(Tropicos_result,Status_Tropicos="Name not matched")} else{
                      if(count(usefull_data_Tropicos %>% filter(ScientificName == usefull_data_Tropicos$ScientificName[agrep_name_index]))>0){
                        Tropicos_result <- usefull_data_Tropicos %>% filter(ScientificName == usefull_data_Tropicos$ScientificName[agrep_name_index]);Tropicos_result <- data.frame(Tropicos_result,Status_Tropicos="Name and Authors not matched")}
                    }
                }
              } else {Tropicos_result <- data.frame(Taxon_Tropicos="", Author_Tropicos="", TaxonAuthor_Tropicos="", ObraPrinceps_Tropicos="", NomenclatureStatus_Tropicos="", Status_Tropicos="")}
            }
          }
        }

      ##Bloco ajuste do dataframe do Tropicos

      ObraPrinceps_Tropicos <- paste(Tropicos_result$DisplayReference,". ", Tropicos_result$DisplayDate, sep="")
      TaxonAuthor_Tropicos <- as.data.frame(str_split(Tropicos_result$ScientificName, " ", simplify=TRUE))
      TaxonAuthor_Tropicos <- data.frame(TaxonAuthor_Tropicos, Tropicos_result$Author)

      TaxonAuthor_Tropicos <-
        if(is.empty(TaxonAuthor_Tropicos$V3)){

          #Espécie (sem variedade)
          paste(TaxonAuthor_Tropicos$V1,TaxonAuthor_Tropicos$V2,TaxonAuthor_Tropicos$Tropicos_result.Author)

        } else{

          #Variedade típica

          if(TaxonAuthor_Tropicos$V3 %in% c("var.","subsp.") & TaxonAuthor_Tropicos$V4 == TaxonAuthor_Tropicos$V2){

            paste(

              TaxonAuthor_Tropicos$V1,
              TaxonAuthor_Tropicos$V2,
              TaxonAuthor_Tropicos$Tropicos_result.Author,
              TaxonAuthor_Tropicos$V3,
              TaxonAuthor_Tropicos$V4

            )

          } else{

            #Variedade não típica

            if(

              TaxonAuthor_Tropicos$V3 %in% c("var.","subsp.") &
              TaxonAuthor_Tropicos$V4 != TaxonAuthor_Tropicos$V2

            ){

              paste(

                TaxonAuthor_Tropicos$V1,
                TaxonAuthor_Tropicos$V2,
                TaxonAuthor_Tropicos$V3,
                TaxonAuthor_Tropicos$V4,
                TaxonAuthor_Tropicos$Tropicos_result.Author

              )

            }

          }

        }

      Tropicos_result <-
        data.frame(

          Tropicos_result$ScientificName,
          Tropicos_result$Author,
          TaxonAuthor_Tropicos,
          ObraPrinceps_Tropicos,
          Tropicos_result$NomenclatureStatusName,
          Tropicos_result$Status_Tropicos

        )
      colnames(Tropicos_result) <-
        c(

          "Taxon_Tropicos",
          "Author_Tropicos",
          "TaxonAuthor_Tropicos",
          "ObraPrinceps_Tropicos",
          "NomenclatureStatus_Tropicos",
          "Status_Tropicos"

        )

    }


    # IPNI

    data_IPNI <- suppressMessages(search_ipni(DATA_for_IPNI))
    if(data_IPNI$total == 0){

      IPNI_result <-
        data.frame(

          Taxon_IPNI = "",
          Author_IPNI = "",
          TaxonAuthor_IPNI = "",
          ObraPrinceps_IPNI = "",
          Status_IPNI = ""

        )

    } else{

      parsed_IPNI <- tidy(data_IPNI)
      usefull_data_IPNI <- data.frame(Species=parsed_IPNI$name, authors=parsed_IPNI$authors, reference=parsed_IPNI$reference)


      # Filtragens

      if(

        count(

          usefull_data_IPNI %>%
          dplyr::filter(Species == DATA_for_IPNI & authors == Author_data)

        ) == 1

      ){

        IPNI_result <- usefull_data_IPNI %>% filter(Species == DATA_for_IPNI & authors == Author_data)
        IPNI_result <- data.frame(IPNI_result,Status_IPNI="Authors matched")

      }

      if(

        count(

          usefull_data_IPNI %>% filter(Species == DATA_for_IPNI & authors == Author_data)

        ) > 1

      ){

        IPNI_result <-
          usefull_data_IPNI %>%
          dplyr::filter(Species == DATA_for_IPNI & authors == Author_data)

        IPNI_result <- data.frame(IPNI_result, Status_IPNI = "Authors matched")

      } else{

        if(

          count(

            usefull_data_IPNI %>% filter(Species == DATA_for_IPNI & authors == Author_data)

          ) == 0

        ){


          if(

            count(

              usefull_data_IPNI %>% filter(Species == DATA_for_IPNI)

            ) == 0

          ){

            IPNI_result <-
              data.frame(

                Species="",
                authors="",
                TaxonAuthor_IPNI="",
                Status_IPNI=""

              )

          } else{

            IPNI_result <-
              usefull_data_IPNI %>%
              dplyr::filter(Species == DATA_for_IPNI)
            IPNI_result <- data.frame(IPNI_result, Status_IPNI = "Authors not matched")

          }

        }

      }


      # Bloco ajuste do dataframe do IPNI

      ObraPrinceps_IPNI <- sub("\\s$","",IPNI_result$reference)

      if(length(ObraPrinceps_IPNI) == 0){

        ObraPrinceps_IPNI <- ""

      }

      TaxonAuthor_IPNI <-
        as.data.frame(

          str_split(

            IPNI_result$Species,
            " ",
            simplify=TRUE

          )

        )

      TaxonAuthor_IPNI <- data.frame(TaxonAuthor_IPNI, IPNI_result$authors)
      TaxonAuthor_IPNI <-
        if(is.empty(TaxonAuthor_IPNI$V3)){

          # Espécie (sem variedade)

          paste(

            TaxonAuthor_IPNI$V1,
            TaxonAuthor_IPNI$V2,
            TaxonAuthor_IPNI$IPNI_result.authors

          )

        } else {

          # Variedade típica

          if(

            TaxonAuthor_IPNI$V3 %in% c("var.","subsp.") &
            TaxonAuthor_IPNI$V4 == TaxonAuthor_IPNI$V2){

            paste(

              TaxonAuthor_IPNI$V1,
              TaxonAuthor_IPNI$V2,
              TaxonAuthor_IPNI$IPNI_result.authors,
              TaxonAuthor_IPNI$V3,
              TaxonAuthor_IPNI$V4

            )

          } else {

            # Variedade não típica

            if(

              TaxonAuthor_IPNI$V3 %in% c("var.","subsp.") &
              TaxonAuthor_IPNI$V4 != TaxonAuthor_IPNI$V2){

              paste(

                TaxonAuthor_IPNI$V1,
                TaxonAuthor_IPNI$V2,
                TaxonAuthor_IPNI$V3,
                TaxonAuthor_IPNI$V4,
                TaxonAuthor_IPNI$IPNI_result.authors

              )

            }

          }

        }

      suppressWarnings(

        if(TaxonAuthor_IPNI == "  "){

          TaxonAuthor_IPNI <- ""

        }

      )

      IPNI_result <-
        data.frame(

          IPNI_result$Species,
          IPNI_result$authors,
          TaxonAuthor_IPNI,
          ObraPrinceps_IPNI,
          IPNI_result$Status_IPNI

        )

      colnames(IPNI_result) <-
        c(

          "Taxon_IPNI",
          "Author_IPNI",
          "TaxonAuthor_IPNI",
          "ObraPrinceps_IPNI",
          "Status_IPNI"

        )

    }


    # Output

    Output_Tropicos_n <- as.numeric(count(Tropicos_result))
    Output_Tropicos_blank <-
      data.frame(

        Taxon_Tropicos = "",
        Author_Tropicos = "",
        TaxonAuthor_Tropicos = "",
        ObraPrinceps_Tropicos = "",
        NomenclatureStatus_Tropicos = "",
        Status_Tropicos = ""

      )

    Output_IPNI_n <- as.numeric(count(IPNI_result))
    Output_IPNI_blank <-
      data.frame(

        Taxon_IPNI = "",
        Author_IPNI = "",
        TaxonAuthor_IPNI = "",
        ObraPrinceps_IPNI = "",
        Status_IPNI = ""

      )

    if(Output_Tropicos_n == Output_IPNI_n){} else{

      if(Output_Tropicos_n > Output_IPNI_n){

        Output_IPNI_blank_end <-
          do.call(

            "rbind",
            replicate(Output_Tropicos_n - Output_IPNI_n, Output_IPNI_blank, simplify = FALSE)

          )

        IPNI_result <- rbind(IPNI_result, Output_IPNI_blank_end)

      } else{

        if(Output_Tropicos_n<Output_IPNI_n){

          Output_Tropicos_blank_end <-
            do.call(

              "rbind",
              replicate(Output_IPNI_n-Output_Tropicos_n, Output_Tropicos_blank, simplify = FALSE)

            )

          Tropicos_result <- rbind(Tropicos_result, Output_Tropicos_blank_end)}

      }

    }

    Output_end <-

      data.frame(

        Taxon = Taxon_data,
        Author = Author_data,
        TaxonAuthor = TaxonAuthor_data,
        TaxonAuthor_Tropicos = Tropicos_result$TaxonAuthor_Tropicos,
        Author_Tropicos = Tropicos_result$Author_Tropicos,
        Status_Tropicos = Tropicos_result$Status_Tropicos,
        TaxonAuthor_IPNI = IPNI_result$TaxonAuthor_IPNI,
        Author_IPNI = IPNI_result$Author_IPNI,
        Status_IPNI = IPNI_result$Status_IPNI,
        ObraPrinceps_Tropicos = Tropicos_result$ObraPrinceps_Tropicos,
        NomenclatureStatus_Tropicos = Tropicos_result$NomenclatureStatus_Tropicos,
        ObraPrinceps_IPNI = IPNI_result$ObraPrinceps_IPNI

      )

    if(count(Output_end) > 1){

      Output_end <- data.frame(Output_end, Status = "Verificar")

    } else{

      if(count(Output_end) == 1){

        Output_end <- data.frame(Output_end, Status = "OK")

      }

    }

    Output <- rbind(Output, Output_end)

  }

  Output <- Output[-1,]

  Output <- data.frame(

    Output,
    Revision = ""

  )

  write.csv2(

    Output,
    paste0(sub("Packages/CNCFloraR", "", getwd()), "/CNCFlora_data/outputs/obrasPrinceps/obrasPrinceps.csv"),
    row.names = F,
    fileEncoding = "UTF-8"

  )

  message(

    paste0(

      "File ",
      paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/outputs/obrasPrinceps/obrasPrinceps.csv"),
      " created."

    )

  )

  selection <- toupper(readline("Open the file in spreadsheet editor? (y/n)"))

  if(selection == "Y"){

    message("Resolve lines marked with 'Verificar'.\nYou need to keep one line per species.\nUse column 'Revision' to select the correct form to citation.")

    invisible(readline(prompt = "Press ENTER to continue."))

    shell(

      paste0(

        "start scalc ",
        paste0(sub("Packages/CNCFloraR", "", getwd()), "/CNCFlora_data/outputs/obrasPrinceps/obrasPrinceps.csv")

      )

    )

  }

}
