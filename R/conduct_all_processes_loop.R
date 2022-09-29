conduct_all_processes_loop <- function(){

  # Deleting files ####

  if(exists(paste0(getwd(),"/out1_empty_fields.html"))){

    file.remove(paste0(getwd(),"/out1_empty_fields.html"))
    file.remove(paste0(getwd(),"/out1_empty_fields.html.rawhtml"))

  }

  if(exists(paste0(getwd(),"/out2_species_not_found_in_FFB.html"))){

    file.remove(paste0(getwd(),"/out2_species_not_found_in_FFB.html"))
    file.remove(paste0(getwd(),"/out2_species_not_found_in_FFB.html.rawhtml"))

  }

  if(exists(paste0(getwd(),"/out3_species_without_obraPrinceps.html"))){

    file.remove(paste0(getwd(),"/out3_species_without_obraPrinceps.html"))
    file.remove(paste0(getwd(),"/out3_species_without_obraPrinceps.html.rawhtml"))

  }

  if(exists(paste0(getwd(),"/out4_get_occurrenceRecords.html"))){

    file.remove(paste0(getwd(),"/out4_get_occurrenceRecords.html"))
    file.remove(paste0(getwd(),"/out4_get_occurrenceRecords.html.rawhtml"))

  }

  if(exists(paste0(getwd(),"/out5_validationOccurrences.html"))){

    file.remove(paste0(getwd(),"/out5_validationOccurrences.html"))
    file.remove(paste0(getwd(),"/out5_validationOccurrences.html.rawhtml"))

  }

  if(exists(paste0(getwd(),"/out5_validationOccurrences.html"))){

    file.remove(paste0(getwd(),"/out6_errors_in_coordinates.html"))
    file.remove(paste0(getwd(),"/out6_errors_in_coordinates.html.rawhtml"))

  }

  `%notin%` <- Negate(`%in%`)

  # Load packages ####

  suppressMessages({
    suppressPackageStartupMessages({
      suppressWarnings({

        library(data.table)
        library(stringr)
        library(googlesheets4)
        library(R3port)
        library(cli)
        library(dplyr)

      })
    })
  })


  # Start loop ####

  while(0 == 0){

    # Hello loop

    cat("\014")
    print(boxx(

      c(

        "CNCFlora workflow \U221E Infinite Loop",
        "2022"

      ),
      border_col = "darkgreen",
      background_col = "darkgreen"

    ))


    # Step 1: Inclusion of species in the flow ####

    cli_h1("Step 1: Inclusion of species in the flow")

    ## Check all files of species from the file `check_all_files_of_species.csv` ####

    cli_h2("Checking and getting data from follow-up table in local computer")

    df <- check_all_files_of_species(ask_to_open_file = F)

    for(i in 1:length(df)){

      df[,i] <- str_detect(df[,i], "TRUE")

    }


    ## Get sheet List_for_HTML_profile from the follow-up table in GoogleSheets ####

    cli_h2("Checking and getting data from follow-up table in cloud")

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()


    ## Have species to include in the flow? ####

    ### Remove NAs
    List_for_HTML_profile_followUpTable <-
      List_for_HTML_profile_followUpTable[is.na(List_for_HTML_profile_followUpTable$Espécie) == F, ]

    have_species_to_include_in_the_flow <-
      all(List_for_HTML_profile_followUpTable$Espécie %in% colnames(df) == T) == F

    if(have_species_to_include_in_the_flow == T){

      ### Yes ####

      cli_alert_success("Have species to include in the flow!")

      species_to_include_in_the_flow <- List_for_HTML_profile_followUpTable$Espécie[List_for_HTML_profile_followUpTable$Espécie %in% colnames(df) == F]


      ### Get the local path of the downloaded list of species file ####

      listOfSpecies_localPath_check_all_files <-
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/check_all_files_of_species.csv"

        )


      ### Import the list of species file from local path ####

      listOfSpecies_check_all_files <- fread(

        listOfSpecies_localPath_check_all_files,
        header = F,
        sep = ";",
        encoding = "UTF-8"

      )


      ### Get data (flow ; validation of records) of species to include in the flow from follow-up table in GoogleSheets ####

      data_of_species_to_include_in_the_flow <-
        List_for_HTML_profile_followUpTable %>%
        filter(Espécie %in% species_to_include_in_the_flow)

      species_to_include_in_the_flow <- data.frame(

        V1 = data_of_species_to_include_in_the_flow$Espécie,
        V2 = data_of_species_to_include_in_the_flow$`PA/PNA`,
        V3 = data_of_species_to_include_in_the_flow$Registros

      )

      listOfSpecies_check_all_files <- rbind (

        listOfSpecies_check_all_files,
        species_to_include_in_the_flow

      )


      ### Write check_all_files_of_species.csv ####

      write.table(

        listOfSpecies_check_all_files,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/listOfSpecies_for_processing/check_all_files_of_species.csv"

        ),
        sep = ";",
        col.names = F,
        row.names = F

      )

      cli_alert_success("Species included in the flow:")
      print(species_to_include_in_the_flow)

    } else {

      cli_alert_success("No species to include in the flow!")

    }


    # Step 2: Checking for empty fields in the infoSpecies table in cloud ####

    cli_h1("Step 2: Checking for empty fields in the infoSpecies table in cloud")

    ## Get sheet List_for_HTML_profile from the follow-up table in GoogleSheets ####

    cli_h2("Reading sheet List_for_HTML_profile from followUpTable")

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()

    species_in_List_for_HTML_profile_followUpTable <-
      List_for_HTML_profile_followUpTable$Espécie[is.na(List_for_HTML_profile_followUpTable$Espécie) == F]

    ## Get sheet Acomp_spp from the infoSpeciesTable in GoogleSheets ####

    cli_h2("Reading sheet Acomp_spp from infoSpeciesTable")

    Acomp_spp_infoSpeciesTable <-
      get_sheet_Acomp_spp_from_infoSpeciesTable_in_cloud()


    ## Have empty fields in the infoSpecies table in cloud? ####

    Acomp_spp_infoSpeciesTable.filtered <-
      Acomp_spp_infoSpeciesTable %>%
      dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable)

    output_empty_fields <- data.frame(

      Species = "",
      empty_field = ""

    )

    for(species in Acomp_spp_infoSpeciesTable.filtered$NameFB_semAutor){

      fields <- c(

        "FB2020_endemism",
        "occurrenceRemarks",
        "location",
        "lifeForm",
        "vegetationType",
        "habitat"

      )

      for(field in fields){

        if(

          is.na(

            Acomp_spp_infoSpeciesTable.filtered %>%
            dplyr::filter(NameFB_semAutor == Acomp_spp_infoSpeciesTable.filtered$NameFB_semAutor[Acomp_spp_infoSpeciesTable.filtered$NameFB_semAutor == species]) %>%
            dplyr::select(field)

          ) == T

        ){

          output_empty_fields <- rbind(

            output_empty_fields,
            data.frame(

              Species = species,
              empty_field = field

            )

          )

        }

      }

    }


    if(output_empty_fields[1, 1] == ""){

      output_empty_fields <- output_empty_fields[-1, ]

    }

    output_empty_fields$Species[duplicated(output_empty_fields$Species)] <-
      ""


    if(nrow(output_empty_fields) > 0){

      ### Yes ####

      empty_fields_table <- data.frame(

        i = 1:nrow(output_empty_fields),
        Species = output_empty_fields$Species,
        empty_field = output_empty_fields$empty_field

      )


      #### Table: Empty fields ####

      html_list(

        empty_fields_table,
        vars = names(empty_fields_table),
        title = "Empty fields in the infoSpecies table to correct",
        out = "out1_empty_fields.html",
        show = F

      )

    }


    # Step 3: Get data of species from the infoSpecies table in GoogleSheets ####

    cli_h1("Step 3: Get data of species from the infoSpecies table in GoogleSheets")
    cli_h2("Checking and getting data of species")

    species_to_get_from_infoSpecies <- setdiff(

      Acomp_spp_infoSpeciesTable.filtered$NameFB_semAutor,
      unique(empty_fields_table$Species)[unique(empty_fields_table$Species) != ""]

    )

    get_species_from_followUpTable(

      list = species_to_get_from_infoSpecies,
      ask_to_open_file = F

    )


    # Step 4: Species without Flora e Funga do Brasil citation in the followUpTable in local computer ####

    cli_h1("Step 4: Species without Flora e Funga do Brasil citation")

    ## Get sheet List_for_HTML_profile from the follow-up table in GoogleSheets ####

    cli_h2("Reading sheet List_for_HTML_profile from followUpTable")

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()

    species_in_List_for_HTML_profile_followUpTable <-
      List_for_HTML_profile_followUpTable$Espécie[is.na(List_for_HTML_profile_followUpTable$Espécie) == F]


    ## Get sheet Acomp_spp from the infoSpecies table in GoogleSheets ####

    cli_h2("Checking and getting species without Flora e Funga do Brasil citation in the followUpTable in cloud")

    Acomp_spp_infoSpeciesTable <- get_sheet_Acomp_spp_from_infoSpeciesTable_in_cloud()

    Acomp_spp_infoSpeciesTable$FFB_citation_short[is.na(Acomp_spp_infoSpeciesTable$FFB_citation_short) == T] <-
      ""

    ## Have species without Flora e Funga do Brasil citation in the followUpTable in cloud? ####

    have_species_without_FFBcitation_in_followUpTable_in_cloud <-
      nrow(

        Acomp_spp_infoSpeciesTable %>%
          dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable) %>%
          dplyr::filter(is.na(FFB_citation_short) == T) %>%
          dplyr::select(NameFB_semAutor)

      ) > 0

    if(have_species_without_FFBcitation_in_followUpTable_in_cloud == T){

      ### Yes ####

      species_without_FFBcitation_in_followUpTable_in_cloud <-
        Acomp_spp_infoSpeciesTable %>%
        dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable) %>%
        dplyr::filter(is.na(FFB_citation_short) == T) %>%
        dplyr::select(NameFB_semAutor)

      species_without_FFBcitation_in_followUpTable_in_cloud <-
        species_without_FFBcitation_in_followUpTable_in_cloud$NameFB_semAutor

    }


    ## Get follow-up table from local computer ####

    cli_h2("Get followUpTable in local computer")

    followUpTable <- get_followUpTable_from_localComputer()


    ## Update follow-up table in local computer from cloud ####

    df_Acomp_spp_infoSpeciesTable <- Acomp_spp_infoSpeciesTable %>%
      dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable) %>%
      dplyr::select(NameFB_semAutor, FFB_citation_short, FFB_citation_long)

    colnames(df_Acomp_spp_infoSpeciesTable) <- c("NameFB_semAutor", "FFB_citation_short_cloud", "FFB_citation_long_cloud")

    df_followUpTable <- followUpTable %>%
      dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable) %>%
      dplyr::select(NameFB_semAutor, zcitationFB2020_short, zcitationFB2020)

    colnames(df_followUpTable) <- c("NameFB_semAutor", "FFB_citation_short_local_computer", "FFB_citation_long_local_computer")

    df_check <- left_join(df_Acomp_spp_infoSpeciesTable, df_followUpTable, by = "NameFB_semAutor")

    if(

      all(

        df_check$FFB_citation_short_local_computer == "" &
        df_check$FFB_citation_short_cloud != "" &
        df_check$FFB_citation_short_cloud != "coletar"

      ) == T

    ){

      species_in_df_check <- df_check$NameFB_semAutor[

        df_check$FFB_citation_short_local_computer == "" &
          df_check$FFB_citation_short_cloud != "" &
          df_check$FFB_citation_short_cloud != "coletar"

      ]

      followUpTable$zcitationFB2020_short[

        followUpTable$NameFB_semAutor %in% species_in_df_check

      ] <- df_check$FFB_citation_short_cloud[

        df_check$NameFB_semAutor %in% species_in_df_check

      ]

      followUpTable$zcitationFB2020[

        followUpTable$NameFB_semAutor %in% species_in_df_check

      ] <- df_check$FFB_citation_long_cloud[

        df_check$NameFB_semAutor %in% species_in_df_check

      ]

      write.csv2(

        followUpTable,
        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/inputs/follow-up_table/follow-up_table.csv"

        ),
        row.names = FALSE,
        fileEncoding = "UTF-8"

      )

    }


    ## Have species without Flora e Funga do Brasil citation in the followUpTable in local computer? ####

    have_species_without_FFBcitation_in_followUpTable_in_localComputer <-
      !all(followUpTable$zcitationFB2020 == "", F)


    if(have_species_without_FFBcitation_in_followUpTable_in_localComputer == T){

      ### Yes ####

      species_without_FFBcitation_in_followUpTable_in_localComputer <-
        followUpTable$NameFB_semAutor[

          followUpTable$zcitationFB2020 == "" |
            is.na(followUpTable$zcitationFB2020) == T

        ]

      #### Include species without Flora e Funga do Brasil citation in the followUpTable in cloud ####

      if(exists("species_without_FFBcitation_in_followUpTable_in_cloud") == T){

        species_without_FFBcitation_in_followUpTable_in_localComputer <- c(

          species_without_FFBcitation_in_followUpTable_in_cloud,
          species_without_FFBcitation_in_followUpTable_in_localComputer

        )

      }

      species_without_FFBcitation_in_followUpTable_in_localComputer <-
        species_without_FFBcitation_in_followUpTable_in_localComputer[

          duplicated(species_without_FFBcitation_in_followUpTable_in_localComputer) == F

        ]

      #### Get Flora e Funga do Brasil citation by scraping ####

      cli_h2("Get Flora e Funga do Brasil citation by scraping")

      FFBcitations <- get_citations_from_FloraFungaBrasil(

        list = species_without_FFBcitation_in_followUpTable_in_localComputer,
        ask_to_open_file = F

      )

      species_not_found_in_FFBcitations <- FFBcitations$species[which(FFBcitations$zcitationFB == c("", ""), T)]

      if(length(species_not_found_in_FFBcitations) > 0){

        species_not_found_in_FFBcitations_table <- data.frame(

          i = 1:length(species_not_found_in_FFBcitations),
          Species = species_not_found_in_FFBcitations

        )


        ### Discover the cell in infoSpecies_table ####

        species_not_found_in_FFBcitations_table$Species <- paste0(

          '<a href="',
          ss_infoSpeciesTable_URL_URL,
          '&range=',
          LETTERS_for_colNum[grep("FFB_citation_short", colnames(Acomp_spp_infoSpeciesTable))],
          which(Acomp_spp_infoSpeciesTable$NameFB_semAutor %in% species_not_found_in_FFBcitations_table$Species)+1,
          '">',
          species_not_found_in_FFBcitations_table$Species,
          '</a>'

        )


        ### Table: Species not found ####

        html_list(

          species_not_found_in_FFBcitations_table,
          vars = names(species_not_found_in_FFBcitations_table),
          title = "Species not found in Flora e Funga do Brasil",
          out = "out2_species_not_found_in_FFB.html",
          show = F

        )

      }


      ## Fill the follow-up table in local computer ####

      if(length(FFBcitations$species) > 0){

        fill_followUpTable_with_citations_from_FloraFungaBrasil(

          list = FFBcitations$species

        )


        ## Fill the follow-up table in cloud ####

        ### get infoSpecies Table ####

        ss_infoSpeciesTable <- gs4_get(ss_infoSpeciesTable_URL)

        Acomp_spp_infoSpeciesTable <- get_sheet_Acomp_spp_from_infoSpeciesTable_in_cloud()

        FFBcitations[FFBcitations$zcitationFB_short == "",]$zcitationFB_short <-
          "coletar"

        ### fill infoSpecies Table ####

        for(FFBcitation_i in 1:nrow(FFBcitations)){

          FFBcitation <- FFBcitations[FFBcitation_i, ]
          FFBcitation_species <- FFBcitation$species
          FFBcitation_short <- FFBcitation$zcitationFB_short
          FFBcitation_long <- FFBcitation$zcitationFB

          range_write(

            data = as.data.frame(FFBcitation_short),
            range = as.character(

              paste0(

                LETTERS_for_colNum[grep("FFB_citation_short", colnames(Acomp_spp_infoSpeciesTable))],
                grep(FFBcitation_species, Acomp_spp_infoSpeciesTable$NameFB_semAutor) + 1

              )

            ),
            ss = ss_infoSpeciesTable,
            sheet = which(ss_infoSpeciesTable$sheets$name == "Acomp_spp"),
            col_names = F

          )

          Sys.sleep(1)

          range_write(

            data = as.data.frame(FFBcitation_long),
            range = as.character(

              paste0(

                LETTERS_for_colNum[grep("FFB_citation_long", colnames(Acomp_spp_infoSpeciesTable))],
                grep(FFBcitation_species, Acomp_spp_infoSpeciesTable$NameFB_semAutor) + 1

              )

            ),
            ss = ss_infoSpeciesTable,
            sheet = which(ss_infoSpeciesTable$sheets$name == "Acomp_spp"),
            col_names = F

          )

          Sys.sleep(1)

        }

      }

    }


    # Step 5: Species without obra princeps in the followUpTable in local computer ####

    cli_h1("Step 5: Species without obra princeps in the followUpTable")
    cli_h2("Checking and getting data of species without obra princeps in the followUpTable in local computer")

    followUpTable <- get_followUpTable_from_localComputer()


    ## Have species without obra princeps in the followUpTable in local computer ####

    have_species_without_obraPrinceps <-
      !all(followUpTable$zobra == "", F)

    species_without_obraPrinceps <-
      followUpTable$NameFB_semAutor[followUpTable$zobra == ""]

    species_without_obraPrinceps_table <- data.frame(

      i = 1:length(species_without_obraPrinceps),
      Species = species_without_obraPrinceps

    )

    html_list(

      species_without_obraPrinceps_table,
      vars = names(species_without_obraPrinceps_table),
      title = "Species to get obra princeps",
      out = "out3_species_without_obraPrinceps.html",
      show = F,
      footnote = '
        <button type="button" onclick="run_R_script()">Execute R function: get_obraPrinceps_from_Tropicos_IPNI()</button>
        <script>
          function run_R_script() {
            var ExePath = "C:/Program Files/RStudio/bin/rstudio.exe";
            var scriptToExecute = "C:/R/R-4.1.1/working/Packages/CNCFloraR/R/get_obraPrinceps_from_Tropicos_IPNI.R";
            new ActiveXObject("Shell.Application").ShellExecute(ExePath, scriptToExecute);
            var WshShell = new ActiveXObject("WScript.Shell");
            WshShell.SendKeys("1");

          }
        </script>

        '

    )

    Acomp_spp_infoSpeciesTable <- get_sheet_Acomp_spp_from_infoSpeciesTable_in_cloud()

    Acomp_spp_infoSpeciesTable.filtered <-
      Acomp_spp_infoSpeciesTable %>%
      dplyr::filter(NameFB_semAutor %in% species_without_obraPrinceps_table) %>%
      dplyr::select(NameFB_semAutor, FB2020_AcceptedNameUsage)

    if(TRUE %in% duplicated(Acomp_spp_infoSpeciesTable$NameFB_semAutor)){

      Acomp_spp_infoSpeciesTable.filtered <- Acomp_spp_infoSpeciesTable.filtered[-duplicated(Acomp_spp_infoSpeciesTable.filtered$NameFB_semAutor),]

    }

    output_obraPrinceps <- Acomp_spp_infoSpeciesTable.filtered

    output_obraPrinceps$FB2020_AcceptedNameUsage <- sub("\\w+\\s", "", output_obraPrinceps$FB2020_AcceptedNameUsage)
    output_obraPrinceps$FB2020_AcceptedNameUsage <- sub("\\w+\\s", "", output_obraPrinceps$FB2020_AcceptedNameUsage)

    colnames(output_obraPrinceps) <- c("species", "author")

    write.table(

      data.frame(

        species = output_obraPrinceps$species,
        author = output_obraPrinceps$author

      ),
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/listOfSpecies_for_processing/species_for_obraPrinceps.csv"

      ),
      col.names = F,
      row.names = F,
      sep = ";"

    )


    # Step 6: Get occurrence records from old system ####

    cli_h1("Step 6: Get occurrence records from old system")
    cli_h2("Checking and getting occurrence records")

    listOfSpecies_getOccurrences <- colnames(

      df[

        which(

          df["occurrenceRecords",] == F

        )

      ]

    )

    if(length(listOfSpecies_getOccurrences) > 0){

      cli_alert_info("Including species")

      species_to_get_occurrenceRecords <- prepare_listOfSpecies_files_to_getOccurrences(

        ask_to_open_file = F,
        ask_to_write_file = F

      )

      AHKscript_to_download_occurrenceRecords_from_oldSystem(

        species_to_get_occurrenceRecords,
        ask_to_open_file = F,
        ask_to_open_filePath = F

      )


      ## Table: Get occurrence records ####

      get_occurrenceRecords_table <- data.frame(

        i = 1:length(species_to_get_occurrenceRecords),
        Species = species_to_get_occurrenceRecords

      )

      html_list(

        get_occurrenceRecords_table,
        vars = names(get_occurrenceRecords_table),
        title = "Species to get occurrence records",
        out = "out4_get_occurrenceRecords.html",
        show = F,
        footnote = '
        <button type="button" onclick="runAhkScript()">Executar AHK script! Pressione F4 para disparar.</button>
        <script>
          function runAhkScript() {
            var ahkExePath = "C:/Program Files/AutoHotkey/AutoHotkey.exe";
            var scriptToExecute = "C:/R/R-4.1.1/working/CNCFlora_data/outputs/AHK_scripts/get_occurrences.ahk";
            new ActiveXObject("Shell.Application").ShellExecute(ahkExePath, scriptToExecute);
          }
        </script>

        '

      )

    } else {

      cli_alert_success("No species to get occurrence records.")

    }


    # Step 7: Check validation of occurrence records and SIG ####

    cli_h1("Step 7: Check validation of occurrence records and SIG")

    cli_h2("Checking species in the flow")

    listOfSpecies_checkValidation <- colnames(

      df[

        which(

          df["intersectPANs",] == F &
            df["intersectTERs",] == F &
            df["intersectUCs",] == F &
            df["overlayMapBiomasTodosOsAnos",] == F

        )

      ]

    )

    output_validationOccurrences <- validationOccurrences(

      list = listOfSpecies_checkValidation

    )

    output_validationOccurrences <- output_validationOccurrences %>%
      dplyr::filter(

        SpName_file_HTML == F |
          All_invalid == T |
          All_SIG_NOT_OK == T

      )

    validationOccurrences_table <- data.frame(

      i = 1:nrow(output_validationOccurrences),
      Species = output_validationOccurrences$Species,
      SpName_file_HTML = output_validationOccurrences$SpName_file_HTML,
      n_records = output_validationOccurrences$n_records,
      invalidated = output_validationOccurrences$invalidated,
      SIG_NOT_OK = output_validationOccurrences$SIG_NOT_OK,
      All_invalid = output_validationOccurrences$All_invalid,
      All_SIG_NOT_OK = output_validationOccurrences$All_SIG_NOT_OK

    )

    html_list(

      validationOccurrences_table,
      vars = names(validationOccurrences_table),
      title = "Check validations of occurrence records",
      out = "out5_validationOccurrences.html",
      show = F

    )


    # Step 8: Check validation of coordinates ####

    cli_h1("Step 8: Check validation of coordinates")

    cli_h2("Checking species in the flow")

    listOfSpecies_checkValidationCoords <- colnames(

      df[

        which(

          df["intersectPANs",] == F &
            df["intersectTERs",] == F &
            df["intersectUCs",] == F &
            df["overlayMapBiomasTodosOsAnos",] == F

        )

      ]

    )

    output_validationCoordinates <- validationCoordinates(

      list = listOfSpecies_checkValidationCoords,
      ask_to_open_file = F

    )


    validationCoordinates_table <- merge(

      output_validationCoordinates[[1]],
      output_validationCoordinates[[2]]

    )

    validationCoordinates_table$Validade <- sub(

      "álido",
      "alid",
      validationCoordinates_table$Validade

    )

    html_list(

      validationCoordinates_table,
      vars = names(validationCoordinates_table),
      title = "Errors in coordinates",
      out = "out6_errors_in_coordinates.html",
      show = F

    )


    # Generate final report (report.html) ####

    html_combine(

      out = "report.hta",
      rtitle = paste0("Report: ", format(Sys.time(), "%d/%m/%Y - %H:%M:%S")),
      toctheme = TRUE,
      template = paste0(system.file(package = "R3port"), "/bootstrap_for_HTA.html")

    )


    # Deleting files ####

    if(exists(paste0(getwd(),"/out1_empty_fields.html"))){

      file.remove(paste0(getwd(),"/out1_empty_fields.html"))
      file.remove(paste0(getwd(),"/out1_empty_fields.html.rawhtml"))

    }

    if(exists(paste0(getwd(),"/out2_species_not_found_in_FFB.html"))){

      file.remove(paste0(getwd(),"/out2_species_not_found_in_FFB.html"))
      file.remove(paste0(getwd(),"/out2_species_not_found_in_FFB.html.rawhtml"))

    }

    if(exists(paste0(getwd(),"/out3_species_without_obraPrinceps.html"))){

      file.remove(paste0(getwd(),"/out3_species_without_obraPrinceps.html"))
      file.remove(paste0(getwd(),"/out3_species_without_obraPrinceps.html.rawhtml"))

    }

    if(exists(paste0(getwd(),"/out4_get_occurrenceRecords.html"))){

      file.remove(paste0(getwd(),"/out4_get_occurrenceRecords.html"))
      file.remove(paste0(getwd(),"/out4_get_occurrenceRecords.html.rawhtml"))

    }

    if(exists(paste0(getwd(),"/out5_validationOccurrences.html"))){

      file.remove(paste0(getwd(),"/out5_validationOccurrences.html"))
      file.remove(paste0(getwd(),"/out5_validationOccurrences.html.rawhtml"))

    }

    if(exists(paste0(getwd(),"/out5_validationOccurrences.html"))){

      file.remove(paste0(getwd(),"/out6_errors_in_coordinates.html"))
      file.remove(paste0(getwd(),"/out6_errors_in_coordinates.html.rawhtml"))

    }


    rm(

      list = setdiff(

        ls(),
        c(

          "`%notin%`"

        )

      )

    )

    # End of loop ####

  }


  # End of function: conduct_all_processes_loop() ####

}
