conduct_all_processes_loop <- function(){

  # Deleting files of previous report ####

  if(file.exists(paste0(getwd(),"/out1_empty_fields.html"))){

    file.remove(paste0(getwd(),"/out1_empty_fields.html"))
    file.remove(paste0(getwd(),"/out1_empty_fields.html.rawhtml"))

  }

  if(file.exists(paste0(getwd(),"/out2_species_not_found_in_FFB.html"))){

    file.remove(paste0(getwd(),"/out2_species_not_found_in_FFB.html"))
    file.remove(paste0(getwd(),"/out2_species_not_found_in_FFB.html.rawhtml"))

  }

  if(file.exists(paste0(getwd(),"/out3_species_without_obraPrinceps.html"))){

    file.remove(paste0(getwd(),"/out3_species_without_obraPrinceps.html"))
    file.remove(paste0(getwd(),"/out3_species_without_obraPrinceps.html.rawhtml"))

  }

  if(file.exists(paste0(getwd(),"/out4_get_occurrenceRecords.html"))){

    file.remove(paste0(getwd(),"/out4_get_occurrenceRecords.html"))
    file.remove(paste0(getwd(),"/out4_get_occurrenceRecords.html.rawhtml"))

  }

  if(file.exists(paste0(getwd(),"/out5_validationOccurrences.html"))){

    file.remove(paste0(getwd(),"/out5_validationOccurrences.html"))
    file.remove(paste0(getwd(),"/out5_validationOccurrences.html.rawhtml"))

  }

  if(file.exists(paste0(getwd(),"/out6_errors_in_coordinates.html"))){

    file.remove(paste0(getwd(),"/out6_errors_in_coordinates.html"))
    file.remove(paste0(getwd(),"/out6_errors_in_coordinates.html.rawhtml"))

  }

  if(file.exists(paste0(getwd(),"/out7_overlay_analyses_errors.html"))){

    file.remove(paste0(getwd(),"/out7_overlay_analyses_errors.html"))
    file.remove(paste0(getwd(),"/out7_overlay_analyses_errors.html.rawhtml"))

  }

  if(file.exists(paste0(getwd(),"/out8_intersect_analyses_errors.html"))){

    file.remove(paste0(getwd(),"/out8_intersect_analyses_errors.html"))
    file.remove(paste0(getwd(),"/out8_intersect_analyses_errors.html.rawhtml"))

  }

  if(file.exists(paste0(getwd(),"/out9_overlay_analyses_fire_errors.html"))){

    file.remove(paste0(getwd(),"/out9_overlay_analyses_fire_errors.html"))
    file.remove(paste0(getwd(),"/out9_overlay_analyses_fire_errors.html.rawhtml"))

  }

  if(file.exists(paste0(getwd(),"/out10_profileOfSpeciesHTML.html"))){

    file.remove(paste0(getwd(),"/out10_profileOfSpeciesHTML.html"))
    file.remove(paste0(getwd(),"/out10_profileOfSpeciesHTML.html.rawhtml"))

  }


  if(file.exists(paste0(getwd(),"/report.hta"))){

    file.remove(paste0(getwd(),"/report.hta"))

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

    ## Clear workspace ####

    rm(

      list = setdiff(

        ls(),
        c(

          "%notin%"

        )

      )

    )

    # Console print Hello! ####

    cat("\014")
    print(boxx(

      c(

        "CNCFlora workflow \U221E Infinite Loop",
        "2022"

      ),
      border_col = "darkgreen",
      background_col = "darkgreen"

    ))


    # Step 1: Get species in the flow ####

    cli_h1("Step 1: Get species in the flow")

    ## Get sheet List_for_HTML_profile from the follow-up table in GoogleSheets ####

    cli_h2("Checking and getting data from follow-up table in cloud")

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()

    List_for_HTML_assessment_followUpTable <-
      get_sheet_List_for_HTML_assessment_from_followUpTable_in_cloud()

    species_in_the_flow_profile <- List_for_HTML_profile_followUpTable$Espécie[

      is.na(List_for_HTML_profile_followUpTable$Espécie) == F

    ]

    species_in_the_flow_assessment <- List_for_HTML_assessment_followUpTable$Espécie[

      is.na(List_for_HTML_assessment_followUpTable$Espécie) == F

    ]

    species_in_the_flow <- c(species_in_the_flow_profile, species_in_the_flow_assessment)

    ## Check all files of species in the flow ####

    cli_h2("Check all files of species in the flow")

    df <- check_all_files_of_species(

      list = species_in_the_flow,
      ask_to_open_file = F

    )

    for(i in 1:length(df)){

      df[,i] <- str_detect(df[,i], "TRUE")

    }


    ### Get data (flow ; validation of records) of species to include in the profile of species flow from follow-up table in GoogleSheets ####

    #### Profile of species ####
    data_of_species_to_include_in_the_flow_profile <-
      List_for_HTML_profile_followUpTable %>%
      filter(Espécie %in% species_in_the_flow)

    species_in_the_flow_profile <- data.frame(

      V1 = data_of_species_to_include_in_the_flow_profile$Espécie,
      V2 = data_of_species_to_include_in_the_flow_profile$`PA/PNA`,
      V3 = data_of_species_to_include_in_the_flow_profile$Registros

    )

    #### Assessment ####

    data_of_species_to_include_in_the_flow_assessment <-
      List_for_HTML_assessment_followUpTable %>%
      filter(Espécie %in% species_in_the_flow)

    species_in_the_flow_assessment <- data.frame(

      V1 = data_of_species_to_include_in_the_flow_assessment$Espécie,
      V2 = data_of_species_to_include_in_the_flow_assessment$`PA/PNA`,
      V3 = ""

    )


    listOfSpecies_check_all_files <- rbind(

      species_in_the_flow_profile,
      species_in_the_flow_assessment

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

      for(field_ in fields){

        if(

          is.na(

            Acomp_spp_infoSpeciesTable.filtered %>%
            dplyr::filter(NameFB_semAutor == Acomp_spp_infoSpeciesTable.filtered$NameFB_semAutor[Acomp_spp_infoSpeciesTable.filtered$NameFB_semAutor == species]) %>%
            dplyr::select(field_)

          ) == T

        ){

          output_empty_fields <- rbind(

            output_empty_fields,
            data.frame(

              Species = species,
              empty_field = field_

            )

          )

        }

      }

    }


    if(

      output_empty_fields[1, 1] == "" |
      is.na(output_empty_fields[1, 1]) == T

    ){

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


    if(nrow(output_empty_fields) > 0){

      species_to_get_from_infoSpecies <- setdiff(

        Acomp_spp_infoSpeciesTable.filtered$NameFB_semAutor,
        unique(empty_fields_table$Species)[unique(empty_fields_table$Species) != ""]

      )

      get_species_from_followUpTable(

        list = species_to_get_from_infoSpecies,
        ask_to_open_file = F

      )

    }


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
          dplyr::filter(FFB_citation_short == "") %>%
          dplyr::select(NameFB_semAutor)

      ) > 0

    if(have_species_without_FFBcitation_in_followUpTable_in_cloud == T){

      ### Yes ####

      species_without_FFBcitation_in_followUpTable_in_cloud <-
        Acomp_spp_infoSpeciesTable %>%
        dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable) %>%
        dplyr::filter(FFB_citation_short == "") %>%
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

    if(T %in% (followUpTable$zcitationFB2020 == "")){

      have_species_without_FFBcitation_in_followUpTable_in_localComputer <-
        T

    } else {

      have_species_without_FFBcitation_in_followUpTable_in_localComputer <-
        F

    }

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

      species_not_found_in_FFBcitations <- FFBcitations$species[which(FFBcitations$zcitationFB == "")]

      species_not_found_in_FFBcitations <-
        Acomp_spp_infoSpeciesTable %>%
        dplyr::filter(NameFB_semAutor %in% species_not_found_in_FFBcitations) %>%
        dplyr::filter(FFB_citation_short != "coletar") %>%
        dplyr::select(NameFB_semAutor) |> as.character()


      if(length(species_not_found_in_FFBcitations) > 0 &
         species_not_found_in_FFBcitations != "character(0)"){

        species_not_found_in_FFBcitations_table <- data.frame(

          i = 1:length(species_not_found_in_FFBcitations),
          Species = species_not_found_in_FFBcitations

        )


        ### Discover the cell in infoSpecies_table ####

        species_not_found_in_FFBcitations_table$Species <- paste0(

          '<a href="',
          ss_infoSpeciesTable_URL,
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


      ## Update data from cloud? ####

      update_FFBcitations <- Acomp_spp_infoSpeciesTable %>%
        dplyr::filter(NameFB_semAutor %in% FFBcitations$species) %>%
        dplyr::filter(FFB_citation_short != "coletar" & FFB_citation_long != "")


      ### Yes ####

      if(nrow(update_FFBcitations) > 0){

        for(update_species in update_FFBcitations$NameFB_semAutor){

          update_species_i <- grep(update_species, FFBcitations$species)

          FFBcitations$zcitationFB_short[update_species_i] <-
            update_FFBcitations %>%
            dplyr::filter(NameFB_semAutor == update_species) %>%
            dplyr::select(FFB_citation_short) |> as.character()

          FFBcitations$zcitationFB[update_species_i] <-
            update_FFBcitations %>%
            dplyr::filter(NameFB_semAutor == update_species) %>%
            dplyr::select(FFB_citation_long) |> as.character()

        }

      }

      ## Fill the follow-up table in local computer ####

      if(length(FFBcitations$species) > 0 &
         nrow(update_FFBcitations) > 0){

        update_followUpTable <- followUpTable %>%
          dplyr::filter(NameFB_semAutor %in% FFBcitations$species) %>%
          dplyr::select(NameFB_semAutor, zcitationFB2020, zcitationFB2020_short)

        colnames(update_followUpTable) <- c(

          "species",
          "zcitationFB",
          "zcitationFB_short"

        )


        if(identical(FFBcitations, update_followUpTable) == F){

          update_followUpTable <- setdiff(FFBcitations, update_followUpTable)

          for(update_species in update_followUpTable$species){

            update_species_i <- grep(update_species, update_followUpTable)

            followUpTable[followUpTable$NameFB_semAutor == update_species]$zcitationFB2020_short <-
              update_followUpTable[update_followUpTable$species == update_species]$zcitationFB_short

            followUpTable[followUpTable$NameFB_semAutor == update_species]$zcitationFB2020 <-
              update_followUpTable[update_followUpTable$species == update_species]$zcitationFB

          }


          write.csv2(

            update_followUpTable,
            paste0(

              sub("Packages/CNCFloraR", "", getwd()),
              "/CNCFlora_data/outputs/citationsFloraFungaBrasil/citationsFloraFungaBrasil.csv"

            ),
            row.names = F,
            fileEncoding = "UTF-8"

          )

        }


        fill_followUpTable_with_citations_from_FloraFungaBrasil(

          list = update_followUpTable$species

        )


        ## Fill the follow-up table in cloud ####

        ### get infoSpecies Table ####

        ss_infoSpeciesTable <- gs4_get(ss_infoSpeciesTable_URL)

        Acomp_spp_infoSpeciesTable <- get_sheet_Acomp_spp_from_infoSpeciesTable_in_cloud()

        if(TRUE %in% FFBcitations$zcitationFB_short == ""){

          FFBcitations[FFBcitations$zcitationFB_short == "",]$zcitationFB_short <-
            "coletar"
        }


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

    cli_h1("Step 5: Species without obra princeps in the followUpTable in local computer")

    ## Get sheet List_for_HTML_profile from the follow-up table in GoogleSheets ####

    cli_h2("Reading sheet List_for_HTML_profile from followUpTable in cloud")

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()

    species_in_List_for_HTML_profile_followUpTable <-
      List_for_HTML_profile_followUpTable$Espécie[is.na(List_for_HTML_profile_followUpTable$Espécie) == F]


    ## Get sheet Acomp_spp from the infoSpecies table in GoogleSheets ####

    cli_h2("Checking and getting species without obra princeps in the followUpTable in cloud")

    Acomp_spp_infoSpeciesTable <- get_sheet_Acomp_spp_from_infoSpeciesTable_in_cloud()

    Acomp_spp_infoSpeciesTable$obraPrinceps[is.na(Acomp_spp_infoSpeciesTable$obraPrinceps) == T] <-
      ""

    ## Have species without obra princeps in the followUpTable in cloud? ####

    have_species_without_obraPrinceps_in_followUpTable_in_cloud <-
      nrow(

        Acomp_spp_infoSpeciesTable %>%
          dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable) %>%
          dplyr::filter(obraPrinceps == "") %>%
          dplyr::select(NameFB_semAutor)

      ) > 0

    if(have_species_without_obraPrinceps_in_followUpTable_in_cloud == T){

      ### Yes ####

      species_without_obraPrinceps_in_followUpTable_in_cloud <-
        Acomp_spp_infoSpeciesTable %>%
        dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable) %>%
        dplyr::filter(obraPrinceps == "") %>%
        dplyr::select(NameFB_semAutor)

      species_without_obraPrinceps_in_followUpTable_in_cloud <-
        species_without_obraPrinceps_in_followUpTable_in_cloud$NameFB_semAutor

    }


    ## Get follow-up table from local computer ####

    cli_h2("Get followUpTable in local computer")

    followUpTable <- get_followUpTable_from_localComputer()


    ## Update follow-up table in local computer from cloud ####

    df_Acomp_spp_infoSpeciesTable <- Acomp_spp_infoSpeciesTable %>%
      dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable) %>%
      dplyr::select(NameFB_semAutor, obraPrinceps)

    colnames(df_Acomp_spp_infoSpeciesTable) <- c("NameFB_semAutor", "obraPrinceps_cloud")

    df_followUpTable <- followUpTable %>%
      dplyr::filter(NameFB_semAutor %in% species_in_List_for_HTML_profile_followUpTable) %>%
      dplyr::select(NameFB_semAutor, zobra)

    colnames(df_followUpTable) <- c("NameFB_semAutor", "obraPrinceps_local_computer")

    df_check <- left_join(df_Acomp_spp_infoSpeciesTable, df_followUpTable, by = "NameFB_semAutor")

    if(

      all(

        df_check$obraPrinceps_local_computer == "" &
        df_check$obraPrinceps_cloud != "" &
        df_check$obraPrinceps_cloud != "revisar"

      ) == T

    ){

      species_in_df_check <- df_check$NameFB_semAutor[

        df_check$obraPrinceps_local_computer == "" &
          df_check$obraPrinceps_cloud != "" &
          df_check$obraPrinceps_cloud != "coletar"

      ]

      followUpTable$zobra[

        followUpTable$NameFB_semAutor %in% species_in_df_check

      ] <- df_check$obraPrinceps_cloud[

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


    ## Have species without obra princeps in the followUpTable in local computer? ####

    if(T %in% (followUpTable$zobra == "")){

      have_species_without_obraPrinceps_in_followUpTable_in_localComputer <-
        T

    } else {

      have_species_without_obraPrinceps_in_followUpTable_in_localComputer <-
        F

    }


    if(have_species_without_obraPrinceps_in_followUpTable_in_localComputer == T){

      ### Yes ####

      species_without_obraPrinceps_in_followUpTable_in_localComputer <-
        followUpTable$NameFB_semAutor[

          followUpTable$zobra == ""

        ]

      #### Include species without obra princeps in the followUpTable in cloud ####

      if(exists("species_without_obraPrinceps_in_followUpTable_in_cloud") == T){

        species_without_obraPrinceps_in_followUpTable_in_localComputer <- c(

          species_without_obraPrinceps_in_followUpTable_in_cloud,
          species_without_obraPrinceps_in_followUpTable_in_localComputer

        )

      }

      if(exists("species_without_obraPrinceps_in_followUpTable_in_cloud") == T){

        species_without_obraPrinceps_in_followUpTable_in_localComputer <-
          species_without_obraPrinceps_in_followUpTable_in_cloud[

            duplicated(species_without_obraPrinceps_in_followUpTable_in_cloud) == F

          ]

        species_without_obraPrinceps_in_followUpTable_in_localComputer <-
          Acomp_spp_infoSpeciesTable %>%
          dplyr::filter(NameFB_semAutor %in% species_without_obraPrinceps_in_followUpTable_in_localComputer) %>%
          dplyr::summarise(NameFB_semAutor, Author)

        colnames(species_without_obraPrinceps_in_followUpTable_in_localComputer) <-
          c("NameFB_semAutor", "Author")

      }


      ## Get obra princeps in Tropicos and IPNI ####

      if(exists("species_without_obraPrinceps_in_followUpTable_in_cloud") == T){

        cli_h2("Get obra princeps in Tropicos and IPNI")

        obraPrinceps <- get_obraPrinceps_from_Tropicos_IPNI(

          list = species_without_obraPrinceps_in_followUpTable_in_localComputer,
          ask_to_open_file = F,
          ask_to_open_spreadsheet_editor = F

        )

      }

      ## Fill the follow-up table in cloud ####

      if(exists("species_without_obraPrinceps_in_followUpTable_in_cloud") == T){

        ### get infoSpecies Table ####

        ss_infoSpeciesTable <- gs4_get(ss_infoSpeciesTable_URL)

        obraPrinceps_infoSpeciesTable <- get_sheet_obraPrinceps_from_infoSpeciesTable_in_cloud()

        obraPrinceps <- obraPrinceps[

          unique(obraPrinceps$Taxon) %notin% obraPrinceps_infoSpeciesTable$Taxon,

        ]

        obraPrinceps <- data.frame(

          Taxon = obraPrinceps$Taxon,
          Author = obraPrinceps$Author,
          Author_Tropicos = obraPrinceps$TaxonAuthor_Tropicos,
          Status_Tropicos = obraPrinceps$Status_Tropicos,
          Author_IPNI = obraPrinceps$Author_IPNI,
          Status_IPNI = obraPrinceps$Status_IPNI,
          ObraPrinceps_Tropicos = obraPrinceps$ObraPrinceps_Tropicos,
          ObraPrinceps_IPNI = obraPrinceps$ObraPrinceps_IPNI,
          Status = obraPrinceps$Status

        )

        obraPrinceps <- obraPrinceps[-1, ]

        ### Discover last filled cell ####

        lastFilledCell <- nrow(obraPrinceps_infoSpeciesTable)

        ### fill infoSpecies Table ####

        range_write(

          data = as.data.frame(obraPrinceps),
          range = as.character(

            paste0(

              "A",
              lastFilledCell + 2

            )

          ),
          ss = ss_infoSpeciesTable,
          sheet = which(ss_infoSpeciesTable$sheets$name == "obrasPrinceps"),
          col_names = F

        )

      }

      if(exists("species_without_obraPrinceps_in_followUpTable_in_cloud") == T){

        species_without_obraPrinceps_in_followUpTable_in_cloud_table <- data.frame(

          i = 1:length(species_without_obraPrinceps_in_followUpTable_in_cloud),
          Species = species_without_obraPrinceps_in_followUpTable_in_cloud

        )

        html_list(

          species_without_obraPrinceps_in_followUpTable_in_cloud_table,
          vars = names(species_without_obraPrinceps_in_followUpTable_in_cloud_table),
          title = "Species to get obra princeps",
          out = "out3_species_without_obraPrinceps.html",
          show = F

        )

      } else {

        revise_Species <- Acomp_spp_infoSpeciesTable$NameFB_semAutor[Acomp_spp_infoSpeciesTable$obraPrinceps == "revisar"]

        if(

          length(revise_Species) > 0

        ){

          species_without_obraPrinceps_in_followUpTable_in_cloud_table <- data.frame(

            i = 1:length(revise_Species),
            Species = revise_Species

          )

          html_list(

            species_without_obraPrinceps_in_followUpTable_in_cloud_table,
            vars = names(species_without_obraPrinceps_in_followUpTable_in_cloud_table),
            title = "Species to revise obra princeps",
            out = "out3_species_without_obraPrinceps.html",
            show = F

          )

        }

      }

    }


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

          df["occurrenceRecords",] == T

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

          df["occurrenceRecords",] == T

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


    # Step 9: Intersect PANS, TERs and UCs ####

    cli_h1("Step 9: Intersect PANS, TERs and UCs")

    cli_h2("Checking species in the flow")

    listOfSpecies_intersect_PANs_TERs_UCs <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["intersectPANs",] == F &
            df["intersectTERs",] == F &
            df["intersectUCs",] == F

        )

      ]

    )


    ## Prepare list of species file ####

    prepare_listOfSpecies_files_to_intersectPANsTERsUCs(

      onlyNonExistentFile = T,
      ask_to_write_file = F

    )


    ## Remove species on development or error ####

    listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error <-
      listOfSpecies_intersect_PANs_TERs_UCs

    listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error_ <- NULL
    for(species in listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error){

      listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error.this <-
        data.frame(

          Species = species,
          exists = if(

            file.exists(

              paste0(

                sub("Packages/CNCFloraR", "", getwd()),
                "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs logs/",
                species,
                ".csv"

              )

            )

          ){T} else {F}

        )

      listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error_ <-
        rbind(

          listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error_,
          listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error.this

        )

    }

    listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error <-
      listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error_$Species[

        listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error_$exists == T

      ]

    intersect_PANs_TERs_UCs_on_development_or_error <- NULL
    for(species in listOfSpecies_intersect_PANs_TERs_UCs_on_development_or_error){

      intersect_PANs_TERs_UCs_on_development_or_error_ <- fread(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/outputs/intersect_PANs_TERs_UCs logs/",
          species,
          ".csv"

        )

      )

      intersect_PANs_TERs_UCs_on_development_or_error <-
        rbind (intersect_PANs_TERs_UCs_on_development_or_error, intersect_PANs_TERs_UCs_on_development_or_error_)

    }


    if(is.null(intersect_PANs_TERs_UCs_on_development_or_error) == T){

      intersect_PANs_TERs_UCs_on_development <- ""
      intersect_PANs_TERs_UCs_on_error <- ""

    } else {

      intersect_PANs_TERs_UCs_on_development <-
        intersect_PANs_TERs_UCs_on_development_or_error$species[

          is.na(intersect_PANs_TERs_UCs_on_development_or_error$end) == T

        ]

      intersect_PANs_TERs_UCs_on_error <-
        intersect_PANs_TERs_UCs_on_development_or_error %>%
        dplyr::filter(end == "error") %>%
        dplyr::select(species)

      intersect_PANs_TERs_UCs_on_error <- intersect_PANs_TERs_UCs_on_error$species

    }


    ## listOfSpecies to proceed ####

    listOfSpecies_intersect_PANs_TERs_UCs <-
      setdiff(listOfSpecies_intersect_PANs_TERs_UCs, intersect_PANs_TERs_UCs_on_development)

    listOfSpecies_intersect_PANs_TERs_UCs <-
      setdiff(listOfSpecies_intersect_PANs_TERs_UCs, intersect_PANs_TERs_UCs_on_error)


    ## Start analysis ####

    if(length(listOfSpecies_intersect_PANs_TERs_UCs) > 0){

      cli_h2("Conduct analysis for a list of species")

      intersect_PANs_TERs_UCs_create_scripts(

        list = listOfSpecies_intersect_PANs_TERs_UCs,
        ask_to_open_file = F

      )

      cli_h2("")

      intersect_PANs_TERs_UCs_execute_scripts(

        list = listOfSpecies_intersect_PANs_TERs_UCs

      )

      print(listOfSpecies_intersect_PANs_TERs_UCs)

    } else {

      cli_alert_success("No species to analyze")

    }

    ## Have analysis with errors? ####

    if(

      length(intersect_PANs_TERs_UCs_on_error) > 0 &
      intersect_PANs_TERs_UCs_on_error[1] != ""

    ){

      ### Yes ####

      intersect_PANs_TERs_UCs_on_error_table <- data.frame(

        i = 1:length(intersect_PANs_TERs_UCs_on_error),
        Species = intersect_PANs_TERs_UCs_on_error

      )


      #### Table: Intersect analysis (PANs_TERs_UCs) with errors ####

      html_list(

        intersect_PANs_TERs_UCs_on_error_table,
        vars = names(intersect_PANs_TERs_UCs_on_error_table),
        title = "Errors in intersect analysis - PANs_TERs_UCs",
        out = "out8_intersect_analyses_errors.html",
        show = F

      )

    }


    # Step 10: Overlay analysis between occurrence records and MapBiomas Land Cover 1985-2020 ####

    cli_h1("Step 10: Overlay analysis between occurrence records and MapBiomas Land Cover 1985-2020")

    cli_h2("Checking species in the flow")

    listOfSpecies_overlayAnalysis <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["overlayMapBiomasTodosOsAnos",] == F

        )

      ]

    )

    ## Get sheet List_for_HTML_profile from the follow-up table in GoogleSheets ####

    cli_h2("Checking and getting data from follow-up table in cloud")

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()

    ## Remove PNA species ####

    List_for_HTML_profile_followUpTable_PNA <- List_for_HTML_profile_followUpTable %>%
      dplyr::filter(`PA/PNA` == "PNA") %>%
      dplyr::select(Espécie)

    listOfSpecies_overlayAnalysis <-
      setdiff(

        listOfSpecies_overlayAnalysis,
        List_for_HTML_profile_followUpTable_PNA$Espécie

      )

    ## Remove species on development or error ####

    listOfSpecies_overlayAnalysis_on_development_or_error <-
      listOfSpecies_overlayAnalysis

    listOfSpecies_overlayAnalysis_on_development_or_error_ <- NULL
    for(species in listOfSpecies_overlayAnalysis_on_development_or_error){

      listOfSpecies_overlayAnalysis_on_development_or_error.this <-
        data.frame(

          Species = species,
          exists = if(

            file.exists(

              paste0(

                sub("Packages/CNCFloraR", "", getwd()),
                "/CNCFlora_data/outputs/overlayAnalysis logs/",
                species,
                ".csv"

              )

            )

          ){T} else {F}

        )

      listOfSpecies_overlayAnalysis_on_development_or_error_ <-
        rbind(

          listOfSpecies_overlayAnalysis_on_development_or_error_,
          listOfSpecies_overlayAnalysis_on_development_or_error.this

        )

    }

    listOfSpecies_overlayAnalysis_on_development_or_error <-
      listOfSpecies_overlayAnalysis_on_development_or_error_$Species[

        listOfSpecies_overlayAnalysis_on_development_or_error_$exists == T

      ]

    overlayAnalysis_on_development_or_error <- NULL
    for(species in listOfSpecies_overlayAnalysis_on_development_or_error){

      overlayAnalysis_on_development_or_error_ <- fread(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/outputs/overlayAnalysis logs/",
          species,
          ".csv"

        )

      )

      overlayAnalysis_on_development_or_error <-
        rbind (overlayAnalysis_on_development_or_error, overlayAnalysis_on_development_or_error_)

    }

    if(is.null(overlayAnalysis_on_development_or_error) == T){

      overlayAnalysis_on_development <- ""
      overlayAnalysis_on_error <- ""

    } else {

      overlayAnalysis_on_development <-
        overlayAnalysis_on_development_or_error$species[

          is.na(overlayAnalysis_on_development_or_error$end) == T

        ]

      overlayAnalysis_on_error <-
        overlayAnalysis_on_development_or_error %>%
        dplyr::filter(end == "error") %>%
        dplyr::select(species)

      overlayAnalysis_on_error <- overlayAnalysis_on_error$species

    }


    ## listOfSpecies to proceed ####

    listOfSpecies_overlayAnalysis <-
      setdiff(listOfSpecies_overlayAnalysis, overlayAnalysis_on_development)

    listOfSpecies_overlayAnalysis <-
      setdiff(listOfSpecies_overlayAnalysis, overlayAnalysis_on_error)


    ## Start analysis ####

    if(length(listOfSpecies_overlayAnalysis) > 0){

      cli_h2("Conduct analysis for a list of species")

      overlayAnalysis_create_scripts(

        list = listOfSpecies_overlayAnalysis,
        ask_to_open_file = F

      )

      overlayAnalysis_execute_scripts(

        list = listOfSpecies_overlayAnalysis

      )

      print(listOfSpecies_overlayAnalysis)

    } else {

      cli_alert_success("No species to analyze")

    }


    ## Have analysis with errors? ####

    if(length(overlayAnalysis_on_error) > 0){

      ### Yes ####

      overlayAnalysis_on_error_table <- data.frame(

        i = 1:length(overlayAnalysis_on_error),
        Species = overlayAnalysis_on_error

      )


      #### Table: Overlay analysis with errors ####

      html_list(

        overlayAnalysis_on_error_table,
        vars = names(overlayAnalysis_on_error_table),
        title = "Errors in overlay analysis",
        out = "out7_overlay_analyses_errors.html",
        show = F

      )

    }


    # Step 11: Overlay analysis between occurrence records and MapBiomas Fire Land Cover 2020 ####

    cli_h1("Step 11: Overlay analysis between occurrence records and MapBiomas Fire Land Cover 2020")

    cli_h2("Checking species in the flow")

    listOfSpecies_overlayAnalysis_Fire <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["overlayMapBiomasFire",] == F

        )

      ]

    )


    ## Get sheet List_for_HTML_profile from the follow-up table in GoogleSheets ####

    cli_h2("Checking and getting data from follow-up table in cloud")

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()


    ## Remove PNA species ####

    List_for_HTML_profile_followUpTable_PNA <- List_for_HTML_profile_followUpTable %>%
      dplyr::filter(`PA/PNA` == "PNA") %>%
      dplyr::select(Espécie)

    listOfSpecies_overlayAnalysis_Fire <-
      setdiff(

        listOfSpecies_overlayAnalysis_Fire,
        List_for_HTML_profile_followUpTable_PNA$Espécie

      )


    ## Remove species on development or error ####

    listOfSpecies_overlayAnalysis_Fire_on_development_or_error <-
      listOfSpecies_overlayAnalysis_Fire

    listOfSpecies_overlayAnalysis_Fire_on_development_or_error_ <- NULL
    for(species in listOfSpecies_overlayAnalysis_Fire_on_development_or_error){

      listOfSpecies_overlayAnalysis_Fire_on_development_or_error.this <-
        data.frame(

          Species = species,
          exists = if(

            file.exists(

              paste0(

                sub("Packages/CNCFloraR", "", getwd()),
                "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire logs/",
                species,
                ".csv"

              )

            )

          ){T} else {F}

        )

      listOfSpecies_overlayAnalysis_Fire_on_development_or_error_ <-
        rbind(

          listOfSpecies_overlayAnalysis_Fire_on_development_or_error_,
          listOfSpecies_overlayAnalysis_Fire_on_development_or_error.this

        )

    }

    listOfSpecies_overlayAnalysis_Fire_on_development_or_error <-
      listOfSpecies_overlayAnalysis_Fire_on_development_or_error_$Species[

        listOfSpecies_overlayAnalysis_Fire_on_development_or_error_$exists == T

      ]

    overlayAnalysis_Fire_on_development_or_error <- NULL
    for(species in listOfSpecies_overlayAnalysis_Fire_on_development_or_error){

      overlayAnalysis_Fire_on_development_or_error_ <- fread(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire logs/",
          species,
          ".csv"

        )

      )

      overlayAnalysis_Fire_on_development_or_error <-
        rbind (overlayAnalysis_Fire_on_development_or_error, overlayAnalysis_Fire_on_development_or_error_)

    }

    if(is.null(overlayAnalysis_Fire_on_development_or_error) == T){

      overlayAnalysis_Fire_on_development <- ""
      overlayAnalysis_Fire_on_error <- ""

    } else {

      overlayAnalysis_Fire_on_development <-
        overlayAnalysis_Fire_on_development_or_error$species[is.na(overlayAnalysis_Fire_on_development_or_error$end) == T]

      overlayAnalysis_Fire_on_error <-
        overlayAnalysis_Fire_on_development_or_error %>%
        dplyr::filter(end == "error") %>%
        dplyr::select(species)

      overlayAnalysis_Fire_on_error <- overlayAnalysis_Fire_on_error$species

    }


    ## listOfSpecies to proceed ####

    listOfSpecies_overlayAnalysis_Fire <-
      setdiff(listOfSpecies_overlayAnalysis_Fire, overlayAnalysis_Fire_on_development)

    listOfSpecies_overlayAnalysis_Fire <-
      setdiff(listOfSpecies_overlayAnalysis_Fire, overlayAnalysis_Fire_on_error)


    ## Start analysis ####

    if(length(listOfSpecies_overlayAnalysis_Fire) > 0){

      cli_h2("Conduct analysis for a list of species")

      overlayAnalysis_Fire_create_scripts(

        list = listOfSpecies_overlayAnalysis_Fire,
        ask_to_open_file = F

      )

      cli_h2("")

      for(species_overlayAnalysis_Fire in listOfSpecies_overlayAnalysis_Fire){

        if(

          file.exists(

            paste0(

              sub("Packages/CNCFloraR", "", getwd()),
              "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire logs/",
              species_overlayAnalysis_Fire,
              ".csv"

            )

          ) == T

        ){

          backgroundJobs_log <- data.table::fread(

            paste0(

              sub("Packages/CNCFloraR", "", getwd()),
              "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire logs/",
              species_overlayAnalysis_Fire,
              ".csv"

            )

          )

        } else {

          backgroundJobs_log <- data.frame(

            job = "Overlay analysis - Fire",
            species = species_overlayAnalysis_Fire,
            start = format(Sys.Date(), "%d/%m/%Y"),
            end = ""

          )

          write.csv2(

            backgroundJobs_log,
            paste0(

              sub("Packages/CNCFloraR", "", getwd()),
              "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire logs/",
              species_overlayAnalysis_Fire,
              ".csv"

            ),
            row.names = F

          )

        }

        if(backgroundJobs_log$end == ""){

          overlayAnalysis_Fire_execute_scripts(

            list = species_overlayAnalysis_Fire

          )

        }

        while(backgroundJobs_log$end == ""){

          backgroundJobs_log <- data.table::fread(

            paste0(

              sub("Packages/CNCFloraR", "", getwd()),
              "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire logs/",
              species_overlayAnalysis_Fire,
              ".csv"

            )

          )

          Sys.sleep(3)

          if(is.na(backgroundJobs_log$end) == T){

            backgroundJobs_log$end <- ""

          }

          if(backgroundJobs_log$end != ""){

            break

          }

        }

      }

    } else {

      cli_alert_success("No species to analyze")

    }

    ## Have analysis with errors? ####

    if(

      length(overlayAnalysis_Fire_on_error) > 0 &
      overlayAnalysis_Fire_on_error != ""

    ){

      ### Yes ####

      overlayAnalysis_Fire_on_error_table <- data.frame(

        i = 1:length(overlayAnalysis_Fire_on_error),
        Species = overlayAnalysis_Fire_on_error

      )


      #### Table: Overlay analysis (MapBiomas Fire 2020) with errors ####

      html_list(

        overlayAnalysis_Fire_on_error_table,
        vars = names(overlayAnalysis_Fire_on_error_table),
        title = "Errors in overlay analysis - MapBiomas Fire 2020",
        out = "out9_overlay_analyses_fire_errors.html",
        show = F

      )

    }


    # Step 12: Create the species profile HTML ####

    cli_h1("Step 12: Create the species profile HTML")

    cli_h2("Checking species in the flow")

    listOfSpecies_profileOfSpeciesHTML <- colnames(

      df[

        which(

          df["occurrenceRecords",] == T &
            df["intersectPANs",] == T &
            df["intersectTERs",] == T &
            df["intersectUCs",] == T &
            df["overlayMapBiomasTodosOsAnos",] == T &
            df["overlayMapBiomasFire",] == T


        )

      ]

    )

    ## Prepare list of species file ####

    prepare_listOfSpecies_files_to_build_profileOfSpeciesHTMLs(

      onlyNonExistentProfile = T,
      ask_to_write_file = F

    )


    ## Get sheet Acomp_spp from the infoSpeciesTable in GoogleSheets ####

    cli_h2("Reading sheet Acomp_spp from infoSpeciesTable")

    Acomp_spp_infoSpeciesTable <-
      get_sheet_Acomp_spp_from_infoSpeciesTable_in_cloud()


    ## Check obra princeps and FFB citation in Acomp_spp from the infoSpeciesTable in GoogleSheets ####

    check_obraPrinceps_FFBcitation_of_listOfSpecies_profileOfSpeciesHTML <-
      Acomp_spp_infoSpeciesTable %>%
      dplyr::filter(NameFB_semAutor %in% listOfSpecies_profileOfSpeciesHTML) %>%
      dplyr::select(NameFB_semAutor, obraPrinceps, FFB_citation_short, FFB_citation_long)


    ## Remove species without obra princeps and/or FFB citation in Acomp_spp from the infoSpeciesTable in GoogleSheets ####

    listOfSpecies_profileOfSpeciesHTML <-
      check_obraPrinceps_FFBcitation_of_listOfSpecies_profileOfSpeciesHTML$NameFB_semAutor[

        (

          check_obraPrinceps_FFBcitation_of_listOfSpecies_profileOfSpeciesHTML$obraPrinceps == "revisar" |
            is.na(check_obraPrinceps_FFBcitation_of_listOfSpecies_profileOfSpeciesHTML$FFB_citation_short) |
            is.na(check_obraPrinceps_FFBcitation_of_listOfSpecies_profileOfSpeciesHTML$FFB_citation_long)

        ) == F

      ]


    ## Get sheet List_for_HTML_profile from the follow-up table in GoogleSheets ####

    cli_h2("Checking and getting data from follow-up table in cloud")

    List_for_HTML_profile_followUpTable <-
      get_sheet_List_for_HTML_profile_from_followUpTable_in_cloud()


    ## Remove PNA species ####

    List_for_HTML_profile_followUpTable_PNA <- List_for_HTML_profile_followUpTable %>%
      dplyr::filter(`PA/PNA` == "PNA") %>%
      dplyr::select(Espécie)

    listOfSpecies_profileOfSpeciesHTML <-
      setdiff(

        listOfSpecies_profileOfSpeciesHTML,
        List_for_HTML_profile_followUpTable_PNA$Espécie

      )


    ## Remove species on development or error ####

    listOfSpecies_profileOfSpeciesHTML_on_development_or_error <-
      listOfSpecies_profileOfSpeciesHTML

    listOfSpecies_profileOfSpeciesHTML_on_development_or_error_ <- NULL
    for(species in listOfSpecies_profileOfSpeciesHTML_on_development_or_error){

      listOfSpecies_profileOfSpeciesHTML_on_development_or_error.this <-
        data.frame(

          Species = species,
          exists = if(

            file.exists(

              paste0(

                sub("Packages/CNCFloraR", "", getwd()),
                "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire logs/",
                species,
                ".csv"

              )

            )

          ){T} else {F}

        )

      listOfSpecies_profileOfSpeciesHTML_on_development_or_error_ <-
        rbind(

          listOfSpecies_profileOfSpeciesHTML_on_development_or_error_,
          listOfSpecies_profileOfSpeciesHTML_on_development_or_error.this

        )

    }

    listOfSpecies_profileOfSpeciesHTML_on_development_or_error <-
      listOfSpecies_profileOfSpeciesHTML_on_development_or_error_$Species[

        listOfSpecies_profileOfSpeciesHTML_on_development_or_error_$exists == T

      ]

    profileOfSpeciesHTML_on_development_or_error <- NULL
    for(species in listOfSpecies_profileOfSpeciesHTML_on_development_or_error){

      profileOfSpeciesHTML_on_development_or_error_ <- fread(

        paste0(

          sub("Packages/CNCFloraR", "", getwd()),
          "/CNCFlora_data/outputs/overlayAnalysis MapBiomasFire logs/",
          species,
          ".csv"

        )

      )

      profileOfSpeciesHTML_on_development_or_error <-
        rbind (profileOfSpeciesHTML_on_development_or_error, profileOfSpeciesHTML_on_development_or_error_)

    }

    if(is.null(profileOfSpeciesHTML_on_development_or_error) == T){

      profileOfSpeciesHTML_on_development <- ""
      profileOfSpeciesHTML_on_error <- ""

    } else {

      profileOfSpeciesHTML_on_development <-
        profileOfSpeciesHTML_on_development_or_error$species[is.na(profileOfSpeciesHTML_on_development_or_error$end) == T]

      profileOfSpeciesHTML_on_error <-
        profileOfSpeciesHTML_on_development_or_error %>%
        dplyr::filter(end == "error") %>%
        dplyr::select(species)

      profileOfSpeciesHTML_on_error <- profileOfSpeciesHTML_on_error$species

    }


    ## listOfSpecies to proceed ####

    listOfSpecies_profileOfSpeciesHTML <-
      setdiff(listOfSpecies_profileOfSpeciesHTML, profileOfSpeciesHTML_on_development)

    listOfSpecies_profileOfSpeciesHTML <-
      setdiff(listOfSpecies_profileOfSpeciesHTML, profileOfSpeciesHTML_on_error)


    ## Start creation ####

    if(length(listOfSpecies_profileOfSpeciesHTML) > 0){

      cli_h2("Conduct creation of profile of species HTML for a list of species")

      create_profileOfSpeciesHTML_create_scripts(

        list = listOfSpecies_profileOfSpeciesHTML,
        ask_to_open_file = F,
        flow = "PT"

      )

      create_profileOfSpeciesHTML_create_scripts(

        list = listOfSpecies_profileOfSpeciesHTML,
        ask_to_open_file = F,
        flow = "PNT"

      )

      cli_h2("")

      create_profileOfSpeciesHTML_execute_scripts(

        list = listOfSpecies_profileOfSpeciesHTML,
        flow = "PT"

      )

      create_profileOfSpeciesHTML_execute_scripts(

        list = listOfSpecies_profileOfSpeciesHTML,
        flow = "PNT"

      )

      print(listOfSpecies_profileOfSpeciesHTML)

    } else {

      cli_alert_success("No species to create")

    }

    ## Have creation with errors? ####

    if(length(profileOfSpeciesHTML_on_error) > 0){

      ### Yes ####

      profileOfSpeciesHTML_on_error_table <- data.frame(

        i = 1:length(profileOfSpeciesHTML_on_error),
        Species = profileOfSpeciesHTML_on_error

      )


      #### Table: Profile of species HTMLs with errors ####

      html_list(

        profileOfSpeciesHTML_on_error_table,
        vars = names(profileOfSpeciesHTML_on_error_table),
        title = "Errors in creation of profile of species HTMLs",
        out = "out10_profileOfSpeciesHTML.html",
        show = F

      )

    }

    # Final step: Generate final report (report.html) ####

    html_combine(

      out = "report.hta",
      rtitle = paste0("Report: ", format(Sys.time(), "%d/%m/%Y - %H:%M:%S")),
      toctheme = TRUE,
      template = paste0(system.file(package = "R3port"), "/bootstrap_for_HTA.html")

    )


    # Step 13: Get filled profiles of species from old system ####

    cli_h1("Step 13: Get filled profiles of species from old system")
    cli_h2("Checking and getting filled profiles of species")

    listOfSpecies_filledProfile <- colnames(

      df[

        which(

          df["HTMLprofile",] == T &
            df["filledProfile_from_oldSystem",] == F

        )

      ]

    )

    if(length(listOfSpecies_getFilledProfile) > 0){

      cli_alert_info("Including species")

      species_to_get_filledProfiles <- prepare_listOfSpecies_files_to_get_filledProfileOfSpecies_from_oldSystem(

        ask_to_open_file = F,
        ask_to_write_file = F

      )

      AHKscript_to_download_filledProfileOfSpecies_from_oldSystem(

        species_to_get_filledProfiles,
        ask_to_open_file = F,
        ask_to_open_filePath = F

      )


      ## Table: Get filled profiles of species ####

      get_filledProfiles_table <- data.frame(

        i = 1:length(species_to_get_filledProfiles),
        Species = species_to_get_filledProfiles

      )

      html_list(

        get_filledProfiles_table,
        vars = names(get_filledProfiles_table),
        title = "Species to get filled profiles of species",
        out = "out4_get_filledProfiles.html",
        show = F,
        footnote = '
        <button type="button" onclick="runAhkScript()">Executar AHK script! Pressione F4 para disparar.</button>
        <script>
          function runAhkScript() {
            var ahkExePath = "C:/Program Files/AutoHotkey/AutoHotkey.exe";
            var scriptToExecute = "C:/R/R-4.1.1/working/CNCFlora_data/outputs/AHK_scripts/get_filledProfileOfSpecies.ahk";
            new ActiveXObject("Shell.Application").ShellExecute(ahkExePath, scriptToExecute);
          }
        </script>

        '

      )

    } else {

      cli_alert_success("No species to get filled profiles of species.")

    }


    # End of loop ####

  }


  # End of function: conduct_all_processes_loop() ####

}
