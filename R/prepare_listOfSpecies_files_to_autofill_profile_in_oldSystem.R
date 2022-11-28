prepare_listOfSpecies_files_to_autofill_profile_in_oldSystem <- function(onlyNonFilledProfile = T, ask_to_open_file = T, ask_to_write_file = T){

  library(stringr)
  library(googlesheets4)

  # Load follow-up table from GoogleSheets ####

  Acomp_spp_followUpTable <- get_sheet_Acomp_spp_from_followUpTable_in_cloud()
  Acomp_spp_followUpTable.filtered <- Acomp_spp_followUpTable %>%
    dplyr::filter(

      is.na(HTMLs) == F &
        is.na(`Preenc. Autom.`) == T &
        is.na(`Perfil parcial`) == T &
        is.na(`Perfil Completo`) == T

        ) %>%
    dplyr::select(`NameFB_semAutor(textPlane)`, Recorte, `PA/PNA`)

  output <- data.frame(

    Recorte = Acomp_spp_followUpTable.filtered$Recorte,
    NameFB_semAutor = Acomp_spp_followUpTable.filtered$`NameFB_semAutor(textPlane)`,
    flow = Acomp_spp_followUpTable.filtered$`PA/PNA`

  )


  if(ask_to_write_file == T){

    answer <- ""

    while(

      answer != "Y" |
      answer != "N"

    ){

      answer <-
        toupper(readline("Write the list of species file (fill_profiles_in_oldSystem.csv)? (y/n): "))

      if(answer == "Y"){

        write.table(

          output,
          paste0(

            sub("Packages/CNCFloraR", "", getwd()),
            "/CNCFlora_data/inputs/listOfSpecies_for_processing/fill_profiles_in_oldSystem.csv"

          ),
          col.names = F,
          row.names = F,
          sep = ";"

        )

        message("File created.")

        break

      }

      if(answer == "N"){

        break

      }

    }

  }

}
