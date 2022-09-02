give_access_of_profileOfSpeciesHTMLs_to_anyone_with_the_link  <- function(){

  library(googledrive)


  df <- check_all_files_of_species()

  for(i in 1:length(df)){

    df[,i] <- str_detect(df[,i], "TRUE")

  }


  listOfSpecies <- colnames(

    df[

      which(

        df["HTMLprofile",] == T

      )

    ]

  )


  #Fornecer permissão para o domínio CNCFlora

  for(species in listOfSpecies){

    drive_get(

      id = "1B4XkGRyQJYVlYcqrbIWxBttbhaEGAoiB",
      path = paste0(

        "profileOfSpeciesHTML results/",
        species,
        ".html"

      )

    ) %>%
      drive_share(type = "anyone")

  }

}

