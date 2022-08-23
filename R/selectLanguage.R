#' Select the output language
#'
#' Seleciona a linguagem de saída

selectLanguage <- function(language = "PT"){

  if(language == "PT"){

    outputLanguage <- "PT"
    message("Language: Português")

  }

  if(language == "ES"){

    outputLanguage <- "ES"
    message("Language: Español")

  }

  if(language == "EN"){

    outputLanguage <- "EN"
    message("Language: English")

  }

  `%notin%` <- Negate(`%in%`)

  if(language %notin% c("EN", "PT", "ES")){

    outputLanguage <- ""
    message("Language invalid. Use 'PT' for Portuguese, 'ES' for Spanish, or 'EN' for English.")

  }

}
