#' Generation length chart (parameter A3)
#'
#' Gera um gráfico indicador do parâmetro A3


genLengthChart <- function(){

  species <- readline("Nome da espécie: ")
  reduction30 <- as.numeric(readline("Redução em 3 gerações (comprimento da geração 30 anos): "))
  reduction50 <- as.numeric(readline("Redução em 3 gerações (comprimento da geração 50 anos): "))

  GenLength_30 <- c(0, 0, 0, 0, 0)
  GenLength_50 <- c(0, 0, 0, 0, 0)


  # Reduction 30

  if(reduction30 < 30){

    GenLength_30[2] <- reduction30

  }

  if(reduction30 > 30 &
     reduction30 < 50){

    GenLength_30[3] <- reduction30

  }

  if(reduction30 > 50 &
     reduction30 < 80){

    GenLength_30[4] <- reduction30

  }

  if(reduction30 > 80){

    GenLength_30[5] <- reduction30

  }


  # Reduction 50

  if(reduction50 < 30){

    GenLength_50[2] <- reduction50

  }

  if(reduction50 > 30 &
     reduction50 < 50){

    GenLength_50[3] <- reduction50

  }

  if(reduction50 > 50 &
     reduction50 < 80){

    GenLength_50[4] <- reduction50

  }

  if(reduction50 > 80){

    GenLength_50[5] <- reduction50

  }


  df <-
    data.frame(

      GenLength = c(0, 30, 20, 30, 20),
      GenLength_50 = GenLength_50,
      GenLength_30 = GenLength_30

    )

  tiff(paste0("genLength ", species,".tif"), width = 290)

  barplot(

    as.matrix(df),
    main = species,
    space = 0.05,
    col = c("red", "green", "yellow", "orange"),
    axes = F,
    xlab = "Redução",
    names.arg = c("Critério A3", "50 anos", "30 anos"),
    xlim = c(-0.27, 2.8)

  )

  title(ylab = "Risco de extinção", line = 1.5)

  text(30, paste0(30, '%'), adj = 3.4)
  text(50, paste0(50, '%'), adj = 3.4)
  text(80, paste0(80, '%'), adj = 3.4)

  text(15, "LC", adj = 2.1)
  text(40, "VU", adj = 2.1)
  text(65, "EN", adj = 2.1)
  text(90, "CR", adj = 2.1)

  text(
    c(
      -100,
      reduction50+4,
      reduction30+4
    ),
    format(
      c(
        paste0(reduction30, '%'),
        paste0(reduction50, '%')
      )
    ),
    adj = c(1.1, 1)
  )

  dev.off()

}
