#' Connect Google Drive API
#'
#' Conectar ao Google Drive API via pacote googledrive

conectaGoogleDriveAPI <- function(){

  suppressMessages({
    suppressPackageStartupMessages({

      library(googledrive)

    })
  })

  drive_auth()

}
