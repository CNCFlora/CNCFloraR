getShapefileUCs_from_GoogleDrive <- function(){

  message("Downloading shapefile of UCs from Google Drive...")

  with_drive_quiet(
    drive_download(

      drive_get(id = "1jk2bq3oR8RDpNLKPT5G3GsZMP3vf5QHN"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.shx"),
      overwrite = TRUE

    )
  )

  with_drive_quiet(
    drive_download(

      drive_get(id = "1hqRPUTnjvyvPzKdPIipZIDUsC1XPFZjP"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.shp.xml"),
      overwrite = TRUE

    )
  )

  with_drive_quiet(
    drive_download(

      drive_get(id = "1v4b4sOcRWQlPofvVVH9wkdM28BOaFkne"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.shp"),
      overwrite = TRUE

    )
  )

  with_drive_quiet(
    drive_download(

      drive_get(id = "1Q32_Acu7kfO-W01exBgL90dRWSzpg-sj"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.sbx"),
      overwrite = TRUE

    )
  )

  with_drive_quiet(
    drive_download(

      drive_get(id = "1nDavQPMxzZl2xUHQnmklTx9O_ndLGE1p"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.sbn"),
      overwrite = TRUE

    )
  )

  with_drive_quiet(
    drive_download(

      drive_get(id = "1SbOOd851Pe70HQ-LQX5wzKjXmNz9Z_1E"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.prj"),
      overwrite = TRUE

    )
  )

  with_drive_quiet(
    drive_download(

      drive_get(id = "1aDg8upDI0l96SiS0KZQ_9WwaYE7E66pB"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.dbf"),
      overwrite = TRUE

    )
  )

  with_drive_quiet(
    drive_download(

      drive_get(id = "1f2MeHZYeCxKKtu7pOMIpkfzwp53I8DuT"),
      path = paste0(sub("Packages/CNCFloraR", "", getwd()), "CNCFlora_data/inputs/shapefiles/UCs/UCs.cpg"),
      overwrite = TRUE

    )
  )

  message("Download of shapefile of UCs from Google Drive successful.")

}
