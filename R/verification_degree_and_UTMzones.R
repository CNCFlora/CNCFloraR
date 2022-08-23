verification_degree_and_UTMzones <- function(coords_df_valid){

  coords_df_valid$Degree_zone <- sub("\\..*", "", coords_df_valid$lon)
  coords_df_valid$Degree_zone <- sub("^$", "EMPTY", coords_df_valid$Degree_zone)

  coords_df_valid$UTMzone <- grepl("-", coords_df_valid$lat)
  coords_df_valid$UTMzone <- sub(TRUE, "S", coords_df_valid$UTMzone)
  coords_df_valid$UTMzone <- sub(FALSE, "N", coords_df_valid$UTMzone)

  coords_df_valid$Degree_zone <- sub("-", "", coords_df_valid$Degree_zone)

  EMPTY_row<-grep("EMPTY", coords_df_valid$Degree_zone)

  coords_df_valid$UTMzone[EMPTY_row]<-""
  coords_df_valid$Degree_zone[EMPTY_row]<-""

  coords_df_valid_n_row <- as.numeric(1:(length(coords_df_valid$Degree_zone)))

  for(i in coords_df_valid_n_row){

    coords_df_valid$UTMzone[i] <-
      paste(
        ceiling(
          (
            -as.numeric(coords_df_valid$Degree_zone[i]) + 180
          ) / 6
        ),
        coords_df_valid$UTMzone[i],
        sep = ""
      )

  }

  return(coords_df_valid)

}
