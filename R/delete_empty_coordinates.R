delete_empty_coordinates <- function(coords_df_valid){

  coords_df_valid <- coords_df_valid %>% mutate_all(na_if,"")
  coords_df_valid_n_row <- nrow(coords_df_valid)

  for(i in coords_df_valid_n_row){

    if(is.na(coords_df_valid$precision[i])){

      coords_df_valid <- coords_df_valid[-i,]

    }

  }

  return(coords_df_valid)

}
