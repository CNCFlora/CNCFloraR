convert_UTMzone_to_CRSstatement <- function(coords_df_valid){

  coords_df_valid$CRS <- coords_df_valid$UTMzone
  coords_df_valid$CRS <- sub("18S", "+proj=utm +zone=18 +south +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("19S", "+proj=utm +zone=19 +south +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("20S", "+proj=utm +zone=20 +south +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("21S", "+proj=utm +zone=21 +south +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("22S", "+proj=utm +zone=22 +south +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("23S", "+proj=utm +zone=23 +south +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("24S", "+proj=utm +zone=24 +south +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("25S", "+proj=utm +zone=25 +south +datum=WGS84", coords_df_valid$CRS)

  coords_df_valid$CRS <- sub("18N", "+proj=utm +zone=18 +north +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("19N", "+proj=utm +zone=19 +north +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("20N", "+proj=utm +zone=20 +north +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("21N", "+proj=utm +zone=21 +north +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("22N", "+proj=utm +zone=22 +north +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("23N", "+proj=utm +zone=23 +north +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("24N", "+proj=utm +zone=24 +north +datum=WGS84", coords_df_valid$CRS)
  coords_df_valid$CRS <- sub("25N", "+proj=utm +zone=25 +north +datum=WGS84", coords_df_valid$CRS)

  return(coords_df_valid)

}
