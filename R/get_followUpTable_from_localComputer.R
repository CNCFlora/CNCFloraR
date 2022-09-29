get_followUpTable_from_localComputer <- function(){

  followUpTable_localPath <- paste0(

    sub("Packages/CNCFloraR", "", getwd()),
    "/CNCFlora_data/inputs/follow-up_table/follow-up_table.csv"

  )

  followUpTable <- fread(

    followUpTable_localPath,
    header = T,
    sep = ";",
    encoding = "UTF-8"

  )

  return(followUpTable)

}
