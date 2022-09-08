store_profileIDs <- function(){

  library(data.table)
  library(editData)

  listOfprofileIDs <- fread(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/profileIDs_in_oldSystem/profileIDs.csv"

    ),
    header = F

  )

  listOfprofileIDs_txt <- fread(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/profileIDs_in_oldSystem/profileIDs.txt"

    ),
    header = F,
    sep = ";"

  )

  if(all(listOfprofileIDs_txt$V1 %in% listOfprofileIDs$V1 == F)){} else{

    listOfprofileIDs_txt_to_store <-
      listOfprofileIDs_txt[listOfprofileIDs_txt$V1 %in% listOfprofileIDs$V1 == F,]

    listOfprofileIDs <- rbind(listOfprofileIDs, listOfprofileIDs_txt_to_store)

    listOfprofileIDs <- editData(

      listOfprofileIDs,

    )

    write.table(

      listOfprofileIDs,
      paste0(

        sub("Packages/CNCFloraR", "", getwd()),
        "/CNCFlora_data/inputs/profileIDs_in_oldSystem/profileIDs.csv"

      ),
      col.names = F,
      row.names = F,
      sep = ";"

    )

  }

  file.remove(

    paste0(

      sub("Packages/CNCFloraR", "", getwd()),
      "/CNCFlora_data/inputs/profileIDs_in_oldSystem/profileIDs.txt"

    )

  )

}
