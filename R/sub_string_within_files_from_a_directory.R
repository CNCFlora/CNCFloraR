sub_string_within_files_from_a_directory <- function(dir = "", ext = c("csv", "R"), encoding = c("UTF-8", "latin1"), replace = "", to = ""){
 
  if(dir == ""){
    
    stop("Directory not informed.")
    
  }
  
  if(ext == ""){
    
    stop("Extension of the files not informed.")
    
  }
  
  if(encoding == ""){
    
    stop("Encoding of the files not informed.")
    
  }
  
  if(replace == ""){
    
    stop("String to substitute not informed.")
    
  }
  
  if(dir == ""){
    
    stop("String to replace not informed.")
    
  }
  
  files <- list.files(dir, pattern = "csv", full.names = T)
  
  dir.create(dir)
  
  for(file in files){
    
    table <- read.csv(file, sep = ",", encoding = encoding)
    table.colnames <- colnames(table)
    colnames(table) <- sub(replace, to , table.colnames)
    write.table(table, file, row.names = F, sep = ",", fileEncoding = encoding)
    
  }
  
}

