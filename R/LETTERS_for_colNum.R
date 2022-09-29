LETTERS_for_colNum <- LETTERS
for(i in LETTERS[1:10]){

  LETTERS_for_colNum_ <- paste0(i, LETTERS)
  LETTERS_for_colNum <- c(LETTERS_for_colNum, LETTERS_for_colNum_)

}
