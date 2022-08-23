#' Get today date in extended format
#'
#' Obtém a string da data de hoje em formato por extenso

getTodayString <- function() {

  DATA<-Sys.Date()
  DATA<-as.character(DATA)
  DATA<-as.data.frame(strsplit(DATA, "-"))
  DATA<-t(DATA)
  ANO<-DATA[,1]
  MES<-DATA[,2]
  MES<-sub("01", "janeiro",MES)
  MES<-sub("02", "fevereiro",MES)
  MES<-sub("03", "março",MES)
  MES<-sub("04", "abril",MES)
  MES<-sub("05", "maio",MES)
  MES<-sub("06", "junho",MES)
  MES<-sub("07", "julho",MES)
  MES<-sub("08", "agosto",MES)
  MES<-sub("09", "setembro",MES)
  MES<-sub("10", "outubro",MES)
  MES<-sub("11", "novembro",MES)
  MES<-sub("12", "dezembro",MES)
  DIA<-DATA[,3]
  DATA<-capture.output(cat(DIA, "de", MES, "de", ANO))

  return(DATA)

}
