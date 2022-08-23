#' Read the basic informations from profile of species HTML
#'
#' Ler as informações básicas do perfil das espécies


read_infoBasic_from_profileOfSpeciesHTML <- function(){

  library(XML)
  library(dplyr)
  #Apenas a primeira é diferente

  htmls<-read.csv("TextMining/htmls.csv",header=F)

  if(is.na(htmls[1,])==T){}else{
    ESPECIE<-htmls[1,]
    x<-readHTMLTable(ESPECIE)
    x1<-x[[1]]
    n_x1<-as.numeric(count(x1)+1)
    n2_x1<-as.numeric(count(x1)+2)
    x1[n_x1,]<-sub("\\s\\s\\s\\s.*","",x1[6,])
    x1[n2_x1,]<-sub(".*\\s\\s\\s\\s","",x1[6,])
    x1[n2_x1,1]<-"Ref_FB2020"
    x1<-rbind(x1[2,],x1[3,],x1[5,],x1[n_x1,],x1[n2_x1,],x1[7,],x1[8,],x1[9,],x1[10,],x1[11,],x1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x1<-data.frame(Espécie = ESPECIE, x1)
    colnames(x1)<-c("Espécie","variable","info")
  }

  if(is.na(htmls[2,])==T){}else{
    ESPECIE<-htmls[2,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[3,])==T){}else{
    ESPECIE<-htmls[3,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[4,])==T){}else{
    ESPECIE<-htmls[4,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[5,])==T){}else{
    ESPECIE<-htmls[5,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[6,])==T){}else{
    ESPECIE<-htmls[6,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[7,])==T){}else{
    ESPECIE<-htmls[7,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[8,])==T){}else{
    ESPECIE<-htmls[8,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[9,])==T){}else{
    ESPECIE<-htmls[9,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[10,])==T){}else{
    ESPECIE<-htmls[10,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[11,])==T){}else{
    ESPECIE<-htmls[11,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[12,])==T){}else{
    ESPECIE<-htmls[12,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[13,])==T){}else{
    ESPECIE<-htmls[13,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[14,])==T){}else{
    ESPECIE<-htmls[14,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[15,])==T){}else{
    ESPECIE<-htmls[15,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[16,])==T){}else{
    ESPECIE<-htmls[16,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[17,])==T){}else{
    ESPECIE<-htmls[17,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[18,])==T){}else{
    ESPECIE<-htmls[18,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[19,])==T){}else{
    ESPECIE<-htmls[19,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[20,])==T){}else{
    ESPECIE<-htmls[20,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[21,])==T){}else{
    ESPECIE<-htmls[21,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[22,])==T){}else{
    ESPECIE<-htmls[22,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[23,])==T){}else{
    ESPECIE<-htmls[23,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[24,])==T){}else{
    ESPECIE<-htmls[24,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[25,])==T){}else{
    ESPECIE<-htmls[25,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[26,])==T){}else{
    ESPECIE<-htmls[26,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[27,])==T){}else{
    ESPECIE<-htmls[27,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[28,])==T){}else{
    ESPECIE<-htmls[28,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[29,])==T){}else{
    ESPECIE<-htmls[29,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[30,])==T){}else{
    ESPECIE<-htmls[30,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[31,])==T){}else{
    ESPECIE<-htmls[31,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[32,])==T){}else{
    ESPECIE<-htmls[32,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[33,])==T){}else{
    ESPECIE<-htmls[33,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[34,])==T){}else{
    ESPECIE<-htmls[34,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[35,])==T){}else{
    ESPECIE<-htmls[35,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[36,])==T){}else{
    ESPECIE<-htmls[36,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[37,])==T){}else{
    ESPECIE<-htmls[37,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[38,])==T){}else{
    ESPECIE<-htmls[38,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[39,])==T){}else{
    ESPECIE<-htmls[39,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[40,])==T){}else{
    ESPECIE<-htmls[40,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[41,])==T){}else{
    ESPECIE<-htmls[41,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[42,])==T){}else{
    ESPECIE<-htmls[42,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[43,])==T){}else{
    ESPECIE<-htmls[43,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[44,])==T){}else{
    ESPECIE<-htmls[44,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[45,])==T){}else{
    ESPECIE<-htmls[45,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[46,])==T){}else{
    ESPECIE<-htmls[46,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[47,])==T){}else{
    ESPECIE<-htmls[47,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[48,])==T){}else{
    ESPECIE<-htmls[48,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[49,])==T){}else{
    ESPECIE<-htmls[49,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[50,])==T){}else{
    ESPECIE<-htmls[50,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[51,])==T){}else{
    ESPECIE<-htmls[51,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[52,])==T){}else{
    ESPECIE<-htmls[52,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[53,])==T){}else{
    ESPECIE<-htmls[53,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[54,])==T){}else{
    ESPECIE<-htmls[54,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[55,])==T){}else{
    ESPECIE<-htmls[55,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[56,])==T){}else{
    ESPECIE<-htmls[56,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[57,])==T){}else{
    ESPECIE<-htmls[57,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[58,])==T){}else{
    ESPECIE<-htmls[58,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[59,])==T){}else{
    ESPECIE<-htmls[59,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[60,])==T){}else{
    ESPECIE<-htmls[60,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[61,])==T){}else{
    ESPECIE<-htmls[61,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[62,])==T){}else{
    ESPECIE<-htmls[62,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[63,])==T){}else{
    ESPECIE<-htmls[63,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[64,])==T){}else{
    ESPECIE<-htmls[64,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[65,])==T){}else{
    ESPECIE<-htmls[65,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[66,])==T){}else{
    ESPECIE<-htmls[66,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[67,])==T){}else{
    ESPECIE<-htmls[67,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[68,])==T){}else{
    ESPECIE<-htmls[68,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[69,])==T){}else{
    ESPECIE<-htmls[69,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[70,])==T){}else{
    ESPECIE<-htmls[70,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[71,])==T){}else{
    ESPECIE<-htmls[71,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[72,])==T){}else{
    ESPECIE<-htmls[72,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[73,])==T){}else{
    ESPECIE<-htmls[73,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[74,])==T){}else{
    ESPECIE<-htmls[74,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[75,])==T){}else{
    ESPECIE<-htmls[75,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[76,])==T){}else{
    ESPECIE<-htmls[76,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[77,])==T){}else{
    ESPECIE<-htmls[77,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[78,])==T){}else{
    ESPECIE<-htmls[78,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[79,])==T){}else{
    ESPECIE<-htmls[79,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[80,])==T){}else{
    ESPECIE<-htmls[80,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[81,])==T){}else{
    ESPECIE<-htmls[81,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[82,])==T){}else{
    ESPECIE<-htmls[82,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[83,])==T){}else{
    ESPECIE<-htmls[83,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[84,])==T){}else{
    ESPECIE<-htmls[84,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[85,])==T){}else{
    ESPECIE<-htmls[85,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[86,])==T){}else{
    ESPECIE<-htmls[86,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[87,])==T){}else{
    ESPECIE<-htmls[87,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[88,])==T){}else{
    ESPECIE<-htmls[88,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[89,])==T){}else{
    ESPECIE<-htmls[89,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[90,])==T){}else{
    ESPECIE<-htmls[90,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[91,])==T){}else{
    ESPECIE<-htmls[91,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[92,])==T){}else{
    ESPECIE<-htmls[92,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[93,])==T){}else{
    ESPECIE<-htmls[93,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[94,])==T){}else{
    ESPECIE<-htmls[94,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[95,])==T){}else{
    ESPECIE<-htmls[95,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[96,])==T){}else{
    ESPECIE<-htmls[96,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[97,])==T){}else{
    ESPECIE<-htmls[97,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[98,])==T){}else{
    ESPECIE<-htmls[98,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[99,])==T){}else{
    ESPECIE<-htmls[99,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  if(is.na(htmls[100,])==T){}else{
    ESPECIE<-htmls[100,]
    x<-readHTMLTable(ESPECIE)
    x_1<-x[[1]]
    n_x_1<-as.numeric(count(x_1)+1)
    n2_x_1<-as.numeric(count(x_1)+2)
    x_1[n_x_1,]<-sub("\\s\\s\\s\\s.*","",x_1[6,])
    x_1[n2_x_1,]<-sub(".*\\s\\s\\s\\s","",x_1[6,])
    x_1[n2_x_1,1]<-"Ref_FB2020"
    x_1<-rbind(x_1[2,],x_1[3,],x_1[5,],x_1[n_x_1,],x_1[n2_x_1,],x_1[7,],x_1[8,],x_1[9,],x_1[10,],x_1[11,],x_1[12,])
    ESPECIE<-sub("output ", "",ESPECIE)
    ESPECIE<-sub(".html", "",ESPECIE)
    x_1<-data.frame(Espécie = ESPECIE, x_1)
    colnames(x_1)<-c("Espécie","variable","info")
    x1<-rbind(x_1,x1)
  }

  write.csv(x1, "tabela_infobasic.csv")

}

