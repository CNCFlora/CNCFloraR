# CNCFloraR
Programa para produção dos painéis do perfil e de avaliação

## Install

```r
if (!require(devtools)) install.packages('devtools')
devtools::install.packages("CNCFlora/CNCFloraR")
```

## Menu principal

```r
menu_CNCFlora()
```

## Baixa lista de espécies da planilha de acompanhamento para criação do HTML do perfil

```r
prepare_listOfSpecies_from_followUpTable_sheet6()
```

## Checa todos os arquivos da lista de espécies

```r
check_all_files_of_species()
```

## Prepara a lista de espécies para baixamento da página de registros de ocorrências, para baixá-las via script AHK do antigo sistema 

```r
prepare_listOfSpecies_files_to_getOccurrences()
```

## Prepara o script AHK para baixar a página de registros de ocorrências do antigo sistema

```r
AHKscript_to_download_occurrenceRecords_from_oldSystem()
```




## Profile of species HTML

## Assessment HTML
