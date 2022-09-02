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

URL planilha de acomp. atual: [https://docs.google.com/spreadsheets/u/1/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY](https://docs.google.com/spreadsheets/u/1/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY)

- Sheet 6 : [https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=265256008](https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=265256008)
(List_for_HTML_profile)

```r
prepare_listOfSpecies_from_followUpTable_sheet6()
```

## Baixa os dados das espécies da planilha de acompanhamento da nuvem para o arquivo local

```r
get_species_from_followUpTable()
```

## Obtém as *obras princeps* do Tropicos e IPNI da lista de espécies

Ao término da obtenção, é necessário proceder a revisão manual das *strings* obtidas, salvando a forma correta na aba `revision`.

```r
get_obraPrinceps_from_Tropicos_IPNI()
```

## Preenche a planilha de acompanhamento no arquivo local com as *obras princeps* obtidas

```r
fill_followUpTable_with_obrasPrinceps()
```

## Obtém as citações da Flora e Funga do Brasil via scraping

```r
get_citations_from_FloraFungaBrasil()
```

## Preenche a planilha de acompanhamento no arquivo local com as citações da Flora e Funga do Brasil obtidas

```r
fill_followUpTable_with_citations_from_FloraFungaBrasil()
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

## Prepara a lista de espécies para análise de interseção com PANS, TERs e UCs

Utilizando o argumento `onlyNonExistentFile = T`, apenas são arroladas as espécies que não possuem resultados para as análises

```r
prepare_listOfSpecies_files_to_intersectPANsTERsUCs(onlyNonExistentFile = T)
```




## Profile of species HTML

## Assessment HTML
