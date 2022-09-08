# CNCFloraR
Programa para produção dos painéis do perfil e de avaliação de espécies

## Instalação

```r
if (!require(devtools)) install.packages('devtools')
devtools::install.packages("CNCFlora/CNCFloraR")
```

## Menu principal

```r
menu_CNCFlora()
```

Outros menus:

- Menu para preparação dos arquivos de listas de espécies para todos os processos
  - `menu_prepare_listOfSpecies_files()`
- Menu para condução da análise de interseção com PANS, TERs e UCs
  - `menu_intersect_occurrenceRecords_with_PANs_TERs_UCs()`
- Menu para condução da análise de sobreposição com a série histórica do MapBiomas - Cobertura do Solo: 1985-2020
  - `menu_overlayAnalysis()`
- Menu para condução da criação do perfil de espécies Possivelmente Ameaçadas (PAs)
  - `menu_create_profileOfSpeciesHTMLs_PTs()`
- Menu para condução da criação do perfil de espécies Possivelmente Não Ameaçadas (PNAs)
  - `menu_create_profileOfSpeciesHTMLs_PNTs()`


## Prepara lista de espécies a partir da planilha de acompanhamento para criação do HTML do perfil

URL planilha de acomp. atual: [https://docs.google.com/spreadsheets/u/1/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY](https://docs.google.com/spreadsheets/u/1/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY)

- Sheet 6 : [https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=265256008](https://docs.google.com/spreadsheets/d/1vdU2njQ-ZJl4FiDCPpmiX-VrL0637omEyS_hBXQtllY/edit#gid=265256008)
(List_for_HTML_profile)

```r
prepare_listOfSpecies_from_followUpTable_sheet6()
```

## Construção da planilha de acompanhamento no arquivo local

### Prepara lista de espécies para obtenção dos dados das espécies da planilha de acompanhamento da nuvem para o arquivo local, das obras princeps e citações da Flora e Funga do Brasil

```r
prepare_listOfSpecies_files_to_build_followUpTable()
```

### Obtém os dados das espécies da planilha de acompanhamento da nuvem para o arquivo local

```r
get_species_from_followUpTable()
```

### *Obras princeps*

#### Obtém as *obras princeps* do Tropicos e IPNI da lista de espécies

Ao término da obtenção, é necessário proceder a revisão manual das *strings* obtidas, salvando a forma correta na aba `revision`.

```r
get_obraPrinceps_from_Tropicos_IPNI()
```

#### Preenche a planilha de acompanhamento no arquivo local com as *obras princeps* obtidas

```r
fill_followUpTable_with_obrasPrinceps()
```

### Citações da Flora e Funga do Brasil

#### Obtém as citações da Flora e Funga do Brasil via scraping

```r
get_citations_from_FloraFungaBrasil()
```

#### Preenche a planilha de acompanhamento no arquivo local com as citações da Flora e Funga do Brasil obtidas

```r
fill_followUpTable_with_citations_from_FloraFungaBrasil()
```

## Checa todos os arquivos da lista de espécies

```r
check_all_files_of_species()
```

## Baixamento da página de registros de ocorrências

### Prepara a lista de espécies para baixamento da página de registros de ocorrências, para baixá-las via script AHK do antigo sistema 

```r
prepare_listOfSpecies_files_to_getOccurrences()
```

### Gera o script AHK para baixar a página de registros de ocorrências do antigo sistema

```r
AHKscript_to_download_occurrenceRecords_from_oldSystem()
```

Execute o arquivo gerado.

## Condução da análise de interseção com PANS, TERs e UCs

### Prepara a lista de espécies para análise de interseção com PANS, TERs e UCs

Utilizando o argumento `onlyNonExistentFile = T`, apenas são arroladas as espécies que não possuem resultados para as análises

```r
prepare_listOfSpecies_files_to_intersectPANsTERsUCs(onlyNonExistentFile = T)
```

### Menu para condução da análise de interseção com PANS, TERs e UCs

```r
menu_intersect_occurrenceRecords_with_PANs_TERs_UCs()
```

A condução da análise de interseção com PANS, TERs e UCs também pode ser executada utilizando as funções específicas para produção e execução do *script*, além da checagem dos arquivos (*script* e resultados).

#### Criação dos *scripts* para condução da análise de interseção com PANS, TERs e UCs

```r
intersect_PANs_TERs_UCs_create_scripts()
```

#### Execução dos *scripts* para condução da análise de interseção com PANS, TERs e UCs

```r
intersect_PANs_TERs_UCs_execute_scripts()
```

#### Checa os arquivos da análise de interseção com PANS, TERs e UCs (*scripts* e resultados)

```r
intersect_PANs_TERs_UCs_check_files()
```

## Condução da análise de sobreposição com a série histórica do MapBiomas - Cobertura do Solo: 1985-2020

### Prepara a lista de espécies para análise de sobreposição com a série histórica do MapBiomas - Cobertura do Solo: 1985-2020

Utilizando o argumento `onlyNonExistentFile = T`, apenas são arroladas as espécies que não possuem resultados para as análises

```r
prepare_listOfSpecies_files_to_overlayAnalysis(onlyNonExistentFile = T)
```

### Menu para condução da análise de sobreposição com a série histórica do MapBiomas - Cobertura do Solo: 1985-2020

```r
menu_overlayAnalysis()
```

A condução da análise de sobreposição também pode ser executada utilizando as funções específicas para produção e execução do *script*, além da checagem dos arquivos (*script* e resultados).

#### Criação dos *scripts* para condução da análise de sobreposição

```r
overlayAnalysis_create_scripts()
```

#### Execução dos *scripts* para condução da análise de sobreposição

```r
overlayAnalysis_execute_scripts()
```

#### Checa os arquivos da análise de sobreposição (*scripts* e resultados)

```r
overlayAnalysis_check_files()
```

## Condução da criação dos HTMLs dos perfis das espécies

### Prepara a lista de espécies para criação dos HTMLs dos perfis das espécies

Utilizando o argumento `onlyNonExistentFile = T`, apenas são arroladas as espécies que não possuem HTMLs dos perfil

```r
prepare_listOfSpecies_files_to_build_profileOfSpeciesHTMLs(onlyNonExistentFile = T)
```

### Menu para condução da criação dos HTMLs dos perfis das espécies

```r
menu_create_profileOfSpeciesHTMLs()
```

A condução da criação dos HTMLs dos perfis das espécies também pode ser executada utilizando as funções específicas para produção e execução do *script*, além da checagem dos arquivos (*script* e resultados).

#### Criação dos *scripts* para condução da criação dos HTMLs dos perfis das espécies

```r
create_profileOfSpeciesHTML_create_scripts()
```

#### Execução dos *scripts* para condução da criação dos HTMLs dos perfis das espécies

```r
create_profileOfSpeciesHTML__execute_scripts()
```

#### Checa os arquivos dos HTMLs dos perfis das espécies

```r
create_profileOfSpeciesHTML__check_files()
```

## Preenchimento automático dos perfis das espécies no sistema antigo

### Obtém o profile ID das espécies no sistema antigo

### Leitura dos dados a partir dos HTMLs dos perfis das espécies

#### Informações básicas

#### Ameaças

#### Ações de conservação

### Preparação dos *scripts* AHK para preenchimento automático no sistema antigo

#### Informações básicas

#### Ameaças

#### Ações de conservação

## Assessment HTML
