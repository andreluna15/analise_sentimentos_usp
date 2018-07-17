---
title: "Análise de Sentimentos para o Painel USP de Gêmeos"
author: "André Luna"
date: "7/13/2018"
output: html_document
---

## Propósito do Estudo

## Coleta de Dados

### Dicionário
```{r loading_data, include = FALSE}
#encode para ler strings em PT-BR
Sys.setlocale(category = "LC_CTYPE", locale = "en_US.UTF-8")

#library
library(tidytext)
library(dplyr)
library(readxl)

  #devtools::install_github("sillasgonzaga/lexiconPT")
library(lexiconPT)
library(tm) #corpus + string manipulation
library(stringr) #string manipulation
## Lendo arquivo
dados <- read_excel("./respostas.xlsx")

###renomeando colunas
#definindo nomes das variaveis
var_name <- c("data_hora", "email", "termo_aceite", "relato", "outros_irmaos", "viveram_separados", 
  "separados_tempo", "personalidade_irmao_antes", "relacao_antes", "personalidade_irmao_agora", 
  "relacao_agora", "cadastro_painel", "deseja_mais_info", "nome", "sobrenome", "escolaridade",
  "nome_mae", "nascimento_mae", "sexo", "nacionalidade", "reside_cidade", "reside_uf", "reside_pais",
  "nascimento", "profissao", "forma de contato", "telefone", "gemeos_sexo", "nome_irmao", "nascimento_irmao",
  "gemeos_zig",
  "gemeos_parece", "parece_nota", "dif_escola", "confunde_escola", "dif_familia", "confunde_familia_nota",
  "cor_olhos_cabelos",
  "parece_nota_infancia", "quando_crianca", "confunde_familia_infancia", "confunde_familia_nota")

#coletando questoes para construir dicionario
questoes <- names(dados)
questoes[4] <- "Relato de uma situação com o irmão"

#renomeando base
names(dados) <- var_name

##construindo dicionario de variaveis
dicionario <- as.tbl(data.frame(variavel = var_name, pergunta = questoes))

###Separando variaveis em bases separadas: dados_corpus, dados_cadastro
                                          #dados_pessoais, dados_irmao, dados_semelhanca
corpus_extract <- c("email", "relato", "personalidade_irmao_antes", "relacao_antes", "personalidade_irmao_agora", 
                    "relacao_agora")

cadastro_extract <-  c("email", "cadastro_painel", "deseja_mais_info", "nome", "sobrenome")

pessoal_extract <-  c("email", "nome_mae", "nascimento_mae", "sexo", "nacionalidade", "reside_cidade", "reside_uf", "reside_pais",
                      "nascimento", "profissao", "forma de contato", "telefone", "escolaridade")

irmao_extract <- c("email", "gemeos_sexo", "nome_irmao", "nascimento_irmao")

semelhanca_extract <- c("email", "gemeos_zig", "gemeos_parece", "parece_nota", "dif_escola", 
                        "confunde_escola", "dif_familia", "confunde_familia_nota",
                        "cor_olhos_cabelos", "parece_nota_infancia", "quando_crianca", 
                        "confunde_familia_infancia", "confunde_familia_nota")


dados_corpus <- dados[, corpus_extract]
dados_cadastro <- dados[, cadastro_extract]
dados_pessoais <- dados[, pessoal_extract]
dados_irmao <- dados[, irmao_extract]
dados_semelhanca <- dados[, semelhanca_extract]

#observando número de caracteres por resposta
dados_corpus <- dados_corpus %>% mutate(nchar_relato = nchar(relato), 
                                      nchar_personalidade_irmao_antes = nchar(personalidade_irmao_antes),
                                      nchar_relacao_antes = nchar(relacao_antes),
                                      nchar_personalidade_irmao_agora = nchar(personalidade_irmao_agora),
                                      nchar_relacao_agora = nchar(relacao_agora))
        #as respostas cumprem os requisitos de caracteres! 
        #mas temos duas não- respostas
#removendo linhas com respostas ruins
dados_corpus <- dados_corpus[-c(3,4),]

```

A tabela a seguir deve ser utilizada como dicionário de variáveis, denotando a qual pergunta cada variável se refere:

```{r dicionario, echo = FALSE} 
knitr::kable(dicionario)
```

### Os Dados
Foram coletadas até o momento `r nrow(dados_corpus)` respostas apropriadas sobre um conjunto de `r ncol(dados)-1` itens. Estes itens se dividem em seções de temas distintos:

* dados cadastrais: Dados de cadastro dos respondentes, composto por: `r names(dados_cadastro)`
* dados pessoais: `r names(dados_pessoais)`
* dados do irmão: `r names(dados_irmao)`
* dados sobre a semelhança com o irmão: `r names(dados_semelhanca)`
* respostas textuais: `r names(dados_corpus)`

As respostas textuais contém os dados utilizados para a análise de sentimento, enquanto as informações pessoais e de semelhança com o irmão serão usadas para controle dos grupos analisados.

Segundo **FONTE** é necessário haver um mínimo de 250 caracteres por tema, e pelo menos *x* caracteres escritos por um indivíduo para que a análise seja conduzida de maniera robusta. Para garantir estes parâmetros, exigimos 1000 caracteres, no mínimo, de resposta ao relato de uma situação com o irmão, e 250 no mínimo para as questões textuais subsequentes. Como resultados, obtivemos todas as respostas dentro dos parâmetros esperados, como podemos ver na tabela a seguir, demonstrando um sumário da contagem de caracteres por resposta. O sufixo "nchar" denota "número de caracteres" de cada resposta:

```{r sumario_respostas, echo = F}
knitr::kable(summary(dados_corpus[,7:11]))


```


## O Método de Análise
Como método de análise de sentimentos, foi utilizada a técnica *Bag of Words*. A Tecnica consiste na *tokenização* dos textos, ou seja, na quebra do texto em uma coluna, cujo cada linha contém apenas uma palavra. A partir disso, calculamos a frequência em que cada palavra aparece, e as cruzamos individualmente com um ou mais *Léxicos de Sentimentos* na língua portuguesa. Os *Léxicos* são dicionários onde cada palavra está atrelada a uma pontuação de conteúdo sentimental. Quando palavras têm conteúdo individual positivo, é atribuídoa pontuação +1. Caso o conteúdo seja negativo, pontua-se -1. Por fim, se a palavra não apresenta conteúdo sentimental, pontua-se 0. Ao multiplicar a frequência de cada palavra pela sua pontuação podemos avaliar se o conteúdo do texto é majoritariamente positivo ou negativo.

A *tokenização* dos textos  e consequente análise de frequência também nos permite comparar os estilos de fala entre diferentes grupos etários, étnicos, econômicos e de gênero. Podemos também fazer distinções quanto às palavras positivas ou negativas utilizadas por tópicos, nos permitindo inferir sobre o conteúdo sentimental além das pontuações oferecidas pelo *Léxico de Sentimentos*.

Embora o método apresente visíveis limitações, não levando em consideração o contexto com que as palavras são utilizadas, podendo incorrer em pontuações equivocadas quando elementos como sarcamos estão presentes, a técnica tem sido amplamente utilizada com diversos propósitos e tem apresentado resultados satisfatórios **FONTE**.


## Pré-processamento dos Dados
Esta seção destina-se a apresentar as etapas de transformação dos dados para a sua forma "tidy", ou seja, pronta para análise.

Para conduzir as análises, as seguintes bibliotecas foram carregadas no R:

```{r livraria, eval = FALSE}
library(dplyr) #biblioteca para manipulação de dados em geral
library(readxl) #biblioteca para importar arquivos do excel para o R
library(lexiconPT) #função que permite aplicar diversos léxicos em português
library(tm) #manipulação de corpus (tipo de dado) e manipulação de strings
library(stringr) #manipulação de strings
```

Caso o leitor tenha pouca familiaridade com o R, vale saber que antes de carregar uma biblioteca é necessário instalá-la, o que pode ser feito com o seguinte código:

```{r install, eval = F}
install.packages("nome_da_biblioteca_entre_aspas")
```

A biblioteca lexiconPT é um caso específico, e deve ser instalada utilizando a biblioteca *devtools*, que deve ser instalada e carregada utilizando os comandos acima. Assim, para instalar lexiconPT:

```{r lexiconpt, eval = F}
library(devtools)
devtools::install_github("sillasgonzaga/lexiconPT")
```

Como estamos lidando com textos cheios de acentos e caracteres especiais, característica das línguas latinas, é comum haver problemas de codificação de caracteres dentro do R, de forma que acentos não sejam lidos corretamente. Para evitar tais problemas, definimos manualmente com qual codificação o R deve ser os textos:

```{r encoding, eval = F}
#encode para ler strings em PT-BR
Sys.setlocale(category = "LC_CTYPE", locale = "en_US.UTF-8")
```

Uma vez que instalamos todas as bibliotecas necessárias e definimos as configurações ótimas, carregamos os dados. Os dados foram manualmente baixados de um formulário do GoogleSheet, e por questões de privacidade não podem ser apresentados integralmente ou disponibilizados para reprodutividade. De toda forma, este documento deve ser suficiente para entender todas as etapas de transformação e análise de dados. 

Os nomes de colunas advindas do GoogleSheets não são apropriados para análise, por tanto um vetor de novos nomes foi criado, renomeando o banco de dados e servindo para a criação do dicionário previamente apresentado. Uma vez que o banco de dados foi renomeado, subvetores foram criados para dividir o banco de dados original nas diferentes seções supracitadas:

```{r loading_naming, eval = F}
## Lendo arquivo
dados <- read_excel("./respostas.xlsx")

###renomeando colunas
#definindo nomes das variaveis
var_name <- c("data_hora", "email", "termo_aceite", "relato", "outros_irmaos", "viveram_separados", 
  "separados_tempo", "personalidade_irmao_antes", "relacao_antes", "personalidade_irmao_agora", 
  "relacao_agora", "cadastro_painel", "deseja_mais_info", "nome", "sobrenome", "escolaridade",
  "nome_mae", "nascimento_mae", "sexo", "nacionalidade", "reside_cidade", "reside_uf", "reside_pais",
  "nascimento", "profissao", "forma de contato", "telefone", "gemeos_sexo", "nome_irmao", "nascimento_irmao",
  "gemeos_zig",
  "gemeos_parece", "parece_nota", "dif_escola", "confunde_escola", "dif_familia", "confunde_familia_nota",
  "cor_olhos_cabelos",
  "parece_nota_infancia", "quando_crianca", "confunde_familia_infancia", "confunde_familia_nota")

#coletando questoes para construir dicionario
questoes <- names(dados)
questoes[4] <- "Relato de uma situação com o irmão"
#O título da pergunta 4 não refletia bem seu conteúdo, por isso foi renomeado acima


#renomeando base - aplicando o vetor de nomes aos nomes das colunas dos dados originais
names(dados) <- var_name

##construindo dicionario de variaveis
dicionario <- as.tbl(data.frame(variavel = var_name, pergunta = questoes))
#transformei o data.frame em tbl, um formato de visualização mais amigável


###Separando variaveis em bases separadas: dados_corpus, dados_cadastro
#dados_pessoais, dados_irmao, dados_semelhanca

#estes vetores contém apenas os nomes que quero extrair dos dados originais
corpus_extract <- c("email", "relato", "personalidade_irmao_antes", "relacao_antes", "personalidade_irmao_agora", "relacao_agora")

cadastro_extract <-  c("email", "cadastro_painel", "deseja_mais_info", "nome", "sobrenome")

pessoal_extract <-  c("email", "nome_mae", "nascimento_mae", "sexo", "nacionalidade", "reside_cidade", "reside_uf", "reside_pais", "nascimento", "profissao", "forma de contato", "telefone", "escolaridade")

irmao_extract <- c("email", "gemeos_sexo", "nome_irmao", "nascimento_irmao")

semelhanca_extract <- c("email", "gemeos_zig", "gemeos_parece", "parece_nota", "dif_escola", 
                        "confunde_escola", "dif_familia", "confunde_familia_nota",
                        "cor_olhos_cabelos", "parece_nota_infancia", "quando_crianca", 
                        "confunde_familia_infancia", "confunde_familia_nota")

#extraindo as colunas desejadas para formar as diferentes seções de dados
dados_corpus <- dados[, corpus_extract]
dados_cadastro <- dados[, cadastro_extract]
dados_pessoais <- dados[, pessoal_extract]
dados_irmao <- dados[, irmao_extract]
dados_semelhanca <- dados[, semelhanca_extract]
  
```

Os dados de texto, necessários para a análise de sentimento estão contidos no *dados_corpus* ...

**CONTINUA**
