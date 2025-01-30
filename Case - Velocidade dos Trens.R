

library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(RODBC)
library(tree)
library(stats)
library(ROSE)
library(randomForest)
library(readxl)
library(formattable)

setwd('E:\\Desktop\\Daniel')
getDTthreads()
setDTthreads(8)
set.seed(3)

#### 1. ETL (EXTRACT TRANSFORM AND LOAD) ####


## 1.1 Lendo as bases ####

Amostra_base_real <- data.table(read_excel("Case VMA requisição 33485\\Case Velocidade Ferrovia do Aço.xlsx", sheet = "Amostra base real"))

VMA <- data.table(read_excel("Case VMA requisição 33485\\Case Velocidade Ferrovia do Aço.xlsx", sheet = "VMA"))

Perfil_trecho <- data.table(read_excel("Case VMA requisição 33485\\Case Velocidade Ferrovia do Aço.xlsx", sheet = "Perfil Longitudinal"))

# Vislumbrando os dados

glimpse(Amostra_base_real)

#Transformando em numérico a variável KM

Amostra_base_real[,KM:=as.numeric(KM)]
Amostra_base_real[,Vel_mph:=as.numeric(Vel_mph)]



## 1.2 Filtros ####

# trens carregados
# prefixo que iniciam em N
# peso igual ou superior a 16 mil toneladas

Amostra_base_real[,.N,Prefixo]
Amostra_base_real[,.N,Peso][order(Peso)]
Amostra_base_real[,.N,Carga]

Amostra_base_real <- Amostra_base_real[str_sub(Prefixo,1,1)=="N" &
                                         Peso>=16000 &
                                         Carga=="Carregado"]



## 1.3 Mergindo as três bases ####


# 1.3.1 Amostra_base_real ~ VMA ####

### Renomeando as variáveis

VMA <- rename(VMA, KM_Inicial_VMA = `KM Inicial`,
              KM_Final_VMA = `KM Final`,
              VMA_kmh = `VMA (km/h)`)

### Preparar a base para o merge "foverlaps"

# SLIDE 2 3

## Verificando se existe algum KM que seja igual a algum ponto de mudança do intervalo KM_Inicial e KM_Final

print(Alteracao_1 <- Amostra_base_real[KM %in% VMA$KM_Final_VMA])

# Verificando qual VMA é a menor (por segurança)

VMA[KM_Inicial_VMA==150 | KM_Final_VMA==150]

## Como o intervalo 4 - 150 tem uma VMA menor, vou fazer uma leve correção
# no KM == 150 para 149.99

Amostra_base_real[KM==150,KM:=149.99]

# Conferindo
Amostra_base_real[KM %in% VMA$KM_Final_VMA]

# Adicionar uma coluna KM_Final_VMA na Amostra_base_real para fazer o intervalo

Amostra_base_real[, KM_Final_VMA := KM]  

# Definir chaves para a função foverlaps

setkey(Amostra_base_real, KM, KM_Final_VMA)
setkey(VMA, KM_Inicial_VMA, KM_Final_VMA)

# Merge com foverlaps
Amostra_base_real <- foverlaps(Amostra_base_real, VMA, by.x = c("KM", "KM_Final_VMA"), by.y = c("KM_Inicial_VMA", "KM_Final_VMA"), type="within")

# Limpando a base e corrigindo a alteração anterior

Amostra_base_real[,i.KM_Final_VMA:=NULL]

Amostra_base_real[Data == Alteracao_1$Data &
                    Vel_mph == Alteracao_1$Vel_mph, KM:=150]


# 1.3.2 Amostra_base_real+VMA ~ Perfil_trecho ####

### Renomeando as variáveis

Perfil_trecho <- rename(Perfil_trecho, KM_Inicial_PERFIL = `KM Inicial`,
                        KM_Final_PERFIL = `KM Final`,
                        INCLINACAO = `Inclinação (%)`)

### Preparar a base para o merge "foverlaps"

## Verificando se existe algum KM que seja igual a algum ponto de mudança do intervalo KM_Inicial_PERFIL e KM_Final_PERFIL

print(Alteracao_2 <- Amostra_base_real[KM %in% Perfil_trecho$KM_Final_PERFIL])

# Verificando qual INCLINACAO é a maior em módulo (por segurança)

Perfil_trecho[KM_Inicial_PERFIL %in% Alteracao_2$KM |
                KM_Final_PERFIL %in% Alteracao_2$KM]

## Alterando para não duplicar no foverlaps

Amostra_base_real[ KM ==  6.15 , KM:= 6.16 ]
Amostra_base_real[ KM == 44.33 , KM:= 44.34 ]
Amostra_base_real[ KM == 75.90 , KM:= 75.89 ]
Amostra_base_real[ KM == 76.50 , KM:= 76.51 ]
Amostra_base_real[ KM == 83.29 , KM:= 83.28 ]
Amostra_base_real[ KM == 88.26 , KM:= 88.25 ]
Amostra_base_real[ KM ==136.34 , KM:= 136.33 ]
Amostra_base_real[ KM ==231.50 , KM:= 231.49 ]

# Conferindo
Amostra_base_real[KM %in% Perfil_trecho$KM_Final_PERFIL]

# Adicionar uma coluna KM_Final_PERFIL na Amostra_base_real para fazer o intervalo

Amostra_base_real[, KM_Final_PERFIL := KM]  

# Definir chaves para a função foverlaps

setkey(Amostra_base_real, KM, KM_Final_PERFIL)
setkey(Perfil_trecho, KM_Inicial_PERFIL, KM_Final_PERFIL)

# Corrigindo os valores em que KM_Inicial_PERFIL > KM_Final_PERFIL

Perfil_trecho <- Perfil_trecho[,PRECISA_CORRIGIR:=(KM_Inicial_PERFIL > KM_Final_PERFIL)]

# Corrigindo
Perfil_trecho <- Perfil_trecho[PRECISA_CORRIGIR=="FALSE"]
Perfil_trecho[,PRECISA_CORRIGIR:=NULL]


# Merge com foverlaps
Amostra_base_real <- foverlaps(Amostra_base_real, Perfil_trecho, by.x = c("KM", "KM_Final_PERFIL"), by.y = c("KM_Inicial_PERFIL", "KM_Final_PERFIL"), type="within")

# Lembrar dos intervalos não contemplados

# Limpando as bases

Amostra_base_real[,i.KM_Final_PERFIL:=NULL]
rm(Alteracao_1,Alteracao_2)




## 1.4 Últimos ajustes na base ####

### Criando o Vel_kmh

Amostra_base_real[,Vel_kmh:=1.60934*Vel_mph]

### Criando a variável PERIGO --> Vel_kmh > VMA

Amostra_base_real[,PERIGO:= case_when(Vel_kmh > VMA_kmh ~ "SIM",
                                      Vel_kmh <= VMA_kmh ~ "NÃO")]

# Vislumbrando a quantidade de casos em perigo
Amostra_base_real[,.N,PERIGO]

# Eliminando da base os casos em que não há informação sobre VMA e Perfil do trecho

Amostra_base_real <- Amostra_base_real[KM<=296]

Amostra_base_real[,.N,PERIGO]

### Criando a variável EFICIENTE --> Vel_kmh < 0.80*VMA

Amostra_base_real[,EFICIENTE:= case_when(Vel_kmh < 0.80*VMA_kmh ~ "NÃO",
                                         Vel_kmh >= 0.80*VMA_kmh ~ "SIM")]

Amostra_base_real[,.N,EFICIENTE]


#### 2. ANÁLISE DESCRITIVA DO PERIGO e EFICIENTE ####

## INSPEÇÃO UNIVARIADA

# Será que a locomotiva é um fator relevante?

Amostra_base_real[,.(
  PERCENTUAL_PERIGO = percent(sum(PERIGO=="SIM")/.N)
  , PERCENTUAL_SEGURO =percent(sum(PERIGO=="NÃO")/.N)
  , N_PERIGO = sum(PERIGO=="SIM")
  , N_SEGURO = sum(PERIGO=="NÃO")
),.(cd_locomotiva)]

Amostra_base_real[,.(
  PERCENTUAL_EFICIENTE = percent(sum(EFICIENTE=="SIM")/.N)
  , PERCENTUAL_NAO_EFICIENTE =percent(sum(EFICIENTE=="NÃO")/.N)
  , N_EFICIENTE = sum(EFICIENTE=="SIM")
  , N_NAO_EFICIENTE = sum(EFICIENTE=="NÃO")
),.(cd_locomotiva)]

# Será que o prefixo é um fator relevante?

Amostra_base_real[,.(
  PERCENTUAL_PERIGO = percent(sum(PERIGO=="SIM")/.N)
  , PERCENTUAL_SEGURO =percent(sum(PERIGO=="NÃO")/.N)
  , N_PERIGO = sum(PERIGO=="SIM")
  , N_SEGURO = sum(PERIGO=="NÃO")
),.(Prefixo)]

Amostra_base_real[,.(
  PERCENTUAL_EFICIENTE = percent(sum(EFICIENTE=="SIM")/.N)
  , PERCENTUAL_NAO_EFICIENTE =percent(sum(EFICIENTE=="NÃO")/.N)
  , N_EFICIENTE = sum(EFICIENTE=="SIM")
  , N_NAO_EFICIENTE = sum(EFICIENTE=="NÃO")
),.(Prefixo)]

# Será que o eixo é um fator relevante?

Amostra_base_real[,.(
  PERCENTUAL_PERIGO = percent(sum(PERIGO=="SIM")/.N)
  , PERCENTUAL_SEGURO =percent(sum(PERIGO=="NÃO")/.N)
  , N_PERIGO = sum(PERIGO=="SIM")
  , N_SEGURO = sum(PERIGO=="NÃO")
),.(Eixos)]

Amostra_base_real[,.(
  PERCENTUAL_EFICIENTE = percent(sum(EFICIENTE=="SIM")/.N)
  , PERCENTUAL_NAO_EFICIENTE =percent(sum(EFICIENTE=="NÃO")/.N)
  , N_EFICIENTE = sum(EFICIENTE=="SIM")
  , N_NAO_EFICIENTE = sum(EFICIENTE=="NÃO")
),.(Eixos)]

# Será que a linha é um fator relevante?

Amostra_base_real[,.(
  PERCENTUAL_PERIGO = percent(sum(PERIGO=="SIM")/.N)
  , PERCENTUAL_SEGURO =percent(sum(PERIGO=="NÃO")/.N)
  , N_PERIGO = sum(PERIGO=="SIM")
  , N_SEGURO = sum(PERIGO=="NÃO")
),.(Linha)][order(N_PERIGO)]

Amostra_base_real[,.(
  PERCENTUAL_EFICIENTE = percent(sum(EFICIENTE=="SIM")/.N)
  , PERCENTUAL_NAO_EFICIENTE =percent(sum(EFICIENTE=="NÃO")/.N)
  , N_EFICIENTE = sum(EFICIENTE=="SIM")
  , N_NAO_EFICIENTE = sum(EFICIENTE=="NÃO")
),.(Linha)][order(N_EFICIENTE)]

# Será que o peso é um fator relevante?

Amostra_base_real[,.(
    PERCENTUAL_LEVE = percent(sum(Peso<16918)/.N)
  , PERCENTUAL_PESADO = percent(sum(Peso>16918)/.N)
  , N_LEVE = sum(Peso<16918)
  , N_PESADO = sum(Peso>16918)
),.(PERIGO)]

Amostra_base_real[,.(
  PERCENTUAL_LEVE = percent(sum(Peso<16918)/.N)
  , PERCENTUAL_PESADO = percent(sum(Peso>16918)/.N)
  , N_LEVE = sum(Peso<16918)
  , N_PESADO = sum(Peso>16918)
),.(EFICIENTE)]

# Será que o intervalo da KM em que a VMA é a mesma é relevante?

# Criando o intervalo
Amostra_base_real[,INTERVALO_VMA:=  paste(KM_Inicial_VMA, KM_Final_VMA, sep = "-")]

Amostra_base_real[,.(
  PERCENTUAL_PERIGO = percent(sum(PERIGO=="SIM")/.N)
  , PERCENTUAL_SEGURO =percent(sum(PERIGO=="NÃO")/.N)
  , N_PERIGO = sum(PERIGO=="SIM")
  , N_SEGURO = sum(PERIGO=="NÃO")
),.(INTERVALO_VMA,VMA_kmh)]

Amostra_base_real[,.(
  PERCENTUAL_EFICIENTE = percent(sum(EFICIENTE=="SIM")/.N)
  , PERCENTUAL_NAO_EFICIENTE =percent(sum(EFICIENTE=="NÃO")/.N)
  , N_EFICIENTE = sum(EFICIENTE=="SIM")
  , N_NAO_EFICIENTE = sum(EFICIENTE=="NÃO")
),.(INTERVALO_VMA,VMA_kmh)]

# Será que o intervalo da KM em que o perfil do trecho é o mesmo é relevante?

# Criando o intervalo
Amostra_base_real[,INTERVALO_PERFIL:=  paste(round(KM_Inicial_PERFIL,2),
                                             round(KM_Final_PERFIL,2), sep = "-")]

Amostra_base_real[,.(
  PERCENTUAL_PERIGO = percent(sum(PERIGO=="SIM")/.N)
  , PERCENTUAL_SEGURO =percent(sum(PERIGO=="NÃO")/.N)
  , N_PERIGO = sum(PERIGO=="SIM")
  , N_SEGURO = sum(PERIGO=="NÃO")
),.(INTERVALO_PERFIL)][order(-PERCENTUAL_PERIGO)]

Amostra_base_real[,.(
  PERCENTUAL_EFICIENTE = percent(sum(EFICIENTE=="SIM")/.N)
  , PERCENTUAL_NAO_EFICIENTE =percent(sum(EFICIENTE=="NÃO")/.N)
  , N_EFICIENTE = sum(EFICIENTE=="SIM")
  , N_NAO_EFICIENTE = sum(EFICIENTE=="NÃO")
),.(INTERVALO_PERFIL)][order(-PERCENTUAL_NAO_EFICIENTE)]

# Será que a inclinação é determinante?

# PERIGO

ggplot(Amostra_base_real, aes(x = PERIGO, y = INCLINACAO)) + 
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1)

ggplot(Amostra_base_real, aes(x = INCLINACAO, color = PERIGO)) +
  geom_density()

ggplot(Amostra_base_real, aes(x = PERIGO, y = INCLINACAO)) +
  geom_jitter(width = 0.1)

# EFICIENTE

ggplot(Amostra_base_real, aes(x = EFICIENTE, y = INCLINACAO)) + 
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1)

ggplot(Amostra_base_real, aes(x = INCLINACAO, color = EFICIENTE)) +
  geom_density()

ggplot(Amostra_base_real, aes(x = EFICIENTE, y = INCLINACAO)) +
  geom_jitter(width = 0.1)

# Será que o período da noite é relevante?

Amostra_base_real[,HORA:=as.numeric(str_sub(Data,"12","13"))]

Amostra_base_real[HORA>=21 | HORA<=5,PERIODO:="NOITE"]
Amostra_base_real[is.na(PERIODO),PERIODO:="DIA"]

Amostra_base_real[,.(
  PERCENTUAL_PERIGO = percent(sum(PERIGO=="SIM")/.N)
  , PERCENTUAL_SEGURO =percent(sum(PERIGO=="NÃO")/.N)
  , N_PERIGO = sum(PERIGO=="SIM")
  , N_SEGURO = sum(PERIGO=="NÃO")
),.(PERIODO)]

Amostra_base_real[,.(
  PERCENTUAL_EFICIENTE = percent(sum(EFICIENTE=="SIM")/.N)
  , PERCENTUAL_NAO_EFICIENTE =percent(sum(EFICIENTE=="NÃO")/.N)
  , N_EFICIENTE = sum(EFICIENTE=="SIM")
  , N_NAO_EFICIENTE = sum(EFICIENTE=="NÃO")
),.(PERIODO)]


## INSPEÇÃO BIVARIADA

# INCLINACAO, Eixos e peso --> PERIGO

# Será que alguma combinação de inclinação e eixos determina?

ggplot(Amostra_base_real, aes(x = as.factor(PERIGO), y = INCLINACAO)) + 
  geom_boxplot() +
  facet_grid(~ Eixos) +
  labs(title = "Boxplot de INCLINACAO por PERIGO e Eixos", x = "PERIGO", y = "INCLINACAO")

ggplot(Amostra_base_real, aes(x = as.factor(PERIGO), y = INCLINACAO)) + 
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  facet_grid(~ Eixos) +
  labs(title = "Violin Plot de INCLINACAO por PERIGO e Eixos", x = "PERIGO", y = "INCLINACAO")

# Será que alguma combinação de inclinação e PESO (leve ou pesado) determina?

Amostra_base_real[Peso<16900,TIPO:="LEVE"]
Amostra_base_real[Peso>=16900,TIPO:="PESADO"]

ggplot(Amostra_base_real, aes(x = as.factor(PERIGO), y = INCLINACAO)) + 
  geom_boxplot() +
  facet_grid(~ TIPO) +
  labs(title = "Boxplot de INCLINACAO por PERIGO e TIPO", x = "PERIGO", y = "INCLINACAO")

ggplot(Amostra_base_real, aes(x = as.factor(PERIGO), y = INCLINACAO)) + 
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  facet_grid(~ TIPO) +
  labs(title = "Violin Plot de INCLINACAO por PERIGO e TIPO", x = "PERIGO", y = "INCLINACAO")

Amostra_base_real[,.N,.(Eixos,TIPO,PERIGO)][order(Eixos,TIPO,PERIGO)]

# LEVE 556
#57/(57+738) = 0.07169811

# PESADO 562
#94/(94+1736) = 0.05136612

# PESADO 556
#4/(4+1039) = 0.003835091

Amostra_base_real[TIPO=="LEVE" & INCLINACAO>0.75,.N,PERIGO]

# LEVE 556 INCLINACAO > 0.75
#52/(52+360) = 0.1262136

# "INCOMODAR" EM 412/3668 = 0.112 "MOMENTOS" PARA TENTAR 
# EVITAR 52/155 = 0.335 SITUAÇÕES DE PERIGO

# INCLINACAO, Eixos e peso --> EFICIENTE

ggplot(Amostra_base_real, aes(x = as.factor(EFICIENTE), y = INCLINACAO)) + 
  geom_boxplot() +
  facet_grid(~ Eixos) +
  labs(title = "Boxplot de INCLINACAO por EFICIENTE e Eixos", x = "EFICIENTE", y = "INCLINACAO")

ggplot(Amostra_base_real, aes(x = as.factor(EFICIENTE), y = INCLINACAO)) + 
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  facet_grid(~ Eixos) +
  labs(title = "Violin Plot de INCLINACAO por EFICIENTE e Eixos", x = "EFICIENTE", y = "INCLINACAO")



#### 3. MODELAGEM ####

# Arrumando a base

base_modelo <- Amostra_base_real[,.(PERIGO=as.factor(PERIGO),
                                    EFICIENTE=as.factor(EFICIENTE),
                                    INCLINACAO,
                                    cd_locomotiva=as.factor(cd_locomotiva),
                                    Linha=as.factor(Linha),
                                    Prefixo=as.factor(Prefixo),
                                    Peso,
                                    Eixos=as.factor(Eixos),
                                    INTERVALO_VMA=as.factor(INTERVALO_VMA),
                                    INTERVALO_PERFIL=as.factor(INTERVALO_PERFIL),
                                    PERIODO=as.factor(PERIODO),
                                    TIPO=as.factor(TIPO)
                                    )]


base_modelo[is.na(INCLINACAO),INCLINACAO:=1.051]

base_modelo[,.N,PERIGO] # 4,23%
#155/(155+3513)

base_modelo[,.N,EFICIENTE] # 48,53%
#1780/(1888+1780)

#### PERIGO ####

# Aplicando a técnica Under-sampling para balancear os dados

base_PERIGO <- data.table(ovun.sample(PERIGO ~ ., data = base_modelo
                             , method = "under"
                             , N = 2 * sum(base_modelo$PERIGO == "SIM"))$data)


base_PERIGO[,.N,"PERIGO"]

base_PERIGO <- base_PERIGO[,.(PERIGO=as.factor(PERIGO),
                              #EFICIENTE=as.factor(EFICIENTE),
                              INCLINACAO,
                              cd_locomotiva=as.factor(cd_locomotiva),
                              Linha=as.factor(Linha),
                              #Prefixo=as.factor(Prefixo),
                              Peso,
                              Eixos=as.factor(Eixos),
                              INTERVALO_VMA=as.factor(INTERVALO_VMA),
                              #INTERVALO_PERFIL=as.factor(INTERVALO_PERFIL),
                              PERIODO=as.factor(PERIODO),
                              TIPO=as.factor(TIPO)
)]

## Random Forest para determinar quais variáveis são mais importantes

# Random Forest
rf_model <- randomForest(PERIGO ~ .
                           , data = base_PERIGO, importance = TRUE, ntree = 500)

# Plotando a importância das variáveis do modelo
varImpPlot(rf_model)

# Ajustando a árvore de decisão
model <- tree(PERIGO ~ INTERVALO_VMA 
              + INCLINACAO 
              + Peso 
              + PERIODO
              , data = base_PERIGO)

# Taxa de erro de classificação = 0.1677
summary(model)

# Taxa de PERIGO = 0.50
base_PERIGO[,.(PORCENTAGEM_PERIGO = sum(PERIGO=="SIM")/.N)]

# Visualizando a árvore (TRUE <-  ;  FALSE ->)
plot(model)
text(model,pretty=0)


# Avaliando a performance

set.seed(3)

# Usando 70% da base para treinar o modelo
train <-  sample(1:nrow(base_PERIGO), ceiling(0.7*nrow(base_PERIGO)))
base_PERIGO.test <-  base_PERIGO[- train,]
PERIGO.test <- base_PERIGO[- train,PERIGO]

# Ajustado o modelo nos dados de treinamento
tree.model <-  tree(PERIGO ~ Eixos + INCLINACAO + TIPO,base_PERIGO ,subset = train )

# Fazendo a previsão dos dados de teste
tree.predict <- predict(tree.model,base_PERIGO.test,type ="class")
table(tree.predict,PERIGO.test)

plot(tree.model)
text(tree.model,pretty=0)

# Prunando a árvore
cv.tree(tree.model,FUN =prune.misclass)

prune <- prune.misclass(tree.model,best =5)
plot(prune)
text(prune, pretty =0)

#### EFICIENTE ####

base_EFICIENTE <- base_modelo[,.( #PERIGO=as.factor(PERIGO),
                              EFICIENTE=as.factor(EFICIENTE),
                              INCLINACAO,
                              cd_locomotiva=as.factor(cd_locomotiva),
                              Linha=as.factor(Linha),
                              Prefixo=as.factor(Prefixo),
                              Peso,
                              Eixos=as.factor(Eixos),
                              INTERVALO_VMA=as.factor(INTERVALO_VMA),
                              #INTERVALO_PERFIL=as.factor(INTERVALO_PERFIL),
                              PERIODO=as.factor(PERIODO),
                              TIPO=as.factor(TIPO)
)]

## Random Forest para determinar quais variáveis são mais importantes

# Random Forest
rf_model <- randomForest(EFICIENTE ~ .
                         , data = base_EFICIENTE, importance = TRUE, ntree = 500)

# Plotando a importância das variáveis do modelo
varImpPlot(rf_model,main="Importância das variáveis")

# Ajustando a árvore de decisão
model <- tree(EFICIENTE ~ INTERVALO_VMA 
              + INCLINACAO 
              + Peso 
              + Linha
              + PERIODO
              + Eixos
              , data = base_EFICIENTE)

# Taxa de erro de classificação = 0.2306
summary(model)

# Taxa de PERIGO = 0.4852781
base_EFICIENTE[,.(PORCENTAGEM_EFICIENTE = sum(EFICIENTE=="NÃO")/.N)]

# Visualizando a árvore (TRUE <-  ;  FALSE ->)
plot(model)
text(model,pretty=0)


# Avaliando a performance

set.seed(3)

# Usando 70% da base para treinar o modelo
train <-  sample(1:nrow(base_EFICIENTE), ceiling(0.7*nrow(base_EFICIENTE)))
base_EFICIENTE.test <-  base_EFICIENTE[- train,]
EFICIENTE.test <- base_EFICIENTE[- train,EFICIENTE]

# Ajustado o modelo nos dados de treinamento
tree.model <-  tree(EFICIENTE ~ INTERVALO_VMA 
                    + INCLINACAO 
                    + Peso 
                    + Linha
                    + PERIODO
                    + Eixos,base_EFICIENTE ,subset = train )

# Fazendo a previsão dos dados de teste
tree.predict <- predict(tree.model,base_EFICIENTE.test,type ="class")
table(tree.predict,EFICIENTE.test)

plot(tree.model)
text(tree.model,pretty=0)

### Removando tudo

rm(list=ls())



############################################################
############################################################
###### BIG DATA part-00000 a part-00009 ####

big_data <- rbind(
  fread("Case VMA requisição 33485\\part-00000.csv",sep = ",")
  ,fread("Case VMA requisição 33485\\part-00001.csv",sep = ",")
  ,fread("Case VMA requisição 33485\\part-00002.csv",sep = ",")
  ,fread("Case VMA requisição 33485\\part-00003.csv",sep = ",")
  ,fread("Case VMA requisição 33485\\part-00004.csv",sep = ",")
  ,fread("Case VMA requisição 33485\\part-00005.csv",sep = ",")
  ,fread("Case VMA requisição 33485\\part-00006.csv",sep = ",")
  ,fread("Case VMA requisição 33485\\part-00007.csv",sep = ",")
  ,fread("Case VMA requisição 33485\\part-00008.csv",sep = ",")
  ,fread("Case VMA requisição 33485\\part-00009.csv",sep = ",")
)



####  ETL (EXTRACT TRANSFORM AND LOAD) ####


## Lendo as bases ####

VMA <- data.table(read_excel("Case VMA requisição 33485\\Case Velocidade Ferrovia do Aço.xlsx", sheet = "VMA"))

Perfil_trecho <- data.table(read_excel("Case VMA requisição 33485\\Case Velocidade Ferrovia do Aço.xlsx", sheet = "Perfil Longitudinal"))

# Vislumbrando os dados

glimpse(big_data)

#Transformando em numérico a variável KM

big_data[,KM:=as.numeric(KM)]
big_data[,Vel_mph:=as.numeric(Vel_mph)]

## Filtros ####

# trens carregados
# prefixo que iniciam em N
# peso igual ou superior a 16 mil toneladas

big_data <- big_data[str_sub(Prefixo,1,1)=="N" & Peso>=16000 & Carga=="Carregado"]

## Mergindo as três bases ####


# Amostra_base_real ~ VMA ####

### Renomeando as variáveis

VMA <- rename(VMA, KM_Inicial_VMA = `KM Inicial`,
              KM_Final_VMA = `KM Final`,
              VMA_kmh = `VMA (km/h)`)

### Preparar a base para o merge "foverlaps"

# Adicionar uma coluna KM_Final_VMA na Amostra_base_real para fazer o intervalo

big_data[, KM_Final_VMA := KM]  

# Definir chaves para a função foverlaps

setkey(big_data, KM, KM_Final_VMA)
setkey(VMA, KM_Inicial_VMA, KM_Final_VMA)

# Merge com foverlaps
big_data <- foverlaps(big_data, VMA, by.x = c("KM", "KM_Final_VMA"), by.y = c("KM_Inicial_VMA", "KM_Final_VMA"), type="within")

# Limpando a base

big_data[,i.KM_Final_VMA:=NULL]


# Amostra_base_real+VMA ~ Perfil_trecho ####

### Renomeando as variáveis

Perfil_trecho <- rename(Perfil_trecho, KM_Inicial_PERFIL = `KM Inicial`,
                        KM_Final_PERFIL = `KM Final`,
                        INCLINACAO = `Inclinação (%)`)

### Preparar a base para o merge "foverlaps"


# Adicionar uma coluna KM_Final_PERFIL na Amostra_base_real para fazer o intervalo

big_data[, KM_Final_PERFIL := KM]  

# Definir chaves para a função foverlaps

setkey(big_data, KM, KM_Final_PERFIL)
setkey(Perfil_trecho, KM_Inicial_PERFIL, KM_Final_PERFIL)

# Corrigindo os valores em que KM_Inicial_PERFIL > KM_Final_PERFIL

Perfil_trecho <- Perfil_trecho[,PRECISA_CORRIGIR:=(KM_Inicial_PERFIL > KM_Final_PERFIL)]

# Corrigindo
Perfil_trecho <- Perfil_trecho[PRECISA_CORRIGIR=="FALSE"]
Perfil_trecho[,PRECISA_CORRIGIR:=NULL]


# Merge com foverlaps
big_data <- foverlaps(big_data, Perfil_trecho, by.x = c("KM", "KM_Final_PERFIL"), by.y = c("KM_Inicial_PERFIL", "KM_Final_PERFIL"), type="within")

# Limpando as bases

big_data[,i.KM_Final_PERFIL:=NULL]


## Últimos ajustes na base ####

### Criando o Vel_kmh

big_data[,Vel_kmh:=1.60934*Vel_mph]

### Criando a variável PERIGO --> Vel_kmh > VMA

big_data[,PERIGO:= case_when(Vel_kmh > VMA_kmh ~ "SIM",
                                      Vel_kmh <= VMA_kmh ~ "NÃO")]

# Vislumbrando a quantidade de casos em perigo
big_data[,.N,PERIGO]

# 404240/(404240+17171749) = 0.02299956

### Criando a variável EFICIENTE --> Vel_kmh < 0.80VMA

big_data[,EFICIENTE:= case_when(Vel_kmh < 0.80*VMA_kmh ~ "NÃO",
                                         Vel_kmh >= 0.80*VMA_kmh ~ "SIM")]

big_data[,.N,EFICIENTE]

# 8000913/(8000913+9575076) = 0.4552184

# Eliminando da base os casos em que não há informação sobre VMA e Perfil do trecho

big_data <- big_data[KM<=296]

big_data[,INTERVALO_VMA:=  paste(KM_Inicial_VMA, KM_Final_VMA, sep = "-")]

big_data[,INTERVALO_PERFIL:=  paste(round(KM_Inicial_PERFIL,2),
                                             round(KM_Final_PERFIL,2), sep = "-")]

big_data[,HORA:=as.numeric(str_sub(Data,"12","13"))]

big_data[HORA>=21 | HORA<=5,PERIODO:="NOITE"]
big_data[is.na(PERIODO),PERIODO:="DIA"]

big_data[Peso<16900,TIPO:="LEVE"]
big_data[Peso>=16900,TIPO:="PESADO"]



#### MODELAGEM ####

# Arrumando a base

base_modelo <- big_data[,.(PERIGO=as.factor(PERIGO),
                           EFICIENTE=as.factor(EFICIENTE),
                           INCLINACAO,
                           cd_locomotiva=as.factor(cd_loco),
                           Linha=as.factor(Linha),
                           Prefixo=as.factor(Prefixo),
                           Peso,
                           Eixos=as.factor(Eixos),
                           INTERVALO_VMA=as.factor(INTERVALO_VMA),
                           INTERVALO_PERFIL=as.factor(INTERVALO_PERFIL),
                           PERIODO=as.factor(PERIODO),
                           TIPO=as.factor(TIPO)
)]


base_modelo[is.na(INCLINACAO),INCLINACAO:=1.051]

base_modelo[,.N,PERIGO] # 2,30%
#404240/(404240+17171749)

base_modelo[,.N,EFICIENTE] # 45,52%
#8000913/(8000913+9575076)

#### ANÁLISE DESCRITIVA DO PERIGO e EFICIENTE ####

## INSPEÇÃO UNIVARIADA

# Será que a linha é um fator relevante?

base_modelo[,.(
  PERCENTUAL_PERIGO = percent(sum(PERIGO=="SIM")/.N)
  , PERCENTUAL_SEGURO =percent(sum(PERIGO=="NÃO")/.N)
  , N_PERIGO = sum(PERIGO=="SIM")
  , N_SEGURO = sum(PERIGO=="NÃO")
),.(Linha)][order(N_PERIGO)]

base_modelo[,.(
  PERCENTUAL_EFICIENTE = percent(sum(EFICIENTE=="SIM")/.N)
  , PERCENTUAL_NAO_EFICIENTE =percent(sum(EFICIENTE=="NÃO")/.N)
  , N_EFICIENTE = sum(EFICIENTE=="SIM")
  , N_NAO_EFICIENTE = sum(EFICIENTE=="NÃO")
),.(Linha)][order(N_EFICIENTE)]


# Será que a inclinação é determinante?

# PERIGO

ggplot(base_modelo, aes(x = PERIGO, y = INCLINACAO)) + 
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1)

ggplot(base_modelo, aes(x = INCLINACAO, color = PERIGO)) +
  geom_density()


# EFICIENTE

ggplot(base_modelo, aes(x = EFICIENTE, y = INCLINACAO)) + 
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1)

ggplot(base_modelo, aes(x = INCLINACAO, color = EFICIENTE)) +
  geom_density()

# Será que o período da noite é relevante?

base_modelo[,.(
  PERCENTUAL_PERIGO = percent(sum(PERIGO=="SIM")/.N)
  , PERCENTUAL_SEGURO =percent(sum(PERIGO=="NÃO")/.N)
  , N_PERIGO = sum(PERIGO=="SIM")
  , N_SEGURO = sum(PERIGO=="NÃO")
),.(PERIODO)]

base_modelo[,.(
  PERCENTUAL_EFICIENTE = percent(sum(EFICIENTE=="SIM")/.N)
  , PERCENTUAL_NAO_EFICIENTE =percent(sum(EFICIENTE=="NÃO")/.N)
  , N_EFICIENTE = sum(EFICIENTE=="SIM")
  , N_NAO_EFICIENTE = sum(EFICIENTE=="NÃO")
),.(PERIODO)]


#### PERIGO ####

# Aplicando a técnica Under-sampling para balancear os dados

base_PERIGO <- data.table(ovun.sample(PERIGO ~ ., data = base_modelo
                                      , method = "under"
                                      , N = 2 * sum(base_modelo$PERIGO == "SIM"))$data)


base_PERIGO[,.N,"PERIGO"]

base_PERIGO <- base_PERIGO[,.(PERIGO=as.factor(PERIGO),
                              #EFICIENTE=as.factor(EFICIENTE),
                              INCLINACAO,
                              #cd_locomotiva=as.factor(cd_locomotiva),
                              Linha=as.factor(Linha),
                              #Prefixo=as.factor(Prefixo),
                              Peso,
                              Eixos=as.factor(Eixos),
                              INTERVALO_VMA=as.factor(INTERVALO_VMA),
                              #INTERVALO_PERFIL=as.factor(INTERVALO_PERFIL),
                              PERIODO=as.factor(PERIODO)#,
                              #TIPO=as.factor(TIPO)
)]

## Random Forest para determinar quais variáveis são mais importantes

# Random Forest
rf_model <- randomForest(PERIGO ~ .
                         , data = base_PERIGO, importance = TRUE, ntree = 50)

# Plotando a importância das variáveis do modelo
varImpPlot(rf_model,main="Importância das variáveis - PERIGO")

# Ajustando a árvore de decisão
model <- tree(PERIGO ~ INTERVALO_VMA 
              + INCLINACAO 
              + Peso 
              + Linha
              , data = base_PERIGO)

# Taxa de erro de classificação = 0.2487
summary(model)

# Taxa de PERIGO = 0.50
base_PERIGO[,.(PORCENTAGEM_PERIGO = sum(PERIGO=="SIM")/.N)]

# Visualizando a árvore (TRUE <-  ;  FALSE ->)
plot(model)
text(model,pretty=0)


# Avaliando a performance

set.seed(3)

# Usando 70% da base para treinar o modelo
train <-  sample(1:nrow(base_PERIGO), ceiling(0.7*nrow(base_PERIGO)))
base_PERIGO.test <-  base_PERIGO[- train,]
PERIGO.test <- base_PERIGO[- train,PERIGO]

# Ajustado o modelo nos dados de treinamento
tree.model <-  tree(PERIGO ~ INTERVALO_VMA 
                    + INCLINACAO 
                    + Peso 
                    + Linha,base_PERIGO ,subset = train )

# Fazendo a previsão dos dados de teste
tree.predict <- predict(tree.model,base_PERIGO.test,type ="class")
table(tree.predict,PERIGO.test)

plot(tree.model)
text(tree.model,pretty=0)

# Prunando a árvore
cv.tree(tree.model,FUN =prune.misclass)


#### EFICIENTE ####

train <-  sample(1:nrow(base_modelo), ceiling(0.7*nrow(base_modelo)))
base_modelo <-  base_modelo[- train,]

base_EFICIENTE <- base_modelo[,.( #PERIGO=as.factor(PERIGO),
  EFICIENTE=as.factor(EFICIENTE),
  INCLINACAO,
  #cd_locomotiva=as.factor(cd_locomotiva),
  Linha=as.factor(Linha),
  #Prefixo=as.factor(Prefixo),
  Peso,
  Eixos=as.factor(Eixos),
  INTERVALO_VMA=as.factor(INTERVALO_VMA),
  #INTERVALO_PERFIL=as.factor(INTERVALO_PERFIL),
  PERIODO=as.factor(PERIODO)#,
  #TIPO=as.factor(TIPO)
)]

## Random Forest para determinar quais variáveis são mais importantes

# Random Forest
rf_model <- randomForest(EFICIENTE ~ .
                         , data = base_EFICIENTE, importance = TRUE, ntree = 4)

# Plotando a importância das variáveis do modelo
varImpPlot(rf_model,main="Importância das variáveis - EFICIENTE")

# Ajustando a árvore de decisão
model <- tree(EFICIENTE ~ INTERVALO_VMA 
              + INCLINACAO 
              + Peso 
              + Linha
              + Eixos
              , data = base_EFICIENTE)

# Taxa de erro de classificação = 0.2522
summary(model)

prune <- prune.misclass(model,best =7)
plot(prune)
text(prune, pretty =0)

# Taxa de PERIGO = 0.4552503
base_EFICIENTE[,.(PORCENTAGEM_EFICIENTE = sum(EFICIENTE=="NÃO")/.N)]

# Visualizando a árvore (TRUE <-  ;  FALSE ->)
plot(model)
text(model,pretty=0)


# Avaliando a performance

set.seed(3)

# Usando 70% da base para treinar o modelo
train <-  sample(1:nrow(base_EFICIENTE), ceiling(0.7*nrow(base_EFICIENTE)))
base_EFICIENTE.test <-  base_EFICIENTE[- train,]
EFICIENTE.test <- base_EFICIENTE[- train,EFICIENTE]

# Ajustado o modelo nos dados de treinamento
tree.model <- tree(EFICIENTE ~ INTERVALO_VMA 
                    + INCLINACAO 
                    + Peso 
                    + Linha
                    + Eixos,base_EFICIENTE ,subset = train)

# Fazendo a previsão dos dados de teste
tree.predict <- predict(tree.model,base_EFICIENTE.test,type ="class")
table(tree.predict,EFICIENTE.test)

plot(tree.model)
text(tree.model,pretty=0)


### Removando tudo

rm(list=ls())



