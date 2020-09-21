# Lista de Exercícios Parte 2 - Capítulo 11

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/FCD/BigDataRAzure/Cap12")
getwd()


# Regressão Linear
# Definição do Problema: Prever as notas dos alunos com base em diversas métricas
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# Carregando o dataset
df <- read.csv2('estudantes.csv')

# install.packages("ggplot2")
install.packages("ggthemes")
# install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)

## Exploratory Analysis:
# step 1: Summary.
summary(df)
head(df)
str(df)
any(is.na(df))
### Aqui tem uma infinidade!
ex1 = table(df$sex,df$internet)
ex1
barplot(ex1,col = df$sex, legend = rownames(ex1))
####

# Verifying Outliers
# Step1 = Divide variables between factors and numerics
indices = sapply(df, is.numeric)
df_numeric = df[,indices]
dim(df_numeric)
boxplot(df_numeric[,1:8])
# Removing outliers. Part 1
df = df %>% filter(age < 20)
boxplot(df_numeric$Fedu)
df = df %>% filter(Fedu > 1)
boxplot(df_numeric$traveltime)
df = df %>% filter(traveltime < 2)
boxplot(df_numeric$studytime)
df = df %>% filter(studytime < 3 & studytime > 1)
boxplot(df_numeric$failures)
df = df %>% filter(failures < 1)
boxplot(df_numeric$famrel)
df = df %>% filter(famrel > 3)
boxplot(df_numeric$freetime)
df = df %>% filter(freetime > 2)

# Removing Outliers part 2:

boxplot(df_numeric[,9:16])
boxplot(df_numeric$Dalc)
boxplot(df_numeric$freetime)
df = df %>% filter(Dalc < 3)
boxplot(df_numeric$G2)
df = df %>% filter(G2 > 0)
df = df %>% filter(G3 > 0)
df = df %>% filter(absences < 30)

# Verifying Correlation:
cor(df_numeric) # Não gera uma boa visualização.
library(corrplot)
correlations = cor(df_numeric)
corrplot(correlations, method = 'color')

# Removing variable with weak correlation
df$Dalc = NULL


# Creating the ML

#nil: Amostragem
# step 1: Creating samples: 70% training. 30% Test
install.packages("caTools")
library(caTools)
?sample.split
amostra = sample.split(df$Fedu,0.70) # Qualquer variável numérica serve
amostra

treino = subset(df,amostra=T)
teste = subset(df,amostra=F)

modelo = lm(G3~., data = treino)
modelo
summary(modelo)

# Analysing residuals
residuais = residuals(modelo)
res = as.data.frame(residuais)
  
ggplot(res, aes(residuais)) +  
  geom_histogram(fill = 'blue', 
                 alpha = 0.5, 
                 binwidth = 1)

#### Time for predicting!

predictions = predict(modelo,treino)
Results = data.frame(treino$G3,predictions)
View(Results)  
plot(Results)

# nil: Errors Metrics: MSE,RMSE, R-Squared
# MSE
mse <- mean((Results$treino.G3 - Results$predictions)^2)
print(mse)

# RMSE
rmse <- mse^0.5
rmse

# Calculando R Squared
SSE = sum((Results$predictions - Results$treino.G3)^2)
SST = sum((mean(df$G3) - Results$treino.G3)^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2

