#################
# Preprocessing
#################
setwd("~/Kaggle/PNLP")
Sys.setlocale(locale = "es_ES.UTF-8") # Para visualizar caracteres especiales

library(text2vec)
library(data.table)
library(dplyr)
library(magrittr)
source("keyFunctions.R")

# Leyendo datos y transformando
df <- fread("data/train.csv")
# df <- fread("data/test.csv")

df$id <- as.numeric(df$id)
setDT(df)
setkey(df, id)
# df <- df[1:1000,]

# Similarity distances ----------------------------------------------------
start.time <- Sys.time()
df <- getSizeFeatures(df, question1, question2) # Variables básica de diffLargo
df$wordShare <- wordShareIndex(df, question1, question2) # Variable de % de palabras compartidas
df <- getDistFeatures(df, question1, question2) # Variables de distancia de documentos
df <- getGloveFeature(df, question1, question2)
end.time <- Sys.time()

cat("Tiempo estimado de ejecución:", end.time - start.time)

# Sample Set --------------------------------------------------------------
# Training
set.seed(31)
split <- caTools::sample.split(df$is_duplicate, SplitRatio = 0.8) # 80% de los datos totales
forTrain <- df[split]

# test
test <- df[!split]
fwrite(test, file = "data/test_features.csv")

# Validation
set.seed(32)
split <- caTools::sample.split(forTrain$is_duplicate, SplitRatio = 0.8) # 80% de los datos train
train <- forTrain[split]
val <- forTrain[!split]

fwrite(test, file = "data/train_features.csv")
fwrite(val, file = "data/validation_features.csv")

