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
df <- fread("data/test_features.csv")
# train <- fread("data/train_feature.csv")
# df <- fread("data/test.csv")

setDT(df)
# setDT(train)
setkey(df, test_id)
# df <- sample_n(df, nrow(df)/3)

# Similarity distances ----------------------------------------------------
start.time <- Sys.time()
df <- getSizeFeatures(df, question1, question2) # Variables básica de diffLargo
df$wordShare <- wordShareIndex(df, question1, question2) # Variable de % de palabras compartidas
df <- getDistFeatures(df, question1, question2) # Variables de distancia de documentos
end.time <- Sys.time()

start.time <- Sys.time()
df <- getGloveFeature(df, question1, question2)
end.time <- Sys.time()

fwrite(df, file = "data/test_features.csv")
cat("Tiempo estimado de ejecución:", difftime(end.time, start.time, units = c("hours")))

# # Sample Set --------------------------------------------------------------
# # Training
# set.seed(31)
# split <- caTools::sample.split(df$is_duplicate, SplitRatio = 0.8) # 80% de los datos totales
# forTrain <- df[split]
# 
# # test
# test <- df[!split]
# fwrite(test, file = "data/test_features.csv")
# 
# # Validation
# set.seed(32)
# split <- caTools::sample.split(forTrain$is_duplicate, SplitRatio = 0.8) # 80% de los datos train
# train <- forTrain[split]
# val <- forTrain[!split]
# 
# fwrite(test, file = "data/train_features.csv")
# fwrite(val, file = "data/validation_features.csv")
# 
