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

what <- "train"

# Leyendo datos y transformando
if (what == "train") {
  df <- fread("data/train_features.csv")
} else if (what == "test") {
  df <- fread("data/test_features.csv")
}

# Similarity distances ----------------------------------------------------
start.time <- Sys.time()
df <- getSizeFeatures(df, question1, question2) # Variables básica de diffLargo
df$wordShare <- wordShareIndex(df, question1, question2) # Variable de % de palabras compartidas
df <- getDistFeatures(df, question1, question2) # Variables de distancia de documentos
df <- getGloveFeature(df, question1, question2)
end.time <- Sys.time()

cat("Tiempo estimado de ejecución:", difftime(end.time, start.time, units = c("hours")))


if (what == "test") {
  fwrite(df, file = "data/test_features.csv")
} else if (what == "train"){
  # # Sample Set --------------------------------------------------------------
  # Training
  set.seed(31)
  split <- caTools::sample.split(df$is_duplicate, SplitRatio = 0.8) # 80% de los datos totales
  forTrain <- df[split]

  # trainTest
  trainTest <- df[!split]
  fwrite(trainTest, file = "data/trainTest.csv")

  # Validation
  set.seed(32)
  split <- caTools::sample.split(forTrain$is_duplicate, SplitRatio = 0.8) # 80% de los datos train
  train <- forTrain[split]
  val <- forTrain[!split]

  fwrite(train, file = "data/train_features.csv")
  fwrite(val, file = "data/validation_features.csv")
}

