#################
# Preprocessing
#################
setwd("~/Dropbox/PNLP")
Sys.setlocale(locale = "es_ES.UTF-8") # Para visualizar caracteres especiales

library(text2vec)
library(data.table)
library(dplyr)
library(magrittr)
source("keyFunctions.R")

what <- "test"

# Leyendo datos y transformando
if (what == "train") {
  df <- fread("data/train_features.csv")
} else if (what == "test") {
  df <- fread("data/test_features.csv")
}

# Feature extraction ----------------------------------------------------
start.time <- Sys.time()
df <- getSizeFeatures(df, question1, question2, id = id) # Variables básica de diffLargo
df$wordShare <- wordShareIndex(df, question1, question2) # Variable de % de palabras compartidas
df <- getDistFeatures(df, question1, question2) # Variables de distancia de documentos

voc <- getVocabularyTokens(file = "data/test.csv", question1, question2)
wv <- getWordVectors(vocab = voc$vocabulary, it = voc$iTokens)
dr <- getGloveFeature(dr, question1, question2, word_vectors = wv, vocab = voc$vocabulary, nCores = 1)
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

  # Validation
  val <- df[!split]
  fwrite(val, file = "data/val_features.csv")
  fwrite(forTrain, file = "data/train_features.csv")
}

