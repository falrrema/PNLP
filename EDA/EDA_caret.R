########################
# Modelling with Caret  
#######################
setwd("~/Dropbox/PNLP")
source("keyFunctions.R")
if (!require("AppliedPredictiveModeling")) install.packages("AppliedPredictiveModeling")
if (!require(caret)) install.packages("caret", dependencies = c("Depends", "Suggests"))
library(AppliedPredictiveModeling)
library(caret)
library(doMC)
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales
options(scipen = 99999)
train <- fread("data/train_features.csv")
val <- fread("data/val_features.csv")
train[, c("id","qid1", "qid2", "question1", "question2") := NULL]

# Visualization
featurePlot(x = train[, 7:length(train)], 
    y = factor(train$is_duplicate), 
    scales = list(y = list(relation = "free")),
    plot = "box", 
    layout = c(4,7))

# PreProcessing
proc <- preProcess(train[,-1], method = c("center", "scale", "YeoJohnson", "knnImpute"), verbose = T)
trainProc <- predict(proc, train[,-1])

registerDoMC(cores = 2)
nz <- nearZeroVar(trainProc, saveMetrics = TRUE) # fuzz_partial_token_set_ratio is nzv
correlation <-  cor(trainProc)
highCorr <- sum(abs(correlation[upper.tri(correlation)]) > 0.9)
summary(correlation[upper.tri(correlation)])
highlyCorDescr <- findCorrelation(correlation, cutoff = .9, names = T, verbose = T)
