########################
# Modelling  
#######################
setwd("~/Dropbox/ProyectosDS/PNLP")
options(java.parameters = "-Xmx30g") # for ML algorithms that use JAVA
source("keyFunctions.R")
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales
options(scipen = 99999)
library(mlr) # mlrDependencies()

# Reading Data and preparing Tasks
train <- fread("data/train_features.csv")
val <- fread("data/val_features.csv")
test <- fread("data/test_features.csv")
test$is_duplicate <- "1"
traintask <- rbind(train, val) %>% select(is_duplicate:rwmdDist) %>% 
    taskingProcess
testTask <- taskingProcess(test[, c("test_id","question1", "question2") := NULL])

# Basic Model -------------------------------------------------------------
lrn <- makeLearner("classif.featureless", predict.type = "prob")

# Tunning
ps <- makeParamSet( 
    makeDiscreteParam("method", values = c("majority", "sample-prior"))
)
ctrl <- makeTuneControlGrid() 
set_cv <- makeResampleDesc("CV",iters = 10L)
tp <- tuneParams(lrn, task = traintask, resampling = set_cv, 
    par.set = ps, control = ctrl, measures = logloss)
final_base <- setHyperPars(learner = lrn, par.vals = tp$x)

# Train and predict
modBase <- train(final_base, traintask)
pred <- predict(mod, task = testTask)

submit <- data.table(test_id = test$test_id, is_duplicate = pred$data$prob.1)
fwrite(submit, file = "Submit/baseModel.csv")

# RRF Trees -------------------------------------------------------------
lrn <- makeLearner("classif.RRF", predict.type = "prob")

# Train and predict
modRRF <- train(lrn, traintask)
pred <- predict(modRRF, task = testTask)

submit <- data.table(test_id = test$test_id, is_duplicate = pred$data$prob.1)
fwrite(submit, file = "Submit/RRFModel.csv")
