########################
# Modelling  
#######################
setwd("~/Kaggle/PNLP")
setwd("U:/Fabian/Proyectos/PNLP")

source("helper_pnlp.R")
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales

# Leyendo training 
train <- singleRead("data/train_sample_features.csv")
test <- singleRead("data/test_features.csv")

train$filename <- NULL
test$filename <- test$question1 <- test$question2 <- NULL

glimpse(train) # mirada rapida similar a str()
glimpse(test) # mirada rapida similar a str()

# Modelo básico ---------------------------------------------------------
prediction_basic <- sizePlot(train$is_duplicate) # el 37% de los datos contienen duplicados
predicted <- prediction_basic$perc[2]/100
LogLossBinary(train$is_duplicate, predicted)

# Submit solo 1
test <- test %>%
    select(test_id) %>% 
    mutate(is_duplicate = 0)
fwrite(test, file = "Submit/solo0.csv")

# Submit solo 1
test <- test %>%
    select(test_id) %>% 
    mutate(is_duplicate = predicted)
fwrite(test, file = "Submit/modelBasico.csv")

# Importancia Features -----------------------------------------------------
# Filtración de features
trainTask <- makeClassifTask(data = train[, c(6, 9:19)],target = "is_duplicate", positive = "1")
trainTask

# Dummy target for test
test$is_duplicate <- sample(0:1,size = nrow(test),replace = T)
testTask <- makeClassifTask(id = as.character(test$test_id), data = test)

# normalize the variables
trainTask <- normalizeFeatures(trainTask, method = "standardize")
testTask <- normalizeFeatures(testTask, method = "standardize")

# Feature importance
filterTrain <- generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
plotFilterValues(filterTrain, n.show = 20)
plotFilterValuesGGVIS(filterTrain)

# trainTask <- dropFeatures(task = trainTask, features = c("charDiff","wordDiff", "fuzz_partial_token_set_ratio"))
# Regresión Logística -----------------------------------------------------
# logistic regression
logLearner <- makeLearner("classif.logreg", predict.type = "prob")

# cross validation (cv) accuracy
cv.logistic <- crossval(learner = logLearner, task = trainTask, iters = 10,
    stratify = TRUE, measures = list(mcc, acc, f1, auc, logloss), show.info = T)

cv.logistic$aggr # promedio de las métricas en los 10 kfolds
cv.logistic$measures.test # Valores en cada tirada

# Entrenar Modelo
fmodel <- train(logLearner, trainTask)
getLearnerModel(fmodel)

# predict on test data
fpmodel <- predict(fmodel, testTask)

#create submission file
submit <- data.frame(test_id = fpmodel$data, is_duplicate = fpmodel$data$prob.1)
write.csv(submit, "Submit/completeLogReg.csv",row.names = F)



