########################
# Model Tunning 
#######################
setwd("~/Dropbox/PNLP")
source("keyFunctions.R")
Sys.setlocale(locale = "es_ES.UTF-8") # Para visualizar caracteres especiales
mlrDependencies()

# Leyendo training 
val <- fread("data/val_features.csv")
val[, c("id","qid1", "qid2", "question1", "question2") := NULL]
valTask <- taskingProcess(val)

# Setting resampling
set_cv <- makeResampleDesc("CV",iters = 10L, stratify = T)
meas <- list(mlr::auc, logloss, brier)

# Learners
glmLearner <- makeLearner("classif.cvglmnet", predict.type = "prob")
rfLearner <- makeLearner("classif.randomForest", predict.type = "prob")
treeLearner <- makeLearner("classif.rpart", predict.type = "prob")
svmLearner <- makeLearner("classif.svm", predict.type = "prob")
gbmLearner <- makeLearner("classif.gbm", predict.type = "prob")
xgLearner <- makeLearner("classif.xgboost", predict.type = "prob")
bartLearner <- makeLearner("classif.bartMachine", predict.type = "prob")

# RPART
parallelStartSocket(8, show.info = T)

gs <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)
gscontrol <- makeTuneControlGrid()
stune <- tuneParams(learner = treeLearner, resampling = set_cv, 
                    task = valTask, par.set = gs, control = gscontrol, 
                    measures = meas)

# RANDOMFOREST
gs <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
gscontrol <- makeTuneControlRandom(maxit = 100L)
rf_tune <- tuneParams(learner = rfLearner, resampling = set_cv, task = valTask, 
                      par.set = gs, control = gscontrol, measures = meas)

# SVM 
gs <- makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
)
gscontrol <- makeTuneControlGrid()
svmTune <- tuneParams(svmLearner, task = valTask, resampling = set_cv, 
                  par.set = gs, control = gscontrol, measures = meas)

# GBM
gs <- makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)
gscontrol <- makeTuneControlRandom(maxit = 100L)
tune_gbm <- tuneParams(learner = gbmLearner, task = valTask,
                       resampling = set_cv, measures = meas,
                       par.set = gs,control = gscontrol)

# XGBOOST
gs <- makeParamSet(
  makeIntegerParam("nrounds",lower=200,upper=600),
  makeIntegerParam("max_depth",lower=3,upper=20),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)
gscontrol <- makeTuneControlRandom(maxit = 20L) #do 100 iterations
xg_tune <- tuneParams(learner = xgLearner, task = valTask, 
                      resampling = set_cv, measures = meas,
                      par.set = gs, control = gscontrol)

# GLMNET
gs <- makeParamSet(
  makeNumericParam("alpha", lower = 0.1, upper = 1),
  makeIntegerParam("nfolds", lower = 3, upper = 100),
  makeNumericParam("lambda.min.ratio", lower = 0.1, upper = 1)
)
gscontrol <-makeTuneControlGrid() 
glmtune <- tuneParams(glmLearner, task = valTask, resampling = set_cv,
                      par.set = gs, control = gscontrol,measures = meas)
parallelStop()

# BART
getParamSet("classif.bartMachine")

gs <- makeParamSet(
  makeNumericParam("alpha", lower = 0.1, upper = 1),
  makeIntegerParam("nfolds", lower = 3, upper = 100),
  makeNumericParam("lambda.min.ratio", lower = 0.1, upper = 1)
)
gscontrol <-makeTuneControlGrid() 
bartune <- tuneParams(bartLearner, task = valTask, resampling = set_cv,
                      par.set = gs, control = gscontrol, measures = meas)



hyperTunning <- list(stune, rf_tune, svmTune, tune_gbm, glmtune, bartune)
save(hyperTunning, file = "TunningModels_20topfeatures.R")
