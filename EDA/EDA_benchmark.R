########################
# Benchmark EDA  
#######################
# This script studies benchmark results for models previously selected by "EDA_learningCurve.R"
setwd("~/Dropbox/ProyectosDS/PNLP")
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales
options(scipen = 99999)
options(java.parameters = "-Xmx16g") # for ML algorithms that use JAVA
source("keyFunctions.R")
loadThesePackages()
library(mlr) # if not installed mlrDependencies() 

# Reading training 
train <- fread("data/train_features.csv")
train[, c("id","qid1", "qid2", "question1", "question2") := NULL]
trainTask <- taskingProcess(train)

# First Filter of selected learners ------------------------------------------
# These learners were selected after running learningCurve.R
# Prep learners
lrns1 <- c("classif.blackboost", "classif.earth", "classif.penalized", 
    "classif.multinom", "classif.mlp", "classif.C50", "classif.RRF", "classif.kknn", 
    "classif.ctree", "classif.nnTrain", "classif.xgboost", "classif.binomial",
    "classif.rotationForest", "classif.ranger", "classif.randomForest",
    "classif.nnet", "classif.gamboost") 
lrns2 <- c("classif.extraTrees", "classif.PART", "classif.h2o.randomForest", "classif.h2o.deeplearning",
    "classif.h2o.gbm") # Algorithms that have problem with parallels
lrns1 <- lapply(lrns1, function(x) makeLearner(x, predict.type = "prob"))
lrns2 <- lapply(lrns2, function(x) makeLearner(x, predict.type = "prob"))

# Resampling strategy, measures and reproducibility
set_cv <- makeResampleDesc("CV", iters = 10L, stratify = T, predict = "both")
meas <- list(mlr::auc, logloss, brier, timeboth)
configureMlr(on.learner.error = "warn")

# Benchmark will be done using 40% of data
sampleIndex <- caret::createDataPartition(train$is_duplicate, p = 0.4, list = FALSE)
sampleTask <- subsetTask(trainTask, subset = sampleIndex)

# Configuring batchtools
library(batchtools)
reg <- makeExperimentRegistry(
    file.dir = "Parallel_batchmark", packages = c("mlr"), seed = 123)
reg$cluster.functions <-  makeClusterFunctionsMulticore()
res <- list(measure.memory = TRUE)
batchmark(learners = lrns1, tasks = sampleTask, resamplings = set_cv, measures = meas, models = FALSE, reg = reg)

# SubmitJobs
submitJobs(reg = reg, resources = res)
bmr1 <- reduceBatchmarkResults(reg = reg)

# Benchmark laggers learners
bmr2 <- benchmark(lrns2, sampleTask, set_cv, meas, models = FALSE)

# Merging benchmark Results
bmr <- mergeBenchmarkResults(list(bmr1, bmr2))
# save(bmr, file = "Results/benchmarkResults.R")

# Result analysis ---------------------------------------------------------
# load(file = "Results/benchmarkResults.R")
plotBMRBoxplots(bmr) # Tree base algorithms come on top
plotBMRBoxplots(bmr, measure = logloss) + ylim(0,1) # H2O.deeplearning is very bad
plotBMRBoxplots(bmr, measure = brier) # Same here, trees still rule

convertBMRToRankMatrix(bmr, auc) %>% 
    data.table(lrns = row.names(.)) %>% arrange(df) # best classif.extraTrees, worst classif.multinom
convertBMRToRankMatrix(bmr, logloss) %>% 
    data.table(lrns = row.names(.)) %>% arrange(df) # best classif.randomForest, worst classif.h2o.deeplearning
convertBMRToRankMatrix(bmr, brier) %>% 
    data.table(lrns = row.names(.)) %>% arrange(df) # best classif.extraTrees, worst classif.h2o.deeplearning

# Do to problems of parallelization, classif.extraTrees and classif.PART will be eliminated form the group
# But classif.extraTrees will be a submission by itself.
# classif.h2o.deeplearning, will be eliminated also because of poor performance

# Second Filter Learners --------------------------------------------------
# Based on the results of batchmark 5 models will be handpicked and benchmark with tunning
configureMlr(on.learner.error = "warn")

# Benchmark will be done using 40% of data
sampleIndex <- caret::createDataPartition(train$is_duplicate, p = 0.4, list = FALSE)
sampleTask <- subsetTask(trainTask, subset = sampleIndex)

# Internal and Outer Resampling strategy, measures and tunning algorithm
cv_inner <- makeResampleDesc("Holdout", stratify = T, predict = "both")
cv_outer <- makeResampleDesc("CV", iters = 3L, stratify = T)
meas <- list(mlr::auc, logloss, brier, timeboth)
ctrl <- makeTuneControlIrace(maxExperiments = 200L)


ps <- list(makeParamSet(
    makeIntegerParam("ntree", lower = 50, upper = 500),
    makeIntegerParam("mtry", lower = 3, upper = 10),
    makeIntegerParam("nodesize", lower = 10, upper = 50)),
makeParamSet(
    makeIntegerParam("nrounds", lower = 200,upper = 600),
    makeIntegerParam("max_depth",lower = 3,upper = 20),
    makeNumericParam("lambda",lower = 0.55,upper = 0.60),
    makeNumericParam("eta", lower = 0.001, upper = 0.5),
    makeNumericParam("subsample", lower = 0.10, upper = 0.80),
    makeNumericParam("min_child_weight", lower = 1,upper = 5),
    makeNumericParam("colsample_bytree", lower = 0.2,upper = 0.8)),
makeParamSet(
    makeIntegerVectorParam("size", lower = 1, upper = 20, len = 1),
    makeIntegerParam("maxit", lower = 100, upper = 500)),
makeParamSet(
    makeIntegerParam("ntree", lower = 100, upper = 1000),
    makeIntegerParam("mtry", lower = 3, upper = 10),
    makeIntegerParam("nodesize", lower = 10, upper = 50)),
makeParamSet(
    makeIntegerParam("degree", lower = 1, upper = 5),
    makeIntegerParam("nprune", lower = 2, upper = 20))
)

lrn <- c("classif.randomForest", "classif.xgboost", "classif.mlp", "classif.RRF", "classif.earth") 
lrn <- lapply(lrn, function(x) makeLearner(x, predict.type = "prob"))
lrns <- vector(length(lrn), mode = "list")
for (i in 1:length(lrn)) {
    lrns[[i]] <- makeTuneWrapper(lrn[[i]], resampling = cv_inner, 
                                 par.set = ps[[i]], control = ctrl, measures = meas)
}

# Configuring batchtools
library(batchtools)
reg <- makeExperimentRegistry(
    file.dir = "Parallel_batchmark_tunning", packages = c("mlr"), seed = 123)
reg$cluster.functions <-  makeClusterFunctionsMulticore()
res <- list(measure.memory = TRUE)
batchmark(learners = lrns, tasks = sampleTask, resamplings = cv_outer, measures = meas, reg = reg)

# SubmitJobs
submitJobs(reg = reg, resources = res)
bmr <- reduceBatchmarkResults(reg = reg)



