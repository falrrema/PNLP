########################
# Benchmark EDA  
#######################
# This script studies benchmark results for models previously selected by "EDA_learningCurve.R"
setwd("~/Dropbox/ProyectosDS/PNLP")
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales
options(scipen = 99999)
options( java.parameters = "-Xmx16g") # for ML algorithms that use JAVA
source("keyFunctions.R")
library(mlr) # if not installed mlrDependencies() 

# Reading training 
train <- fread("data/train_features.csv")
train[, c("id","qid1", "qid2", "question1", "question2") := NULL]
trainTask <- taskingProcess(train)

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


