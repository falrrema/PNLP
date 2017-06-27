########################
# Modelling  
#######################
setwd("~/Dropbox/PNLP")
source("keyFunctions.R")
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales
options(scipen = 99999)
library(mlr)
# mlrDependencies() # If you haven't install mlr before, install every package needed

# Reading training 
train <- fread("data/train_features.csv")
# split <- sample(train$id, nrow(train)*0.1)
# split <- train$id %in% split
train[, c("id","qid1", "qid2", "question1", "question2") := NULL]
# sort(sapply(ls(), function(x) format(object.size(get(x)), unit = 'auto'))) # Determine objects size

# (Almost) all Learners to test
lrns <- listLearners("classif", properties = "prob")$class
lrns <- lrns[!(lrns %in% c("classif.boosting", "classif.bartMachine", 
                           "classif.cforest", "classif.ctree",
                           "classif.evtree", "classif.gausspr", "classif.qda",
                           "classif.extraTrees"))] # too long to train
lrns <- lapply(lrns, function(x) makeLearner(x, predict.type = "prob"))

# Preparation
set_cv <- makeResampleDesc("CV",iters = 3L, stratify = T, predict = "both")
meas <- list(mlr::auc, logloss, brier, timeboth)
trainTask <- taskingProcess(train)
rm(col_name, getWordVectors, table_NA, parPrep_proc, fSelection, getVocabularyTokens, 
   jacCosineDist, removeMostPunctuation,wordShareIndex, parSumMatrix,getSizeFeatures,getGloveFeature,getDistFeatures,train)
gc()

# ¿Which algorithms take the most time? -----------------------------------
# Test with 10% data on all algorithms one by one in parallel
n_cores <- detectCores() # Calculate the number of cores
core_clusters <- makeCluster(n_cores) # Initiate cluster
clusterExport(core_clusters, c("lrns", "trainTask", "split"))
clusterEvalQ(core_clusters, c(library(mlr), library(data.table)))

results <- pbapply::pblapply(lrns, function(t) {
  time <- system.time(mod <- mlr::train(t, trainTask, subset = split))
  dfResult <- data.table(algorithm = t[[1]][[1]], user = time[[1]], systemT = time[[2]], 
                         elapse = time[[3]], size = format(object.size(mod), units = "Mb"))
  print(dfResult, "\n")
  return(dfResult)
})
stopCluster(core_clusters) # End cluster usage
re <- bind_rows(results)
fwrite(re, file = "results-10%-models.csv")


# Learning curve --------------------------------------------------------
parallelStartMulticore(3)
lc <- generateLearningCurveData(learners = lrns, task = trainTask,
                             percs = c(0.1, 0.2, 0.4, 0.7, 1), measures = meas,
                               resampling = set_cv)
parallelStop()
save(lc, file = "Results/LearningCurveMany.R")

# Visualization
plotLearningCurve(lc)
