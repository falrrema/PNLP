########################
# Learning Curve EDA  
#######################
setwd("~/Dropbox/PNLP")
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales
options(scipen = 99999)
options( java.parameters = "-Xmx16g") # for ML algorithms that use JAVA
source("keyFunctions.R")
# mlrDependencies() # If you haven't install mlr before, install every package needed
library(mlr)

# Reading training 
train <- fread("data/train_features.csv")

# (Almost) all Learners to test
lrns <- listLearners("classif", properties = "prob")$class
lrns <- lrns[(!lrns %in% c("classif.boosting", "classif.evtree", "classif.gausspr", 
                            "classif.bartMachine", "classif.cforest", # to long to train
                           "classif.ada", "classif.qda", "classif.bartMachine", # errors
                           "classif.lqa", "classif.JRip", "classif.neuralnet"))] # not enough memory
lrns <- lapply(lrns, function(x) makeLearner(x, predict.type = "prob"))

# ¿Which algorithms take the most time? -----------------------------------
# Test with 10% data on all algorithms one by one in parallel
split <- sample(train$id, nrow(train)*0.1)
split <- train$id %in% split
train[, c("id","qid1", "qid2", "question1", "question2") := NULL]
trainTask <- taskingProcess(train)

smallSample <- data.table()
for (t in lrns) {
    time <- system.time(mod <- mlr::train(t, trainTask, subset = split))
    dfResult <- data.table(algorithm = t[[1]][[1]], user = time[[1]], systemT = time[[2]], 
        elapse = time[[3]], size = format(object.size(mod), units = "Mb"))
    print(dfResult, "\n")
    smallSample <- rbind(smallSample, dfResult)
}

fwrite(smallSample, file = "Results/results-10%-models.csv")

# Learning curve --------------------------------------------------------
# Preparation
train[, c("id","qid1", "qid2", "question1", "question2") := NULL]
set_cv <- makeResampleDesc("CV", iters = 3L, stratify = T, predict = "both")
meas <- list(mlr::auc, logloss, brier, timeboth)
trainTask <- taskingProcess(train)
configureMlr(on.learner.error = "warn")
gc()

if (!"LearningCurveMany.csv" %in% list.files("Results/")) {
    learningCurve <- data.table()
} else {
    learningCurve <- fread("Results/LearningCurveMany.csv")
}

for (i in lrns) {
  cat("Learning with", i$id, "\n")
  # parallelStartMulticore(4, logging = T)
  lc <- generateLearningCurveData(learners = i, task = trainTask,
                                  percs = c(0.1, 0.2, 0.4, 0.7, 1), measures = meas,
                                  resampling = set_cv)
  # parallelStop()
  gc()
  learningCurve <- rbind(learningCurve, lc$data)
  fwrite(learningCurve, file = "Results/LearningCurveMany.csv")
}


# Result analysis ---------------------------------------------------------
library(plotly)
learningCurve <- fread("Results/LearningCurveMany.csv")

# Filter out those learners that take more than 15 minutes aproximately with 40% of the data
filterLearners <- learningCurve %>% 
    filter(percentage == 0.4, timeboth < 1000, !is.na(logloss)) %$% # Eliminate those that have NA in logloss
    learner
    
# Select those model that performe above than average by AUC
learningCurve %>% 
    filter(learner %in% filterLearners) %>% 
    plot_ly(y = ~auc, type = "box")

filtertopAuc <- learningCurve %>% filter(learner %in% filterLearners, percentage == 1) %>% 
    arrange(desc(auc)) %>% filter(auc > median(auc)) %$% 
    learner

# Visualize learning plot
learningCurve %>% filter(learner %in% filtertopAuc) %>% 
    gather(measures, values, auc:brier) %>% 
    ggplot(aes(x = percentage, y = values, color = learner)) + 
    geom_point() + geom_line() +
    facet_wrap(~ measures, scales = "free")

learningCurve %>% filter(learner %in% filtertopAuc) %>% # by AUC
    plot_ly(x = ~percentage, y = ~auc, split = ~learner, type = "scatter", mode = "lines+markers")

learningCurve %>% filter(learner %in% filtertopAuc) %>% # by Brier
    plot_ly(x = ~percentage, y = ~brier, split = ~learner, type = "scatter", mode = "lines+markers")

learningCurve %>% filter(learner %in% filtertopAuc) %>% # by LogLoss
    plot_ly(x = ~percentage, y = ~brier, split = ~learner, type = "scatter", mode = "lines+markers")

fwrite(learningCurve[learner %in% filtertopAuc], file = "Results/Selected_learners.csv")

# Although SVM algorithms performed well on the data set it took them too long to be included.
# However, classif.ksvm will be a submission on its own.



