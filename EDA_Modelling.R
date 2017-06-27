########################
# Modelling  
#######################
setwd("~/Dropbox/PNLP")
source("keyFunctions.R")
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales
options(scipen = 99999)
mlrDependencies()

# Leyendo training 
train <- fread("data/train_features.csv")
val <- fread("data/val_features.csv")
train[, c("id","qid1", "qid2", "question1", "question2") := NULL]
summarizeColumns(train)

trainTask$env$data %>% # select(-vectSum) %>% # we take vectSum which we know is seriously skewed
    tidyr::gather(predictor, values, common_words:rwmdDist) %>% 
    ggplot(aes(x = predictor, y = values, fill = predictor)) + geom_boxplot() 

train %>% select(vectSum) %>% 
    ggplot(aes(x=log(vectSum))) +
    geom_histogram(aes(y=..count..), bins = 100, colour="black", fill = "dodgerblue", alpha = 0.7) +
    geom_density(alpha=.4, colour = "black", size = 1, fill = "salmon")

# Quitando NA y normalizando (automatizando)
trainTask <- makeClassifTask(data = train, target = "is_duplicate", positive = "1")
imp <- mlr::impute(trainTask, classes = list(integer = imputeMean(), numeric = imputeMean()),
    dummy.classes = c("integer","numeric"), dummy.type = "numeric")
trainTask <- imp$task
trainTask <- normalizeFeatures(trainTask,method = "standardize")

# Conglomerado de Modelos -----------------------------------------------------
# Learners
logLearner <- makeLearner("classif.logreg", predict.type = "prob")
glmLearner <- makeLearner("classif.cvglmnet", predict.type = "prob")
rfLearner <- makeLearner("classif.randomForest", predict.type = "prob")
treeLearner <- makeLearner("classif.rpart", predict.type = "prob")
svmLearner <- makeLearner("classif.svm", predict.type = "prob")
gbmLearner <- makeLearner("classif.gbm", predict.type = "prob")
xgLearner <- makeLearner("classif.xgboost", predict.type = "prob")
bartLearner <- makeLearner("classif.bartMachine", predict.type = "prob")

lrns <- list(logLearner, glmLearner, rfLearner, treeLearner, svmLearner, gbmLearner, xgLearner, bartLearner)

## resampling strategy
resamp <- makeResampleDesc("CV", iters = 5, stratify = T)
meas <- list(mlr::auc, logloss, brier)

## Conduct the benchmark experiment with all predictors and no tunning
parallelStartSocket(8, show.info = T)
bmr <- benchmark(lrns, filtered.task, resamplings = resamp, measures = meas)
parallelStop()
save(bmr, file = "Results/benchmark_all_NoTunning.R")

# Feature Selection -------------------------------------------------------
# What combination of best features gives the best benchmark
trainTask <- taskingProcess(sample_n(train, 20000)) 
resamp <- makeResampleDesc("CV", iters = 5, stratify = T)
meas <- list(mlr::auc, logloss, brier)

results <- data.frame(task.id = "", learner.id = "", iter = 0, auc = 0, logloss = 0, brier = 0, nVar = 0, vars = "")
n <- c(2,4,8,12,16,20,24,27)
for (i in n) {
    parallelStartSocket(8, show.info = T)
    filtered.task <- filterFeatures(trainTask, method = "information.gain", abs = i)
    vars <- names(filtered.task$env$data)[!grepl("duplicate", names(filtered.task$env$data))]
    bmr <- benchmark(lrns, filtered.task, resamplings = resamp, measures = meas)
    bmrDF <- as.data.frame(bmr)
    bmrDF$nVar <- i
    bmrDF$vars <- paste(vars, collapse = ",")
    results <- rbind(results, bmrDF)
    print(bmr)
    parallelStop()
}

r2 <- results %>% 
    filter(nVar != 0) %>% 
    group_by(learner.id, nVar) %>% 
    summarise(meanAUC = mean(auc), meanLogLoss = mean(logloss)) %>% 
    arrange(nVar) %>% 
    ungroup

save(r2, file = "Results/bmr_topSubset_features.R")

ggplot(r2, aes(x = nVar, y = meanLogLoss)) + geom_point(shape=1) + geom_line() +
    facet_grid(learner.id ~ .) # Aproximadamente n = 20

ggplot(r2, aes(x = nVar, y = meanAUC)) + geom_point(shape=1) + geom_line() +
    facet_grid(learner.id ~ .) # Aproximadamente n = 20

# Top Features by group
methods <- c("information.gain", "chi.squared")

# FuzzyWuzzy
sample <- sample_n(train, 20000)
byfilter <- sample %>% select(is_duplicate, starts_with("fuzz")) %>% 
    taskingProcess %>% 
    generateFilterValuesData(method = methods)
byLearner <- sample %>% select(is_duplicate, starts_with("fuzz")) %>% fSelection
save(list(byfilter, byLearner), file = "featureImportance_Fuzzy.R")

# Size and Dist
byfilter <- sample %>% select(is_duplicate, common_words, contains("diff"), wordShare, vectSum, rwmdDist) %>% 
    taskingProcess %>% 
    generateFilterValuesData(method = methods)
byLearner <- sample %>% select(is_duplicate, common_words, contains("diff"), wordShare, vectSum, rwmdDist) %>% fSelection
save(list(byfilter, byLearner), file = "featureImportance_Size.R")

# Jaccard Dist
byfilter <- sample %>% select(is_duplicate, contains("jac")) %>% 
    taskingProcess %>% 
    generateFilterValuesData(method = methods)
byLearner <- sample %>% select(is_duplicate, contains("jac")) %>% fSelection
save(list(byfilter, byLearner), file = "featureImportance_Jac.R")

# Cosine Dist
byfilter <- sample %>% select(is_duplicate, contains("cos")) %>% 
    taskingProcess %>% 
    generateFilterValuesData(method = methods)
byLearner <- sample %>% select(is_duplicate, contains("cos")) %>% fSelection
save(list(byfilter, byLearner), file = "featureImportance_Cos.R")



