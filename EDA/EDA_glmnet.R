#############################
# EDA Feature by Modelling
############################
setwd("~/Dropbox//PNLP")
Sys.setlocale(locale = "es_ES.UTF-8") # Para visualizar caracteres especiales

library(text2vec)
library(data.table)
library(dplyr)
library(magrittr)
library(glmnet)
library(doParallel)
source("keyFunctions.R")

# Leyendo datos y transformando
df <- fread("data/train.csv")
df <- sample_n(df, 100000) 
df %>% count(is_duplicate) %>%
    mutate(prop = n/sum(n)) # Proporción de 0.369 para duplicados

setDT(df)
setkey(df, id)

df[, "q1q2" := paste(question1, question2, sep = " ")]

# Se define una función de preprocesamiento
prep_fun <- function(text) {
    text <- cleanText(text, removeNum = F, encode = F, stemming = T, removeExtraWords = NULL)
    return(text)
}

prep_fun2 <- function(text) {
    text <- cleanText(text, stemming = T, removeExtraWords = NULL, preservePunct = "?")
    return(text)
}

tok_fun <- word_tokenizer
stop_words <- prep_fun(tm::stopwords("en"))

# Tokenization y Vocabulario
it <- itoken(df$q1q2, preprocessor = prep_fun, tokenizer = tok_fun, ids = df$id)
it2 <- itoken(df$q1q2, preprocessor = prep_fun2, tokenizer = tok_fun, ids = df$id)

vocab <- create_vocabulary(it) %>% prune_vocabulary(term_count_min = 5L)
vocabStop <- create_vocabulary(it, stopwords = stop_words) %>% prune_vocabulary(term_count_min = 5L)
vocabBi <- create_vocabulary(it, ngram = c(1L, 2L)) %>% prune_vocabulary(term_count_min = 5L)
vocabBiStop <- create_vocabulary(it, ngram = c(1L, 2L), stopwords = stop_words) %>% prune_vocabulary(term_count_min = 5L)
vocabTri <- create_vocabulary(it, ngram = c(2L, 3L)) %>% prune_vocabulary(term_count_min = 5L)
vocabTriStop <- create_vocabulary(it, ngram = c(2L, 3L), stopwords = stop_words) %>% prune_vocabulary(term_count_min = 5L)
vocabQua <- create_vocabulary(it, ngram = c(3L, 4L)) %>% prune_vocabulary(term_count_min = 5L)
vocabQuaStop <- create_vocabulary(it, ngram = c(3L, 4L), stopwords = stop_words) %>% prune_vocabulary(term_count_min = 5L)
vocab2 <- create_vocabulary(it2) %>% prune_vocabulary(term_count_min = 5L)
vocab2Stop <- create_vocabulary(it2, stopwords = stop_words) %>% prune_vocabulary(term_count_min = 5L)

# Vectorizers and DTM
vectorizer <- vocab_vectorizer(vocab)
vectorizer2 <- vocab_vectorizer(vocab2)
vectorizer2Stop <- vocab_vectorizer(vocab2Stop)
vectorizerStop <- vocab_vectorizer(vocabStop)
vectorizerBi <- vocab_vectorizer(vocabBi)
vectorizerBiStop <- vocab_vectorizer(vocabBiStop)
vectorizerTri <- vocab_vectorizer(vocabTri)
vectorizerTriStop <- vocab_vectorizer(vocabTriStop)
vectorizerQua <- vocab_vectorizer(vocabQua)
vectorizerQuaStop <- vocab_vectorizer(vocabQuaStop)

dtm_its <- create_dtm(it, vectorizer)
dtm_its_stop <- create_dtm(it, vectorizerStop)
dtm_its2 <- create_dtm(it2, vectorizer2)
dtm_its2Stop <- create_dtm(it2, vectorizer2Stop)
dtm_its_bi <- create_dtm(it, vectorizerBi)
dtm_its_bistop <- create_dtm(it, vectorizerBiStop)
dtm_its_tri <- create_dtm(it, vectorizerTri)
dtm_its_triStop <- create_dtm(it, vectorizerTriStop)
dtm_its_Qua <- create_dtm(it, vectorizerQua)
dtm_its_QuaStop <- create_dtm(it, vectorizerQuaStop)

# Normalization

dtm_its_norm <- normalize(dtm_its, "l1")
dtm_its_stop_norm <- normalize(dtm_its_stop, "l1")
dtm_its2_norm <- normalize(dtm_its2, "l1")
dtm_its2_stop_norm <- normalize(dtm_its2Stop, "l1")
dtm_its_stop_norm <- normalize(dtm_its_stop, "l1")
dtm_its_bi_norm <- normalize(dtm_its_bi, "l1")
dtm_its_bistop_norm <- normalize(dtm_its_bistop, "l1")
dtm_its_tri_norm <- normalize(dtm_its_tri, "l1")
dtm_its_triStop_norm <- normalize(dtm_its_triStop, "l1")
dtm_its_Qua_norm <- normalize(dtm_its_Qua, "l1")
dtm_its_QuaStop_norm <- normalize(dtm_its_QuaStop, "l1")

# define tfidf model
tfidf <- TfIdf$new()
dtm_its_norm_tfidf <- fit_transform(dtm_its_norm, tfidf)
dtm_its2_norm_tfidf <- fit_transform(dtm_its2_norm, tfidf)
dtm_its_bi_norm_tfidf <- fit_transform(dtm_its_bi_norm, tfidf)
dtm_its_tri_norm_tfidf <- fit_transform(dtm_its_tri_norm, tfidf)
dtm_its_Qua_norm_tfidf <- fit_transform(dtm_its_QuaStop_norm, tfidf)
dtm_its_bi_norm_tfidf <- fit_transform(dtm_its_bi_norm, tfidf)

# Modelling with glmnet
# Unigram
registerDoParallel(detectCores()) # Basic Model with normalize features
glmnet_it <- cv.glmnet(x = dtm_its_norm, y = df$is_duplicate, family = 'binomial', 
    # L1 penalty
    alpha = 1,
    # interested in the area under ROC curve
    type.measure = "auc", 
    # Parallel
    parallel = T)

registerDoParallel(detectCores()) # Model with normalize features and without stopwords
glmnet_it_norm_stop <- cv.glmnet(x = dtm_its_stop_norm, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with normalize features and with TFIDF
glmnet_it_norm_tfidf <- cv.glmnet(x = dtm_its_norm_tfidf, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with normalize features and without stopwords
glmnet_it2_norm <- cv.glmnet(x = dtm_its2_norm, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with normalize features and with TFIDF
glmnet_it2_norm_stop <- cv.glmnet(x = dtm_its2_stop_norm, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with normalize features and with TFIDF
glmnet_it2_norm_tfidf <- cv.glmnet(x = dtm_its2_norm_tfidf, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

# Unigram - Bigram
registerDoParallel(detectCores()) # Model with bigrams and normalize features
glmnet_it_bi_norm <- cv.glmnet(x = dtm_its_bi_norm, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with bigrams and normalize features without stopwords
glmnet_it_bistop_norm <- cv.glmnet(x = dtm_its_bistop_norm, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with bigrams and normalize features with tfidf
glmnet_it_bi_norm_tfidf <- cv.glmnet(x = dtm_its_bi_norm_tfidf, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

# Bigram - Trigram
registerDoParallel(detectCores()) # Model with normalize features 
glmnet_it_tri_norm <- cv.glmnet(x = dtm_its_tri_norm, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with normalize features and without stopwords 
glmnet_it_tri_stop <- cv.glmnet(x = dtm_its_triStop_norm, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with bigrams and normalize features and without stopwords
glmnet_it_tri_norm_tfidf <- cv.glmnet(x = dtm_its_tri_norm_tfidf, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

# Trigram - Quagram
registerDoParallel(detectCores()) # Model with normalize features 
glmnet_it_qua_norm <- cv.glmnet(x = dtm_its_Qua_norm, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with normalize features and without stopwords 
glmnet_it_qua_stop <- cv.glmnet(x = dtm_its_QuaStop_norm, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

registerDoParallel(detectCores()) # Model with bigrams and normalize features and without stopwords
glmnet_it_qua_norm_tfidf <- cv.glmnet(x = dtm_its_Qua_norm_tfidf, y = df$is_duplicate, family = 'binomial', alpha = 1, type.measure = "auc", parallel = T)

# plot(glmnet_it)
# plot(glmnet_it_stop)
# plot(glmnet_it_bistop)
# plot(glmnet_it_tfidf)
# plot(glmnet_it_stop_tfidf)
# plot(glmnet_it_bistop_tfidf)

models <- ls()[grepl("glmnet", ls())]
stackModels <- lapply(models, get)
names(stackModels) <- models
results <- data.table(models, maxAuc = sapply(models, function(t) round(max(get(t)$cvm), 4)))
serializeModels <- c(stackModels, list(results = results))
serializeModels$results

# df$glmit <- predict(glmnet_it, newx = dtm_its_norm, s = c("lambda.1se","lambda.min"))
# df$glmit_stop <- predict(glmnet_it_stop, newx = dtm_its_stop_norm, s = c("lambda.1se","lambda.min"))
# df$glmit_bistop <- predict(glmnet_it_bistop, newx = dtm_its_bistop_norm, s = c("lambda.1se","lambda.min"))
# df$glmit_tfidf <- predict(glmnet_it_tfidf, newx = dtm_train_norm_tfidf, s = c("lambda.1se","lambda.min"))
# df$glmit_stop_tfidf <- predict(glmnet_it_stop_tfidf, newx = dtm_train_norm_stop_tfidf, s = c("lambda.1se","lambda.min"))
# df$glmit_bistop_tfidf <- predict(glmnet_it_bistop_tfidf, newx = dtm_train_norm_bistop_tfidf, s = c("lambda.1se","lambda.min"))

# df %>% 
#     group_by(is_duplicate) %>% 
#     summarise(mean(glmit), median(glmit))
# boxplot(glmit_bistop_tfidf ~ is_duplicate, data = df, col = c("salmon", "dodgerblue3"))



