####################
# EDA Similarities
####################
setwd("~/Kaggle/PNLP")
Sys.setlocale(locale = "es_ES.UTF-8") # Para visualizar caracteres especiales

library(text2vec)
library(data.table)
library(dplyr)
library(magrittr)
source("keyFunctions.R")

# Leyendo datos y transformando
df <- fread("data/train.csv")
df$id <- as.numeric(df$id)
df %>% count(is_duplicate) %>%
    mutate(prop = n/sum(n)) # Proporción de 0.369 para duplicados

setDT(df)
setkey(df, id)

# Sample Set --------------------------------------------------------------
# Obteniendo una muestra para trabajar más rápido
set.seed(31)
split <- caTools::sample.split(df$is_duplicate, SplitRatio = 0.05) # 5% de los datos
df <- df[split]
df %>% count(is_duplicate) %>%
    mutate(prop = n/sum(n)) # Proporción de 0.369 para duplicados, prácticamente identicos 

set.seed(31)
split <- caTools::sample.split(df$is_duplicate, SplitRatio = 0.8) # 80% de los datos para entrenar
train <- df[split]
val <- df[!split]

# Preprocesamiento --------------------------------------------------------
# Se define una función de preprocesamiento
prep_fun <- function(text) {
    text <- cleanText(text, removeNum = F, encode = F, stemming = T, removeExtraWords=NULL)
    return(text)
}
tok_fun <- word_tokenizer
stop_words <- prep_fun(tm::stopwords("en"))

# Se define los dos sets de documentos a comparar, en este caso question1 con question2
it1 <- itoken(train$question1, preprocessor = prep_fun, tokenizer = tok_fun, ids = train$qid1)
it2 <- itoken(train$question2, preprocessor = prep_fun, tokenizer = tok_fun, ids = train$qid2)

# Para comparar documentos se necesita un espacio vectorial común, por lo que se vectorizará 
# por vocabulario del documento madre que es question 1 + question 2
stackDF <- rbind(data.table(ids = df$qid1, pregunta = df$question1), 
    data.table(ids = df$qid2, pregunta = df$question2))
stackDF <- stackDF[!duplicated(stackDF$ids)] # elimino los duplicados

it <- itoken(stackDF$pregunta, preprocessor = prep_fun, tokenizer = tok_fun, ids = stackDF$ids)
v_uni <- create_vocabulary(it)
v_uni_stop <- create_vocabulary(it, stopwords = stop_words)

v_bi <- create_vocabulary(it, ngram = c(1L, 2L))
v_bi_stop <- create_vocabulary(it, ngram = c(1L, 2L), stopwords = stop_words)

vectorizer_uni <- vocab_vectorizer(v_uni)
vectorizer_uni_stop <- vocab_vectorizer(v_uni_stop)
vectorizer_bi <- vocab_vectorizer(v_bi)
vectorizer_bi_stop <- vocab_vectorizer(v_bi_stop)

# Similitud de Jaccard ----------------------------------------------------
# Unigrama
dtm1 <- create_dtm(it1, vectorizer_uni)
dtm2 <- create_dtm(it2, vectorizer_uni)
jacSim_uni <- psim2(dtm1, dtm2, method = "jaccard", norm = "none")

# Unigrama sin stopwords
dtm1 <- create_dtm(it1, vectorizer_uni_stop)
dtm2 <- create_dtm(it2, vectorizer_uni_stop)
jacSim_uni_stop <- psim2(dtm1, dtm2, method = "jaccard", norm = "none")

compare <- data.table(id = train$id, jacSim = jacSim_uni)
compare2 <- data.table(id = train$id, jacSim = jacSim_uni_stop)
identical(compare, compare2) # están ordenados y son iguales

# Bigrama
dtm1 <- create_dtm(it1, vectorizer_bi)
dtm2 <- create_dtm(it2, vectorizer_bi)
jacSim_bi <- psim2(dtm1, dtm2, method = "jaccard", norm = "none")

compare <- data.table(id = train$id, jacSim_uni = jacSim_uni, jacSim_bi = jacSim_bi)
cbind(train[, .(qid1, qid2)], compare) # Calzan perfecto para devolverlo al training set

# Bigrama sin stopwords
dtm1 <- create_dtm(it1, vectorizer_bi_stop)
dtm2 <- create_dtm(it2, vectorizer_bi_stop)
jacSim_bi_stop <- psim2(dtm1, dtm2, method = "jaccard", norm = "none")

# Estadística para Jaccard
train <- cbind(train, jacSim_uni = jacSim_uni, jacSim_uni_stop = jacSim_uni_stop, 
    jacSim_bi = jacSim_bi, jacSim_bi_stop = jacSim_bi_stop) # Todas las nuevas variables son significativas
wilcox.test(jacSim_uni ~ is_duplicate, data = train)
wilcox.test(jacSim_uni_stop ~ is_duplicate, data = train)
wilcox.test(jacSim_bi ~ is_duplicate, data = train)
wilcox.test(jacSim_bi_stop ~ is_duplicate, data = train)

boxplot(jacSim_uni ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))
boxplot(jacSim_uni_stop ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))
boxplot(jacSim_bi ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))
boxplot(jacSim_bi_stop ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))

train %>% group_by(is_duplicate) %>% # Todas las variables muestran promedios diferentes entre 0 y 1
    summarise_each(funs(mean), jacSim_uni:jacSim_bi_tfidf)

# Similitud de Cosine ----------------------------------------------------
# Unigrama
dtm1 <- create_dtm(it1, vectorizer_uni)
dtm2 <- create_dtm(it2, vectorizer_uni)
cosSim_uni <- psim2(dtm1, dtm2, method = "cosine", norm = "l2")

# Unigrama sin stopwords
dtm1 <- create_dtm(it1, vectorizer_uni_stop)
dtm2 <- create_dtm(it2, vectorizer_uni_stop)
cosSim_uni_stop <- psim2(dtm1, dtm2, method = "cosine", norm = "l2")

# Bigrama
dtm1 <- create_dtm(it1, vectorizer_bi)
dtm2 <- create_dtm(it2, vectorizer_bi)
cosSim_bi <- psim2(dtm1, dtm2, method = "cosine", norm = "l2")

# Bigrama sin stopwords
dtm1 <- create_dtm(it1, vectorizer_bi_stop)
dtm2 <- create_dtm(it2, vectorizer_bi_stop)
cosSim_bi_stop <- psim2(dtm1, dtm2, method = "cosine", norm = "l2")

# Unigrama con TFIDF
tfidf <- TfIdf$new()
dtm1 <- create_dtm(it1, vectorizer_uni)
dtm2 <- create_dtm(it2, vectorizer_uni)
dtm_tfidf1 <- fit_transform(dtm1, tfidf)
dtm_tfidf2 <- fit_transform(dtm2, tfidf)
cosSim_uni_tfidf <- psim2(dtm_tfidf1, dtm_tfidf2, method = "cosine", norm = "l2")

# Unigrama con LSA
lsa = LSA$new(n_topics = 100)
dtm1 <- create_dtm(it1, vectorizer_uni)
dtm2 <- create_dtm(it2, vectorizer_uni)
dtm_lsa1 <- fit_transform(dtm1, lsa)
dtm_lsa2 <- fit_transform(dtm2, lsa)
cosSim_uni_lsa <- psim2(dtm_lsa1, dtm_lsa2, method = "cosine", norm = "l2")

# Unigrama con LSA
dtm1 <- create_dtm(it1, vectorizer_uni)
dtm2 <- create_dtm(it2, vectorizer_uni)
dtm_tfidf1_lsa <- fit_transform(dtm1, tfidf) %>% fit_transform(lsa)
dtm_tfidf2_lsa <- fit_transform(dtm2, tfidf) %>% fit_transform(lsa)
cosSim_uni_tfidf2_lsa <- psim2(dtm_tfidf1_lsa, dtm_tfidf2_lsa, method = "cosine", norm = "l2")

# Estadística para Cosine
train <- cbind(train, cosSim_uni = cosSim_uni, cosSim_uni_stop = cosSim_uni_stop, 
    cosSim_bi = cosSim_bi, cosSim_bi_stop = cosSim_bi_stop, 
    cosSim_uni_tfidf = cosSim_uni_tfidf, cosSim_uni_lsa = cosSim_uni_lsa, 
    cosSim_uni_tfidf2_lsa = cosSim_uni_tfidf2_lsa) # Todas las nuevas variables son significativas
train %>% select(is_duplicate, starts_with("cos"))
wilcox.test(cosSim_uni ~ is_duplicate, data = train)
wilcox.test(cosSim_uni_stop ~ is_duplicate, data = train)
wilcox.test(cosSim_bi ~ is_duplicate, data = train)
wilcox.test(cosSim_bi_stop ~ is_duplicate, data = train)
wilcox.test(cosSim_uni_tfidf ~ is_duplicate, data = train)
wilcox.test(cosSim_uni_lsa ~ is_duplicate, data = train)
wilcox.test(cosSim_uni_tfidf2_lsa ~ is_duplicate, data = train)

boxplot(cosSim_uni ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))
boxplot(cosSim_uni_stop ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))
boxplot(cosSim_bi ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))
boxplot(cosSim_bi_stop ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))
boxplot(cosSim_uni_tfidf ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))
boxplot(cosSim_uni_lsa ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))
boxplot(cosSim_uni_tfidf2_lsa ~ is_duplicate, data = train, col = c("salmon", "dodgerblue3"))

train %>% group_by(is_duplicate) %>% # Todas las variables muestran promedios diferentes entre 0 y 1
    summarise_each(funs(mean), 7:13) %>% View

