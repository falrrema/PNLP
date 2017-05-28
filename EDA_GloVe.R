####################
# EDA GLOVE
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
prep_fun <- function(text, stopWords = NULL) {
    text <- cleanText(text, removeNum = F, encode = F, stemming = T, removeExtraWords=NULL)
    return(text)
}

# Se ocupa un espacio vectorial común para la construcción de vectores de palabras (word vectors)
stackDF <- rbind(data.table(ids = df$qid1, pregunta = df$question1), 
    data.table(ids = df$qid2, pregunta = df$question2))
stackDF <- stackDF[!duplicated(stackDF$ids)] # elimino los duplicados

# Create iterator over tokens
tokens <- space_tokenizer(stackDF$pregunta)

# Create vocabulary. Terms will be unigrams (simple words).
it <- itoken(stackDF$pregunta, preprocessor = prep_fun, tokenizer = space_tokenizer)
vocab <- create_vocabulary(it)

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab, 
    # don't vectorize input
    grow_dtm = FALSE, 
    # use window of 5 for context words
    skip_grams_window = 10L)
tcm <- create_tcm(it, vectorizer)

# Implementación de GloVe -------------------------------------------------
glove <- GlobalVectors$new(word_vectors_size = 100, vocabulary = vocab, x_max = 10)
glove$fit(tcm, n_iter = 100)
word_vectors <- glove$get_word_vectors() # Crea vectores de palabras

token1 <- train$question1 %>% prep_fun() %>% word_tokenizer() # Se preparan los tokens de preguntas
token2 <- train$question2 %>% prep_fun() %>% word_tokenizer()

word_vectors_token1 <- lapply(token1, function(x) { # Se rescata los vectors de palabras
    bool <- dimnames(word_vectors)[[1]] %in% x
    wv <- word_vectors[bool, , drop = FALSE]
})

word_vectors_token2 <- lapply(token2, function(x) { 
    bool <- dimnames(word_vectors)[[1]] %in% x
    wv <- word_vectors[bool, , drop = FALSE]
})

simGlove <- sapply(1:length(word_vectors_token1), function(x) { # Se determina la simulitud de preguntas
    a <- (abs(word_vectors_token1[[x]]) %>% apply(1, sum) %>% sum)
    b <- (abs(word_vectors_token2[[x]]) %>% apply(1, sum) %>% sum)
    simM <- (a - b)^2
})

# Chequeo
select(train, is_duplicate) %>% mutate(simGlove = simGlove)
sam <- train %>% 
    mutate(simGlove = simGlove)
    
sam %>% group_by(is_duplicate) %>% 
    summarise(simGloveMean = mean(simGlove), simGloveMed = median(simGlove))

sam %$% 
    wilcox.test(simGlove ~ is_duplicate)
boxplot(simGlove~is_duplicate)

