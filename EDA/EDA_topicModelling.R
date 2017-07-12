########################
# Topic Modelling EDA  
#######################
setwd("~/Dropbox/PNLP")
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales
options(scipen = 99999)
source("keyFunctions.R")

# Leyendo datos 
df <- fread("data/train.csv")
stop_words <- prep_fun(tm::stopwords("en"))

# iterizing
stackDF <- rbind(data.table(id = df$qid1, pregunta = df$question1), 
    data.table(id = df$qid2, pregunta = df$question2))
stackDF <- stackDF[!duplicated(stackDF$id)] # elimino los duplicados
it <- itoken(stackDF$pregunta, preprocessor = prep_fun, tokenizer = word_tokenizer)

# Vocabulary, vectorizer and DTM
vocab <- create_vocabulary(it, stopwords = stop_words)
vocab <- prune_vocabulary(vocab, term_count_min = 5L)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# Finding Topic Number
tn <- topicNumber(docTermMatrix = dtm, maxTopics = 50, interval = 2, n_cores = 7) 


#

lda_model <- text2vec::LDA$new(n_topics = 10, vocabulary = vocab, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr <- lda_model$fit_transform(dtm, n_iter = 1000, convergence_tol = 0.01, check_convergence_every_n = 10)
lda_model$plot()

lda_model
