#########################
# Key functions for PNLP
########################

installThesePackages <- function() {
    list.of.packages <- c("tm", "SnowballC", "parallelMap", "pbapply", "dplyr", "data.table", "text2vec", 
                          "magrittr", "tidyr", "ggplot2", "dtplyr", "topicmodels", "tokenizers")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
}

mlrDependencies <- function() {
  cat("Go for a coffee this could take a while...")
  devtools::install_github("mlr-org/mlr", dependencies = c("Depends", "Imports", "Suggests"))
}

loadThesePackages <- function() {
    lib <- list("tm", "SnowballC", "parallelMap", "pbapply", "dplyr", "data.table", "text2vec", 
             "magrittr", "tidyr", "ggplot2", "dtplyr", "topicmodels", "tokenizers", "mlr")
    p <- lapply(lib, require, character.only = TRUE)
}

# CleanText ---------------------------------------------------------------
# "removeMostPunctuation" is the function that allows flexible punctuation removal. Those puntuation marks you want to preserve
# are needed to be pass as a vector, for example, c("@", "#"). By default this value is NULL, which the functions then 
# goes back to base "removePunctuation()". 

removeMostPunctuation <- function (text, preserveWhich = NULL) { # The functionality is base in tagging, removing and detagging. 
    if (!is.null(preserveWhich)) {
        for (i in 1:length(preserveWhich)) {
            replacement <- paste("000", i, sep = "")
            text <- gsub(preserveWhich[i], replacement, text)
        }
        text <- removePunctuation(text)
        for (i in 1:length(preserveWhich)) {
            replacement <- paste("000", i, sep = "")
            text <- gsub(replacement, preserveWhich[i], text)
        }
    } else {
        text <- removePunctuation(text)
    }
    return(text)
}  

# "cleanText" is a function that:
# - eliminates accentuation and strange characters (emoticons).
# - converts every strings to lower case strings
# - it eliminates all puntuations if the parameter 'preservePunct' is NULL
# - Preserves puntuations if pass a vector of punctuations to the parameter 'preservePunct'
# - strips whitespace from the string
# - if 'columnNames' is TRUE, then formatting for column names is applied. This means strings more than one word are 
# capitalize, excepto the first one. Strings that have words of 3 or less characters are eliminated, but some are kept
# for better understanding of the variable (id, url, app, etc). 
# Sys.setlocale(locale="es_ES.UTF-8") # spanish reading format

cleanText <- function(whateverText, columnNames = F, removeNum = T, encode = T, lowercase = T, preservePunct = NULL, stemming = F, removeExtraWords = NULL) {
    # iconv() eliminates accentuation and strange characters. Accentuation are turned to apostrophes.
    if (encode == T) {
        # whateverText <- iconv(whateverText, to = "UTF-8")
        whateverText <- iconv(whateverText, "UTF-8", "ASCII//TRANSLIT", sub="") 
    }
    if (!is.null(preservePunct)) { 
        if (!"all" %in% preservePunct) { #  Calling 'removeMostPunctuation' function if not "all"
            whateverText <- removeMostPunctuation(whateverText, preserveWhich = preservePunct) 
        }
    } else {
        whateverText <- tm::removePunctuation(whateverText) 
    }
    if (stemming == T) {
        whateverText <- stemDocument(whateverText, language = "english")
    }
    if (lowercase == T) {
        whateverText <- tolower(whateverText) # lower case function
    }
    if (removeNum == T) {
        whateverText <- removeNumbers(whateverText) # remove numbers
    }
    if (!is.null(removeExtraWords)) {
        whateverText <- 
            tm::removeWords(whateverText, removeExtraWords)
    }
    whateverText <- stripWhitespace(whateverText) # Trim extra whitespace between.
    whateverText <- trimws(whateverText) # Trim extra whitespace at the beginning and the end.
    if (columnNames == T) { 
        whateverText <- sapply(whateverText, function(t) {
            str = unlist(strsplit(t, split = " "))
            if (length(str) >= 3) {
                isKeyword = str %in% c("app", "url", "id", "me")
                isSmall = !(nchar(str) <= 3)
                isNumber = grepl("[[:digit:]]",str)
                keywordOrSmallorNumber = (isKeyword | isSmall | isNumber)
                str = str[keywordOrSmallorNumber]
            }
            str = stringr::str_to_title(str)
            str[1] = tolower(str[1])
            str = paste0(str, collapse = "")
        })
    }
    return(whateverText)
}

# Empaquetamiento de CleanText
prep_fun <- function(text, stop_words = NULL) {
    text <- cleanText(text, removeNum = F, encode = F, stemming = T, removeExtraWords = stop_words)
    return(text)
}

# Feature creation --------------------------------------------------------
wordShareIndex <- function(data, string1, string2) {
    .string1 <- col_name(substitute(string1))
    .string2 <- col_name(substitute(string2))
    
    string_pair <- paste(data[[.string1]], data[[.string2]], sep = "<eos>")
    
    # Parallel computation
    n_cores <- detectCores() # Calculate the number of cores
    cat("Paralelizando.... ocupando", n_cores, "núcleos")
    core_clusters <- makeCluster(n_cores) # Initiate cluster
    
    list_pairs <- parLapply(core_clusters, string_pair, function(t) {
        split_pairs <- unlist(strsplit(t, split = "<eos>"))
        return(as.list(split_pairs))
    })
    
    wordShare <- parSapply(core_clusters, list_pairs, function(t) {
        word_list <- lapply(t, function(k) unlist(strsplit(trimws(k), split = "\\W")))
        intersect_length <- length(Reduce(intersect, word_list))
        pair_similarity <- intersect_length*2/sum(unlist(lapply(word_list, length)))
        return(pair_similarity)
    })
    
    stopCluster(core_clusters) # End cluster usage
    return(round(wordShare, 4))
}

getSizeFeatures <- function(data, id, string1, string2, nCores = 0) {
    .id <- col_name(substitute(id))
    .string1 <- col_name(substitute(string1))
    .string2 <- col_name(substitute(string2))

    stopW <- prep_fun(tm::stopwords("en"))
    data <- data.table(data)
    
    cat("Cleaning Stopwords", "\n")
    dt <- data %>% select_(.id, .string1, .string2) %>% 
      tidyr::gather_(key_col = "preg", value_col= "text", gather_cols = c(.string1, .string2))
    n_cores <- detectCores() - nCores # Calculate the number of cores
    cat("Using", n_cores, "cores", "\n")
    core_clusters <- makeCluster(n_cores, type = "FORK") # Initiate cluster
    dt$textClean <- pbapply::pbsapply(dt$text, function(t) {
        clean <- prep_fun(t, stopW) # Sin stopwords
        return(clean)
    }, cl = core_clusters)
    stopCluster(core_clusters) # End cluster usage
    
    dt <- dt %>% 
        select(-text) %>% 
        tidyr::spread(preg, textClean) %>% 
        rename_(question1Clean = .string1, question2Clean = .string2)
    dt <- data %>% 
      select_(.id, .string1, .string2) %>% 
      inner_join(dt, by = .id)
    
    # Word difference
    wordCount <- function(text) {
        stringr::str_count(text, pattern = "\\S+") # conteo palabras 
    }
    
    cat("Calculating word difference", "\n")
    mutate_call1 <- lazyeval::interp(~ abs(a - b), a = as.name(.string1), b = as.name(.string2))
    mutate_call2 <- lazyeval::interp(~ abs(a - b), a = as.name("question1Clean"), b = as.name("question2Clean"))
    
    wordDiff <- dt %>% select_(quote(-id)) %>% 
        mutate_each(funs(wordCount)) %>% 
        transmute_(word_diff = setNames(mutate_call1, "word_diff"),
                   word_diff_stop = setNames(mutate_call2, "word_diff_stop"))
    
    # Character difference
    cat("Calculating character difference", "\n")
    charDiff <- dt %>% select_(quote(-id)) %>% 
      mutate_each(funs(nchar)) %>% 
      transmute_(char_diff = setNames(mutate_call1, "char_diff"),
                 char_diff_stop = setNames(mutate_call2, "char_diff_stop"))
    
    # Juntando diferencias
    data <- data.table(data)
    data <- cbind(data, wordDiff, charDiff)
    return(data)
}

getDistFeatures <- function(data, string1, string2, method = c("cosine", "jaccard"), it = NULL) {
    .string1 <- col_name(substitute(string1))
    .string2 <- col_name(substitute(string2))
    
    tok_fun <- word_tokenizer
    stop_words <- prep_fun(tm::stopwords("en"))
    
    # Common vector space
    stackDF <- rbind(data.table(pregunta = data[[.string1]]), 
        data.table(pregunta = data[[.string2]]))
    stackDF <- stackDF[!duplicated(stackDF$pregunta)] # elimino los duplicados
    it <- itoken(stackDF$pregunta, preprocessor = prep_fun, tokenizer = tok_fun)
    
    # make tokens
    it1 <- itoken(data[[.string1]], preprocessor = prep_fun, tokenizer = tok_fun)
    it2 <- itoken(data[[.string2]], preprocessor = prep_fun, tokenizer = tok_fun)
    
    # Make vocabularies
    message("Making unigram vocab", "\n")
    v_uni <- create_vocabulary(it)
    message("\n","Making unigram without stopwords vocab")
    v_uni_stop <- create_vocabulary(it, stopwords = stop_words)
    message("\n", "Making bigram vocab")
    v_bi <- create_vocabulary(it, ngram = c(1L, 2L))
    message("\n", "Making bigram without stopwords vocab")
    v_bi_stop <- create_vocabulary(it, ngram = c(1L, 2L), stopwords = stop_words)
    
    # Make vectorizers
    vectorizer_uni <- vocab_vectorizer(v_uni)
    vectorizer_uni_stop <- vocab_vectorizer(v_uni_stop)
    vectorizer_bi <- vocab_vectorizer(v_bi)
    vectorizer_bi_stop <- vocab_vectorizer(v_bi_stop)
    
    if ("jaccard" %in% method) {
        # Jaccard Distances
        message("\n", "calculating unigram Jaccard Distance")
        data$jacSim_uni <- jacCosineDist(it1, it2, vectorizer_uni, distMethod = "jaccard")
        message("\n", "calculating unigram w/stopwords Jaccard Distance")
        data$jacSim_uni_stop <- jacCosineDist(it1, it2, vectorizer_uni_stop, distMethod = "jaccard")
        message("\n", "calculating bigram Jaccard Distance")
        data$jacSim_bi <- jacCosineDist(it1, it2, vectorizer_bi, distMethod = "jaccard")
        message("\n", "calculating bigram w/stopwords Jaccard Distance")
        data$jacSim_bi_stop <- jacCosineDist(it1, it2, vectorizer_bi_stop, distMethod = "jaccard")
    } 
    
    if ("cosine" %in% method) {
        # Cosine Distances
        message("\n", "calculating unigram Cosine Distance")
        data$cosSim_uni <- jacCosineDist(it1, it2, vectorizer_uni, distMethod = "cosine", norm = "l2")
        message("\n", "calculating unigram w/stopwords Cosine Distance")
        data$cosSim_uni_stop <- jacCosineDist(it1, it2, vectorizer_uni_stop, distMethod = "cosine", norm = "l2")
        message("\n", "calculating bigram Cosine Distance")
        data$cosSim_bi <- jacCosineDist(it1, it2, vectorizer_bi, distMethod = "cosine", norm = "l2")
        message("\n", "calculating bigram w/stopwords Cosine Distance")
        data$cosSim_uni_stop <- jacCosineDist(it1, it2, vectorizer_bi_stop, distMethod = "cosine", norm = "l2")
        
        # With TFIDF
        message("\n", "calculating unigram with TFIDF Cosine Distance")
        data$cosSim_uni_tfidf <- jacCosineDist(it1, it2, vectorizer_uni, distMethod = "cosine", norm = "l2", tfidf = T)
        message("\n", "calculating bigram with TFIDF Cosine Distance")
        data$cosSim_bi_tfidf <- jacCosineDist(it1, it2, vectorizer_bi, distMethod = "cosine", norm = "l2", tfidf = T)
        
        # With TFIDF and LSA
        message("\n", "calculating unigram with TFIDF and LSA Cosine Distance")
        data$cosSim_uni_tfidf_lsa <- jacCosineDist(it1, it2, vectorizer_uni, distMethod = "cosine", norm = "l2", tfidf = T, lsa = T)
        message("\n", "calculating bigram with TFIDF and LSA Cosine Distance")
        data$cosSim_bi_tfidf_lsa <- jacCosineDist(it1, it2, vectorizer_bi, distMethod = "cosine", norm = "l2", tfidf = T, lsa = T)
    }
    return(data)
}

jacCosineDist <- function(it1, it2, vect, distMethod, norm = "none", tfidf = FALSE, lsa = FALSE) {
    dtm1 <- create_dtm(it1, vect, verbose = F)
    dtm2 <- create_dtm(it2, vect, verbose = F)
    
    if (tfidf == TRUE) {
        tfidf <- TfIdf$new()
        dtm1 <- fit_transform(dtm1, tfidf)
        dtm2 <- fit_transform(dtm2, tfidf)
    }
    
    if (lsa == TRUE) {
        lsa = LSA$new(n_topics = 100)
        dtm1 <- fit_transform(dtm1, lsa)
        dtm2 <- fit_transform(dtm2, lsa)
    }
    
    dist <- psim2(dtm1, dtm2, method = distMethod, norm = norm)
    return(dist)
}

getVocabularyTokens <- function(file = "data/test.csv", string1, string2) {
  .string1 <- col_name(substitute(string1))
  .string2 <- col_name(substitute(string2))
  stop_words <- prep_fun(tm::stopwords("en"))
  data <- fread(file)
  
  # Common vector space
  stackDF <- rbind(data.table(pregunta = data[[.string1]]), 
                   data.table(pregunta = data[[.string2]]))
  stackDF <- stackDF[!duplicated(stackDF$pregunta)] # elimino los duplicados
  
  # Create iterator over tokens
  message("\n", "Making iterator over tokens")
  tokens <- space_tokenizer(stackDF$pregunta)
  it <- itoken(stackDF$pregunta, preprocessor = prep_fun, tokenizer = space_tokenizer)
  
  # Create vocabulary. Terms will be unigrams (simple words).
  message("\n", "Making vocabulary")
  vocab <- create_vocabulary(it)
  vocab <- prune_vocabulary(vocab, term_count_min = 5L)
  return(list(vocabulary = vocab, iTokens = it))
}

getWordVectors<- function(vocab, it) {
  # Make Vectorizers
  message("\n", "Making vectorizers")
  vectorizerVS <- vocab_vectorizer(vocab, 
                                   # don't vectorize input
                                   grow_dtm = FALSE, 
                                   # use window of 5 for context words
                                   skip_grams_window = 5L)
  
  # make Glove
  message("\n", "Implementing GloVe")
  tcm <- create_tcm(it, vectorizerVS)
  glove <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
  glove$fit(tcm, n_iter = 100)
  word_vectors <- glove$get_word_vectors() # Crea vectores de palabras
  
  return(word_vectors)
}
  
getGloveFeature <- function(data, string1, string2, word_vectors = NULL, vocab = NULL, nCores = detectCores()-1) {
    .string1 <- col_name(substitute(string1))
    .string2 <- col_name(substitute(string2))
    data <- data.table(data)
    
    if (is.null(word_vectors)) {
      # Making word_vectors
      vocabToken <- getVocabularyTokens(file = "data/test.csv", question1, question2)
      vocab <- vocabToken$vocabulary
      word_vectors <- getWordVectors(vocab = vocab, it = vocabToken$iToken)
    } else if (is.null(vocab)) {
      vocabToken <- getVocabularyTokens(file = "data/test.csv", question1, question2)
      vocab <- vocabToken$vocabulary
    }
    # Get Vector Sum
    message("\n","Getting vector Sum feature")

    it1 <- parPrep_proc(data[[.string1]], nCores = nCores)
    it2 <- parPrep_proc(data[[.string2]], nCores = nCores, verbose = F)
    
    n_cores <- nCores # Calculate the number of cores
    cat("Using", n_cores, "cores for matrix sumation", "\n")
    core_clusters <- makeCluster(n_cores) # Initiate cluster
    clusterExport(core_clusters, varlist = c("word_vectors","it1"), envir = environment())
    
    sumMatrix1 <- pbapply::pbsapply(1:length(it1), function(t) {
      return(sum(abs(word_vectors[dimnames(word_vectors)[[1]] %in% it1[[t]][[1]], ,drop = FALSE])))
    }, cl = core_clusters)
    stopCluster(core_clusters) # End cluster usage
    
    core_clusters <- makeCluster(n_cores) # Initiate cluster
    clusterExport(core_clusters, varlist = c("word_vectors","it2"), envir = environment())
    sumMatrix2 <- pbapply::pbsapply(1:length(it2), function(t) {
      return(sum(abs(word_vectors[dimnames(word_vectors)[[1]] %in% it2[[t]][[1]], ,drop = FALSE])))
    }, cl = core_clusters)
    stopCluster(core_clusters) # End cluster usage
    
    data$vectSum <- (sumMatrix1-sumMatrix2)^2

    # get Relaxed Word Movers Distance
    message("\n","Getting Relaxed Word Movers Distance")
    vectorizerRWMD <- vocab_vectorizer(vocab)
    rwmd_model <- RWMD$new(word_vectors, method = "cosine")
    it1 <- itoken(data[[.string1]] , preprocessor = prep_fun, tokenizer = word_tokenizer)
    it2 <- itoken(data[[.string2]] , preprocessor = prep_fun, tokenizer = word_tokenizer)
    
    dtm1 <- create_dtm(it1, vectorizerRWMD)
    dtm2 <- create_dtm(it2, vectorizerRWMD)
    data$rwmdDist <- rwmd_model$pdist2(dtm1, dtm2)
    return(data)
}
    
parPrep_proc <- function(vectorText, nCores = 0, verbose = T) {
  n_cores <- nCores # Calculate the number of cores
  
  if (verbose == T)  cat("Using", n_cores, "cores for text preprocessing", "\n")
  
  core_clusters <- makeCluster(n_cores) # Initiate cluster
  clusterExport(core_clusters, c("prep_fun", "cleanText", "word_tokenizer"), envir = environment())
  clusterEvalQ(core_clusters, library(tm))
  
  vectorText <- pbapply::pblapply(vectorText, function(t) {
    return(word_tokenizer(prep_fun(t)))
  }, cl = core_clusters)
  stopCluster(core_clusters) # End cluster usage
  return(vectorText)
}


# Feature Selection -------------------------------------------------------

fSelection <- function(data, cv = 10, nCores = 4) {
    task <- taskingProcess(data) 
    resamp <- makeResampleDesc("CV", iters = cv, stratify = T)
    meas <- list(mlr::auc, logloss, brier)
    
    ctrlRandom <- makeFeatSelControlRandom(maxit = 20L)
    ctrlSeq <- makeFeatSelControlSequential(method = "sfs", alpha = 0.02)
    
    parallelStartMulticore(nCores, logging = T)
    cat("\n","Feature importance by Logistic Regression")
    logFeatures <- selectFeatures(learner = "classif.logreg", task = task, resampling = resamp, control = ctrlSeq)
    parallelStop()
    
    parallelStartMulticore(nCores, logging = T)
    cat("\n", "Feature importance by Random Forrest")
    rfFeatures = selectFeatures(learner = "classif.randomForest", task = task, resampling = resamp, control = ctrlRandom)
    parallelStop()
    
    return(list(logFeatures, rfFeatures))
}


# Utilities ---------------------------------------------------------------
col_name <- function (x, default = stop("Please supply column name", call. = FALSE)) {
    if (is.character(x))
        return(x)
    if (identical(x, quote(expr = )))
        return(default)
    if (is.name(x))
        return(as.character(x))
    if (is.null(x))
        return(x)
    stop("Invalid column specification", call. = FALSE)
}

# Determina el número y % de datos vacíos por columnas en un DF
table_NA <- function(data) {
    data <- data.frame(data)
    names_column <- names(data)
    list_na <- sapply(names_column, function(t) sum(is.na(data[t])))
    missing_data <- data.frame(column = names(data), numberNA = list_na, row.names = NULL, stringsAsFactors = F)
    missing_data$percent <- round(missing_data$numberNA/nrow(data)*100, 4)
    missing_dataOrder <- missing_data[order(missing_data$numberNA, decreasing = T),]
    return(missing_dataOrder)
} 

taskingProcess <- function(df, target = "is_duplicate", pos = "1") {
    task <- makeClassifTask(data = df, target = target, positive = pos)
    imp <- mlr::impute(task, classes = list(integer = imputeMean(), numeric = imputeMean()),
        dummy.classes = c("integer","numeric"), dummy.type = "numeric")
    task <- imp$task
    dummy <- names(getTaskData(task))[grepl("dummy", names(getTaskData(task)))]
    task <- dropFeatures(task, dummy)
    task <- normalizeFeatures(task,method = "standardize")
    return(task)
}


# Topic Modelling ---------------------------------------------------------
topicNumber <- function(docTermMatrix, interval = 1, maxTopics = 30, n_cores = detectCores() - 1) {
    # Perform LDA topic modeling
    burnin = 1000
    iter = 1000
    keep = 50
    sequ <- seq(2, maxTopics, interval)
    # Now we need to define a set of possible values of k. What this means is that we will run the LDA model once for each 
    # value of k that we specify. Then in the next step we will find out which value of k maximises the harmonic mean of 
    # the log-likelihood, and then we will settle on that as our final number of topics.
    
    if (n_cores == 0) {
        core_clusters <- NULL
    } else {
        core_clusters <- makeCluster(n_cores, type = "FORK") # Initiate cluster
        clusterExport(cl = core_clusters, varlist = c("sequ", "burnin", "iter", "keep", "maxTopics", "docTermMatrix", topicmodels::LDA), 
            envir = environment())
    }

    fitted_many <- pbapply::pblapply(sequ, function(t) {
        cat("Fitting a topic Model with k =", t, "from", maxTopics, "potential topics")
        cat("\n")
        LDA(docTermMatrix, k = t, method = "Gibbs",
            control = list(burnin = burnin, iter = iter, keep = keep))
    }, cl = core_clusters)
    
    # extract logliks from each topic
    logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin/keep))])
    
    # compute harmonic means
    hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
    
    # The peak of the curve shows us which value of k maximises the harmonic mean of the log-likelihood
    p <- plot_ly(x = sequ, y = hm_many, mode = "markers+lines")
    print(p)
    k <- sequ[which.max(hm_many)]
    return(k)
}
