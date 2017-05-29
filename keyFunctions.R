#########################
# Key functions for PNLP
########################

if (!require("tm")) install.packages("tm"); library(tm)
if (!require("SnowballC")) install.packages("SnowballC"); library(SnowballC)
if (!require("parallel")) install.packages("parallel"); library(parallel)


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
    n_cores <- detectCores() - 1 # Calculate the number of cores
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

getSizeFeatures <- function(data, string1, string2) {
    .string1 <- col_name(substitute(string1))
    .string2 <- col_name(substitute(string2))
    stopW <- prep_fun(tm::stopwords("en"))
    data <- data.table(data)
    
    cat("Cleaning Stopwords", "\n")
    dt <- tidyr::gather(data, preg, text, get(.string1), get(.string2))
    n_cores <- detectCores() - 1 # Calculate the number of cores
    core_clusters <- makeCluster(n_cores, type = "FORK") # Initiate cluster
    dt$textClean <- parSapply(core_clusters, dt$text, function(t) {
        clean <- prep_fun(t, stopW) # Sin stopwords
        return(clean)
    })
    stopCluster(core_clusters) # End cluster usage
    
    dt <- dt %>% 
        select(-text, -qid1, -qid2, -is_duplicate) %>% 
        tidyr::spread(preg, textClean) %>% 
        rename(question1Clean = question1, question2Clean = question2)
    data <- data %>%
        inner_join(dt, by = "id")
    
    # Word difference
    wordCount <- function(text) {
        stringr::str_count(text, pattern = "\\S+") # conteo palabras 
    }
    
    cat("Calculating word difference", "\n")
    wordDiff <- data %>% select(starts_with("question")) %>% 
        mutate_each(funs(wordCount)) %>% 
        transmute(word_diff = abs(question1-question2), word_diff_stop = abs(question1Clean-question2Clean))
    
    # Character difference
    cat("Calculating character difference", "\n")
    charDiff <- data %>% select(starts_with("question")) %>% 
        mutate_each(funs(nchar)) %>% 
        transmute(char_diff = abs(question1-question2), char_diff_stop = abs(question1Clean-question2Clean))
    
    # Juntando diferencias
    data <- data.table(data)
    data <- cbind(data, wordDiff, charDiff)
    data[, c("question1Clean", "question2Clean") := NULL]
    return(data)
}

getDistFeatures <- function(data, string1, string2, method = c("cosine", "jaccard"), ids = c("qid1", "qid2")) {
    .string1 <- col_name(substitute(string1))
    .string2 <- col_name(substitute(string2))
    
    tok_fun <- word_tokenizer
    stop_words <- prep_fun(tm::stopwords("en"))
    
    # Common vector space
    stackDF <- rbind(data.table(ids = data[[ids[1]]], pregunta = data[[.string1]]), 
        data.table(ids = data[[ids[2]]], pregunta = data[[.string2]]))
    stackDF <- stackDF[!duplicated(stackDF$ids)] # elimino los duplicados
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

getGloveFeature <- function(data, string1, string2, ids = c("qid1", "qid2")) {
    .string1 <- col_name(substitute(string1))
    .string2 <- col_name(substitute(string2))
    stop_words <- prep_fun(tm::stopwords("en"))
    
    # Common vector space
    stackDF <- rbind(data.table(ids = data[[ids[1]]], pregunta = data[[.string1]]), 
        data.table(ids = data[[ids[2]]], pregunta = data[[.string2]]))
    stackDF <- stackDF[!duplicated(stackDF$ids)] # elimino los duplicados

    # Create iterator over tokens
    message("\n", "Making iterator over tokens")
    tokens <- space_tokenizer(stackDF$pregunta)
    it <- itoken(stackDF$pregunta, preprocessor = prep_fun, tokenizer = space_tokenizer)
    
    # Create vocabulary. Terms will be unigrams (simple words).
    message("\n", "Making vocabulary")
    vocab <- create_vocabulary(it)
    
    # Make Vectorizers
    message("\n", "Making vectorizers")
    vectorizerVS <- vocab_vectorizer(vocab, 
        # don't vectorize input
        grow_dtm = FALSE, 
        # use window of 5 for context words
        skip_grams_window = 5L)
    vectorizerRWMD <- vocab_vectorizer(vocab)
    
    # make Glove
    message("\n", "Implementing GloVe")
    tcm <- create_tcm(it, vectorizerVS)
    glove <- GlobalVectors$new(word_vectors_size = 100, vocabulary = vocab, x_max = 10)
    glove$fit(tcm, n_iter = 100)
    word_vectors <- glove$get_word_vectors() # Crea vectores de palabras
    
    # Get Vector Sum
    message("\n","Getting vector Sum feature")
    token1 <- data[[.string1]] %>% prep_fun() %>% word_tokenizer() # Se preparan los tokens de preguntas
    token2 <- data[[.string2]] %>% prep_fun() %>% word_tokenizer()
    
    n_cores <- detectCores() - 1 # Calculate the number of cores
    core_clusters <- makeCluster(n_cores, type = "FORK") # Initiate cluster
    word_vectors_token1 <- parLapply(core_clusters, token1, function(x) { # Se resmessagea los vectors de palabras
        bool <- dimnames(word_vectors)[[1]] %in% x
        wv <- word_vectors[bool, , drop = FALSE]
    })
    
    word_vectors_token2 <- parLapply(core_clusters, token2, function(x) { 
        bool <- dimnames(word_vectors)[[1]] %in% x
        wv <- word_vectors[bool, , drop = FALSE]
    })
    stopCluster(core_clusters) # End cluster usage
    
    core_clusters <- makeCluster(n_cores, type = "FORK") # Initiate cluster
    data$vectSum <- parSapply(core_clusters, 1:length(word_vectors_token1), function(x) { # Se determina la simulitud de preguntas
        a <- (abs(word_vectors_token1[[x]]) %>% apply(1, sum) %>% sum)
        b <- (abs(word_vectors_token2[[x]]) %>% apply(1, sum) %>% sum)
        simM <- (a - b)^2
    })
    stopCluster(core_clusters) # End cluster usage
    
    # get Relaxed Word Movers Distance
    message("\n","Getting Relaxed Word Movers Distance")
    rwmd_model <- RWMD$new(word_vectors, method = "cosine")
    it1 <- itoken(data[[.string1]] , preprocessor = prep_fun, tokenizer = word_tokenizer)
    it2 <- itoken(data[[.string2]] , preprocessor = prep_fun, tokenizer = word_tokenizer)
    
    dtm1 <- create_dtm(it1, vectorizerRWMD)
    dtm2 <- create_dtm(it2, vectorizerRWMD)
    data$rwmdDist <- rwmd_model$pdist2(dtm1, dtm2)
    
    return(data)
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
