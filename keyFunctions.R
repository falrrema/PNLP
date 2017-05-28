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
        whateverText <- removePunctuation(whateverText) 
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
            removeWords(whateverText, removeExtraWords)
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
