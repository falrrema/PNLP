######################
# Helper PNLP script
######################

if (!require("plotly")) install.packages("plotly"); library(plotly)
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("readxl")) install.packages("readxl"); library(readxl)
if (!require("wordcloud")) install.packages("wordcloud"); library(wordcloud)
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)
if (!require("tm")) install.packages("tm"); library(tm)
if (!require("ngram")) install.packages("ngram"); library(ngram)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("ROCR")) install.packages("ROCR"); library(ROCR)
if (!require("parallel")) install.packages("parallel"); library(parallel)
if (!require("pacman")) install.packages("pacman"); library(pacman)
if (!require("gofastr")) install.packages("gofastr"); library(gofastr)
if (!require("ldatuning")) install.packages("ldatuning"); library(ldatuning)
if (!require("scales")) install.packages("scales"); library(scales)
if (!require("fuzzywuzzyR")) install.packages("fuzzywuzzyR"); library(fuzzywuzzyR)
if (!require("tidytext")) install.packages("tidytext"); library(tidytext)
if (!require("gclus")) install.packages("gclus"); library(gclus)
if (!require("mlr")) install.packages("mlr"); library(mlr)
# if (!require("FSelector")) install.packages("FSelector")

pacman::p_load(ggplot2, topicmodels, Rmpfr)

Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales

# Missingness --------------------------------------------------------------

removeColumns_NA <- function(data, upperBound = 0) {
    data <- data.frame(data)
    names.column <- names(data)
    list.na <- unlist(lapply(names.column, function(t) sum(is.na(data[t]))))
    missing.data <- data.frame(column = names(data), number.na = list.na, row.names = NULL, stringsAsFactors = F)
    data.rows <- nrow(data)
    missing.data$percent <- missing.data$number.na/data.rows*100
    columns.for.elimination <- missing.data$column[which(missing.data$percent >= percent.missing)] # column names
    columns.for.elimination <- which(names(data) %in% columns.for.elimination) # column numbers in the dataset
    data[, c(columns.for.elimination)] <- list(NULL)
    return(data)
}

# Barplot of % missingness for each column of the data set
visualize_NA <- function(data, percent.show = 0) {
    missing.data <- table_NA(data)
    missing.data$percent <- missing.data$number.na/nrow(data)*100
    
    p1 <-  plot_ly(missing.data, x = ~column, y = ~percent, type = "bar", marker = list(color = ~percent)) %>%
        layout(margin = list(b = 120))
    print(p1)
    return(p1)
} 

table_NA <- function(data) {
    data <- data.frame(data)
    names.column <- names(data)
    list.na <- unlist(lapply(names.column, function(t) sum(is.na(data[t]))))
    missing.data <- data.frame(column = names(data), number.na = list.na, row.names = NULL, stringsAsFactors = F)
    data.rows <- nrow(data)
    missing.data$percent <- missing.data$number.na/data.rows*100
    
    missing.data.order <- missing.data[order(missing.data$number.na, decreasing = T),]
    return(missing.data.order)
} 



# CleanText ---------------------------------------------------------------

# "removeMostPunctuation" is the function that allows flexible punctuation removal. Those puntuation marks you want to preserve
# are needed to be pass as a vector, for example, c("@", "#"). By default this value is NULL, which the functions then 
# goes back to base "removePunctuation()". 

removeMostPunctuation<- function (text, preserveWhich = NULL) { # The functionality is base in tagging, removing and detagging. 
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

cleanText <- function(whateverText, columnNames = F, removeNum = T, encode = T, lowercase = T, preservePunct = NULL, removeExtraWords = NULL) {
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

PNLPcleanText <- function(text, remove_stop_words = TRUE){
    # Clean the text, with the option to remove stop_words
    text = tolower(text)
    # Clean the text
    # text = gsub("[^A-Za-z0-9]", " ", text)
    text = gsub("what's", "", text)
    text = gsub("\'s", " ", text)
    text = gsub("\'ve", " have ", text)
    text = gsub("can't", "cannot ", text)
    text = gsub("n't", " not ", text)
    text = gsub("I'm", "I am", text)
    text = gsub(" m ", " am ", text)
    text = gsub("\'re", " are ", text)
    text = gsub("\'d", " would ", text)
    text = gsub("\'ll", " will ", text)
    text = gsub("60k", " 60000 ", text)
    text = gsub(" e g ", " eg ", text)
    text = gsub(" b g ", " bg ", text)
    #text = gsub("\0s", "0", text)
    text = gsub(" 9 11 ", "911", text)
    text = gsub("e-mail", "email", text)
    text = gsub("\\s{2,}", " ", text)
    text = gsub("quikly", "quickly", text)
    text = gsub(" usa ", " America ", text)
    text = gsub(" u s ", " America ", text)
    text = gsub(" uk ", " England ", text)
    text = gsub("imrovement", "improvement", text)
    text = gsub("intially", "initially", text)
    text = gsub(" dms ", "direct messages ", text)  
    text = gsub("demonitization", "demonetization", text) 
    text = gsub("actived", "active", text)
    text = gsub("kms", " kilometers ", text)
    text = gsub("KMs", " kilometers ", text)
    text = gsub(" cs ", " computer science ", text) 
    text = gsub(" upvotes ", " up votes ", text)
    text = gsub(" iPhone ", " phone ", text)
    #text = gsub("\0rs ", " rs ", text)
    text = gsub("calender", "calendar", text)
    text = gsub("ios", "operating system", text)
    text = gsub("programing", "programming", text)
    text = gsub("bestfriend", "best friend", text)
    text = gsub("dna", "DNA", text)
    text = gsub("III", "3", text) 
    text = gsub("the us", "America", text)
    text = gsub(" J K ", " JK ", text)
    
    # Remove punctuation from text
    # text = removePunctuation(text)
    
    stop_words = c('the','a','an','and','but','if','or','because','as','what',
        'which','this','that','these','those','then','just','so','than','such','both',
        'through','about','for','is','of','while','during','to','What','Which','Is','If',
        'While','This')
    stop_words = c(stop_words, tolower(tm::stopwords("en")))
    # Optionally, remove stop words
    if (remove_stop_words) text <- removeWords(text, stop_words)
    
    # Cleaning white spaces
    text %<>% stripWhitespace() %>% trimws()
    
    return(text)
}


# Multifile reading -------------------------------------------------------

singleRead <- function(file, type = "csv") {
    if (type == "csv" | type == "txt") {
        out <- fread(file, showProgress = T, encoding = "UTF-8")
    } else if (type == "xls" | type == "xlsx") {
        out <- read_excel(file)
        out <- out[-nrow(out),]
        out <- data.table(out)
    } else {
        stop("Must be .csv or .txt or .xls or .xlsx, please choose from 'type'")
    }
    out$filename <- file
    return(out)
}

groupReading <- function(filesFolder, type = "csv", pattern = NULL) {
    files <- filesFolder
    if(!is.null(pattern)) { # Revisa si se paso un string a 'patrón', si es TRUE entonces filtra por el patrón
        if(!is.character(pattern)) {
            print("Only strings as pattern")
            break
        }
        files <- filesFolder[grepl(pattern, filesFolder)]
    }
    dataMerge <- dplyr::bind_rows(lapply(files, singleRead, type = type))
    dataMerge <- data.table(dataMerge)
    return(dataMerge)
}   



# Nube de palabras --------------------------------------------------------

# La siguiente funcion permite crear un word cloud alimentando solamente un vector de textos no procesados
# Se puede espeficicar la frecuencia de corte de palabras, el mínimo de frecuencia para aparecer en la nube
# Requiere esta funcion que haya en el entorno un archivo llamado stopwords.es para incorporarlo en el procesamiento y limpieza
# Se agregaron 3 opciones adicionales, vector_eliminacion, stopwords.es y removePunctuation.
# Vector_eliminacion recibe cualquier vector de palabras para eliminar o no considerar en la nube
# stopwords.es es booleano y si es verdadero requerirá que exista el archivo stopwords_es.txt en el working directory
# Si remove_puctuation = T entonces va ejecutar esa función, sino se la salta. Esto es pensando cuando se quiera
# preservar algún signo de punctuación.

nubePalabras <- function(whateverText, ngram = 1, minFreq = 0, removeExtraWords = NULL, removePunct = T, sparcity = 0.99, encode = T, title = NULL, maxWords = 50) {
    if (encode == T) {
        whateverText <- iconv(whateverText, to = "UTF-8")
        whateverText <- iconv(whateverText, "UTF-8", "ASCII//TRANSLIT", sub="") 
    }    
    if (removePunct == T) {
        whateverText <- removePunctuation(whateverText)
    }
    whateverText <- tolower(whateverText)
    if (!is.null(removeExtraWords)) {
        removeExtraWords <- paste0(removeExtraWords, collapse = "|")
        whateverText <- gsub(removeExtraWords, "", whateverText)
    }
    wordsFreq <- getWordFrequency(whateverText, nGram = ngram)
    pal <- RColorBrewer::brewer.pal(8,"Dark2")
    wordcloud::wordcloud(wordsFreq$ngrams, wordsFreq$freq, colors=pal, min.freq = minFreq, random.order=F, max.words = maxWords)
    if (!is.null(title)) {
        text(x=0.5, y=1, paste("Marca", title), font=2, cex = 2)
    }
    return(head(wordsFreq, 20))
}

getWordFrequency <- function(whateverText, nGram = 1) {
    whateverText <- paste0(whateverText, collapse = " ")
    ng <- ngram::ngram(whateverText, nGram)
    return(get.phrasetable(ng))
}



# Text Mining ---------------------------------------------------------

numeroMenciones <- function(keywords, textos) {
    table <- data.frame(key = keywords, menciones = 0)
    table$menciones <- sapply(keywords, function(t) sum(grepl(t, textos, fixed = T)))
    table <- table[order(-table$menciones),]
    return(table)
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

# Visualizaciones ---------------------------------------------------------

sizePlot <- function(categoryColumn, type = "bar") {
    table <- data.frame(category = categoryColumn) %>% group_by(category) %>% 
        summarise(count = n()) %>% arrange(desc(count)) %>%
        mutate(perc = round(count/sum(count)*100, 2))
    
    if (type == "bar") {
        text <- list()
        for (i in seq_len(nrow(table))) {
            m <- table[i, ]
            text[[i]] <- list(
                x = table$count[i] + max(table$count)*0.05,
                y = table$category[i],
                text = paste0("<b>", m$perc, "%</b>"),
                showarrow = F
            )
        }
        p <- plot_ly(table, x = ~count, y = ~reorder(category,count), type = "bar", orientation = "h", marker = list(color = ~count), hoverinfo = "text",
            text = paste0("Volumen de categoría: ", table$count, " (", table$perc, "%)")) %>% 
            layout(margin = list(l = 200), xaxis = list(title = "Frecuencia"), yaxis = list(title = ""), 
                annotations = text)
        print(p)
        return(table)
    }
    else if (type == "pie") {
        p <- plot_ly(table, labels = ~category, values = ~count, type = "pie", outsidetextfont = list(size = 3), 
            insidetextfont = list(size = 18, color = "White"),
            marker = list(colors = "Set2")) %>%
            layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                showlegend = TRUE)
        print(p)
        return(table)
    } else {
        stop("choose between 'bar' or 'pie' chart")
    }
}

timeseriesPlot <- function(data, timeColumn, time = "month", fillOption = NA) {
    .timeColumn <- col_name(substitute(timeColumn))
    
    if(!is.Date(data[[.timeColumn]])) {data[[.timeColumn]] <- ymd_hms(data[[.timeColumn]])} 
    meses <- data.frame(mes = c("Enero", "Febrero", "Marzo","Abril","Mayo","Junio",
        "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"), 
        mes.num = 1:12)
    
    if (time == "day") {
        data$timeCut <- as.Date(data[[.timeColumn]], format = "%Y-%m-%d")
    } else if (time == "week") {
        data$timeCut <- as.Date(cut(data[[.timeColumn]], breaks = "week", start.on.monday = T))
    } else if (time == "month") {
        data$timeCut <- as.Date(cut(data[[.timeColumn]], breaks = "month"))
    } else if (time == "year") {
        data$timeCut <- lubridate::year(data[[.timeColumn]])
    }
    
    if (is.na(fillOption) | length(unique(data[[fillOption]])) == 1) {
        table <- data %>% group_by(timeCut) %>% summarise(count = n()) 
        p <- plot_ly(table, x = ~timeCut, y = ~count, type = "bar", hoverinfo = "text", 
            text = ~paste0(str_to_title(months(table$timeCut)), " ", year(table$timeCut),"<br>", "n: ", table$count),
            marker = list(color = ~count)) %>%  
            layout(barmode = "stack", yaxis = list(title = "Frecuencia"), 
                xaxis = list(title = "", type = "category"), hovermode="closest", margin = list(b = 150))
    } else {
        data$fill <- data[[fillOption]]
        table <- data %>% group_by(timeCut, fill) %>% summarise(count = n()) %>% ungroup %>%
            arrange(timeCut) %>% mutate(timeCut = factor(timeCut))
        p <- plot_ly(table, x = ~timeCut, y = ~count, split = ~fill, type = "bar", hoverinfo = "text", 
            text = ~paste0(str_to_title(months(ymd(table$timeCut))), " ", year(table$timeCut),"<br>", "n: ", table$count)) %>%  
            layout(barmode = "stack", yaxis = list(title = "Frecuencia"), 
                xaxis = list(title = "", type = "category"), hovermode="closest", margin = list(b = 150))
    }
    print(p)
    return(p)
}


# Evaluación métricas ----------------------------------------------------------
LogLossBinary <- function(actual, predicted, eps = 1e-15) { # función que ocupa Kaggle para evaluar
    predicted = pmin(pmax(predicted, eps), 1-eps) - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
    return(predicted)
}


# utils -------------------------------------------------------------------
# Para la entrega de columnas como objeto y no string
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

# Para crear un DTM
makeDTM <- function(data, textColumn = NULL, id = NULL, wordLength = Inf) {
    if (is.null(textColumn)) {
        stop("Please indicate the text column as a character string")
    }
    if (!is.null(id)) {
        cat("Passing indicated IDs to corpus")
        corpus <- VCorpus(DataframeSource(data), 
            readerControl = list(reader = readTabular(mapping = list(content = textColumn, 
                id = id))))
        docTermMatrix <- DocumentTermMatrix(corpus, control=list(wordLengths=c(1,wordLength)))
        return(list(dtm = docTermMatrix, corpus = corpus))
    } else{
        cat("Creating corpus without tailored IDs")
        corpus <- VCorpus(VectorSource(data[[textColumn]]))
        docTermMatrix <- DocumentTermMatrix(corpus)
        return(list(dtm = docTermMatrix, corpus = corpus))
    }
}

# Topic Modelling ---------------------------------------------------------
# Find Optimal Number of Topics
# Iteratively produces models and then compares the harmonic mean of the log 
# likelihoods in a graphical output.

optimal_k <- function(x, max.k = 30, harmonic.mean = TRUE, 
    control = if (harmonic.mean) list(burnin = 500, iter = 1000, keep = 100) else  NULL,
    method = if (harmonic.mean) "Gibbs" else "VEM", verbose = TRUE, drop.seed = TRUE, ...){
    
    if (isTRUE(drop.seed)){
        control[["seed"]] <- NULL
    }
    
    if (isTRUE(harmonic.mean)) {
        optimal_k1(x, max.k = max.k, control = control, method = method, verbose = verbose, ...)
    } else {
        optimal_k2(x, max.k = max.k, control = control, method = method, ...)
    }
}

plot.optimal_k1 <- function(x, ...){
    
    y <- attributes(x)[["k_dataframe"]]
    y <- y[y[["k"]] == as.numeric(x), ]
    
    ggplot2::ggplot(attributes(x)[["k_dataframe"]], ggplot2::aes_string(x="k", y="harmonic_mean")) + 
        ggplot2::xlab(sprintf("Number of Topics (Optimal Number: %s)", as.numeric(x))) + 
        ggplot2::ylab("Harmonic Mean of Log Likelihood") + 
        ggplot2::geom_smooth(method = "loess", fill=NA) + 
        geom_point(data=y, color="red", fill=NA, size = 6, shape = 21) +
        ggplot2::geom_line(size=1) + 
        ggplot2::theme_bw()  + 
        ggplot2::theme(
            axis.title.x = ggplot2::element_text(vjust = -0.25, size = 14),
            axis.title.y = ggplot2::element_text(size = 14, angle=90)
        ) 
}

print.optimal_k <- function(x, ...){
    
    print(graphics::plot(x))
    
}

optimal_k1 <- function(x, max.k = 30, 
    control = list(burnin = 500, iter = 1000, keep = 100), method = "Gibbs", 
    verbose = TRUE, ...){
    
    
    if (max.k > 20) {
        message("\nGrab a cup of coffee this could take a while...\n")
        flush.console()
    }
    
    tic <- Sys.time()
    v <- rep(NA, floor(max.k/10))
    dat <- data.frame(k = v, time = v)
    end <- data.frame(k = max.k^2)
    
    hm_many <- sapply(2:max.k, function(k){
        if (k %% 10 == 0){
            time <- as.numeric(difftime(Sys.time(), tic, units = "mins"))        
            dat[k/10, 1:2] <<- c(k^2, time)          
            if (k/10 > 1) {
                fit <- with(dat, lm(time~k))
                pred <- predict(fit, end) - time
                if (pred < 0) pred <- 0
                est <- paste0("; Remaining: ~", time2char(pred), " mins")
            } else {
                est <- ""
            }
            cur <- format(Sys.time(), format="%I:%M:%S")
            elapsed <- time2char(time)
            #gsub("^0+", "", as.character(round(as.numeric(difftime(Sys.time(), tic, units = "mins")), 1)))
            cat(sprintf("%s of %s iterations (Current: %s; Elapsed: %s mins%s)\n", k, max.k, cur, elapsed, est)); flush.console()
        }
        burnin <- control[["burnin"]]
        keep <- control[["keep"]]
        if (is.null(burnin) | is.null(keep)) stop("Supply burnin & keep to control")
        fitted <- topicmodels::LDA(x, k = k, method = method, control = control)
        logLiks <- fitted@logLiks[-c(1:(burnin/keep))]
        harmonicMean(logLiks)
    })
    
    out <- c(2:max.k)[which.max(hm_many)]
    if (which.max(hm_many) == max.k) warning("Optimal K is last value; suggest increasing `max.k`")
    class(out) <- c("optimal_k", "optimal_k1", class(out))
    attributes(out)[["k_dataframe"]] <- data.frame(
        k = 2:max.k, 
        harmonic_mean = hm_many
    )
    if (isTRUE(verbose)) cat(sprintf("Optimal number of topics = %s\n",as.numeric(out)))
    out
}

harmonicMean <- function(logLikelihoods, precision=2000L) {
    llMed <- Rmpfr::median(logLikelihoods)
    as.double(llMed - log(Rmpfr::mean(exp(-Rmpfr::mpfr(logLikelihoods, prec = precision) + llMed))))
}

optimal_k2 <- function(x, max.k = 30, control = NULL, method = "VEM", ...){
    
    if (max.k > 20) {
        message("\nGrab a cup of coffee this could take a while...\n")
        flush.console()
    }
    
    tic <- Sys.time()
    v <- rep(NA, floor(max.k/10))
    dat <- data.frame(k = v, time = v)
    end <- data.frame(k = max.k^2)
    
    best_model <- lapply(seq(2, max.k, by=1), function(k){
        if (k %% 10 == 0){
            time <- as.numeric(difftime(Sys.time(), tic, units = "mins"))        
            dat[k/10, 1:2] <<- c(k^2, time)            
            if (k/10 > 1) {
                fit <- with(dat, lm(time~k))
                est <- paste0("; Remaining: ~", time2char(predict(fit, end) - time), " mins")
            } else {
                est <- ""
            }
            cur <- format(Sys.time(), format="%I:%M:%S")
            elapsed <- time2char(time)
            #gsub("^0+", "", as.character(round(as.numeric(difftime(Sys.time(), tic, units = "mins")), 1)))
            cat(sprintf("%s of %s iterations (Current: %s; Elapsed: %s mins%s)\n", k, max.k, cur, elapsed, est)); flush.console()
        }
        topicmodels::LDA(x, k = k, method = method, control = control, ...)
    })
    
    out <- data.frame(
        k = c(2:max.k), 
        logLik = sapply(best_model, logLik)
    )
    
    class(out) <- c("optimal_k", "optimal_k2", "data.frame")
    out
}

time2char <- function(x){
    x <- as.character(round(x, 1))
    if (identical("0", x)) return(x)
    gsub("^0+", "", x)
}

plot.optimal_k2 <- function(x, ...){
    
    ggplot2::ggplot(x, ggplot2::aes_string(x="k", y="logLik")) + 
        ggplot2::xlab("Number of Topics") + 
        ggplot2::ylab("Log Likelihood") + 
        ggplot2::geom_smooth(size=.8, se=FALSE, method="loess") + 
        ggplot2::geom_line(size=1) + 
        ggplot2::theme_bw()  + 
        ggplot2::theme(
            axis.title.x = ggplot2::element_text(vjust = -0.25, size = 14),
            axis.title.y = ggplot2::element_text(size = 14, angle=90)
        )
    
}

# Otro método para determinar el número de tópicos óptimos
ldaTuningResult <- function(dtm, from = 2, to = 40, by = 1, cores = 1L){
    result <- FindTopicsNumber(
        dtm,
        topics = seq(from = from, to = to, by = by),
        metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
        method = "Gibbs",
        control = list(seed = 77),
        mc.cores = cores,
        verbose = TRUE
    )
    result <- mutate_each(result, funs(rescale), Griffiths2004:Deveaud2014)
    result <- result %>% mutate(meanMax = (Griffiths2004 + Deveaud2014)/2)
    result <- result %>% mutate(meanMin = (CaoJuan2009 + Arun2010)/2)
    result$meanMaxLoess <- with(result, fitted(loess(meanMax ~ topics)))
    result$meanMinLoess <- with(result, fitted(loess(meanMin ~ topics)))
    
    x.min <- result[which.max(result$meanMaxLoess),]$topic
    x.max <- result[which.min(result$meanMinLoess),]$topics
    y1.max <- result[which.max(result$meanMaxLoess),]$meanMaxLoess
    y1.min <- result[which.max(result$meanMaxLoess),]$meanMinLoess 
    y2.max <- result[which.min(result$meanMinLoess),]$meanMaxLoess
    y2.min <- result[which.min(result$meanMinLoess),]$meanMinLoess 
    
    p <- plot_ly(result, x = ~topics, y = ~meanMax, name = "meanMax", type = "scatter") %>%
        add_trace(x = ~topics, y = ~meanMin, name = "meanMin", type = "scatter") %>%
        add_lines(y = ~result$meanMaxLoess,line = list(color = 'dodgerblue')) %>%
        add_lines(y = ~result$meanMinLoess,line = list(color = 'orange')) %>%
        add_lines(x = x.min, y = c(y1.min, y1.max),line = list(color = 'dodgerblue'), showlegend = F) %>%
        add_lines(x = x.max, y = c(y2.min, y2.max),line = list(color = 'orange'), showlegend = F)
    print(p)
    return(result)
}

trWordDistribution <- function(lda_model) {
    post <- posterior(lda_model)
    cor_mat <- cor(t(post[["terms"]]))
    cor_mat[ cor_mat < .05 ] <- 0
    diag(cor_mat) <- 0
    
    graph <- graph.adjacency(cor_mat, weighted=TRUE, mode="lower")
    graph <- delete.edges(graph, E(graph)[ weight < 0.05])
    
    sums <- colSums(post[["topics"]])
    multiplier <- solve(median(sums), 33)
    
    E(graph)$edge.width <- E(graph)$weight
    V(graph)$label <- paste("Topic", V(graph))
    V(graph)$size <- sums * multiplier
    
    par(mar=c(0, 0, 3, 0))
    set.seed(110)
    plot.igraph(graph, edge.width = E(graph)$edge.width, 
        edge.color = "dodgerblue", vertex.color = "salmon", 
        vertex.frame.color = NA, vertex.label.color = "white")
    title("Strength Between Topics Based On Word Probabilities", cex.main=.8)
}