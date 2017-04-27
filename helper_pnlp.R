######################
# Helper PNLP script
######################

library(plotly)
require(tm)
library(data.table)
library(readxl) 
library(wordcloud)
library(RColorBrewer)
library(tm)
library(ngram)
library(dplyr)
library(ROCR)

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



# Menciones Texto ---------------------------------------------------------

numeroMenciones <- function(keywords, textos) {
    table <- data.frame(key = keywords, menciones = 0)
    table$menciones <- sapply(keywords, function(t) sum(grepl(t, textos, fixed = T)))
    table <- table[order(-table$menciones),]
    return(table)
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
}

?performance
