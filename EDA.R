####################
# EDA PNLP
####################
setwd("~/Kaggle/PNLP")

library(dplyr)
library(data.table)
library(plotly)
library(tidyr)
source("helper_pnlp.R")
Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales

# Leyendo datos y limpiando
df <- singleRead("data/train.csv")
head(df, 10)
glimpse(df)
df$filename <- NULL

df[, c("q1Clean", "q2Clean") := list(cleanText(question1, removeExtraWords = tm::stopwords("en")),
    cleanText(question2, removeExtraWords = tm::stopwords("en")))]

# EDA
table_NA(df) # No se observa datos vacíos
summary(df)

df$is_duplicate2 <- "Distinto"
df[is_duplicate == 1]$is_duplicate2 <- "Duplicados"
sizePlot(df$is_duplicate2) # el 37% de los datos contienen duplicados

df$wordCountQ1 <- stringr::str_count(df$question1, pattern = "\\S+")
df$wordCountQ2 <- stringr::str_count(df$question2, pattern = "\\S+")
df %>% plot_ly(alpha = 0.6) %>%
    add_histogram(x = ~wordCountQ1) %>%
    add_histogram(x = ~wordCountQ2) %>%
    layout(barmode = "overlay") # prácticamente la misma distribución
df %>% slice(which.max(wordCountQ1)) # La pregunta 1 más larga (125 caracteres)
df %>% slice(which.max(wordCountQ2)) # La pregunta 2 más larga (237 caracteres)

nubePalabras(df$q1Clean) # nube de palabras unigrama Q1
nubePalabras(df$q1Clean, ngram = 2) # nube de palabras unigrama Q2

nubePalabras(df$q2Clean) # nube de palabras unigrama Q1
nubePalabras(df$q2Clean, ngram = 2) # nube de palabras unigrama Q2
