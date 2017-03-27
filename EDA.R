####################
# EDA PNLP
####################
setwd("~/Kaggle/PNLP")
# El siguiente código es para instalar usefulchetR
# devtools::install_bitbucket("datasciencebond/usefulchetr", auth_user = "fabian.reyes@digitalbond.cl", password = "Mddb2016")
library(usefulchetr)
library(dplyr)
library(data.table)
library(plotly)
library(tidyr)
source("Missingness.R")
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

sizePlot(df$is_duplicate) # el 37% de los datos contienen duplicados

df$wordCountQ1 <- stringr::str_count(df$question1, pattern = "\\S+")
df$wordCountQ2 <- stringr::str_count(df$question2, pattern = "\\S+")
df %>% plot_ly(alpha = 0.6) %>%
    add_histogram(x = ~wordCountQ1) %>%
    add_histogram(x = ~wordCountQ2) %>%
    layout(barmode = "overlay") # prácticamente la misma distribución
df %>% slice(which.max(wordCountQ1)) # La pregunta 1 más larga (125 caracteres)
df %>% slice(which.max(wordCountQ2)) # La pregunta 2 más larga (237 caracteres)

nubePalabras(df$q1Clean, removeExtraWords = tm::stopwords("en")) # nube de palabras unigrama Q1
nubePalabras(df$q1Clean, ngram = 2, removeExtraWords = tm::stopwords("en")) # nube de palabras unigrama Q2

nubePalabras(df$q2Clean, removeExtraWords = tm::stopwords("en")) # nube de palabras unigrama Q1
nubePalabras(df$q2Clean, ngram = 2, removeExtraWords = tm::stopwords("en")) # nube de palabras unigrama Q2
