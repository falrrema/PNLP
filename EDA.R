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
glimpse(df) # mirada rapida similar a str()
df$filename <- NULL

df <- df %>% mutate(q1Clean = cleanText(question1, removeExtraWords = tm::stopwords("en"), preservePunct = "all", removeNum = F),
    q2Clean = cleanText(question2, removeExtraWords = tm::stopwords("en"), preservePunct = "all", removeNum = F),
    is_duplicate = as.logical(as.numeric(df$is_duplicate))) # Limpie las columnas de textos y converti a FALSE o TRUE la respuesta

# EDA
summary(df) # No se observa datos vacíos

# Preguntas|Hipótesis

# 1. Duplicados? ----------------------------------------------------------
duplicated(df$id) %>% table # por id
duplicated(df$qid1) %>% table # por pregunta 1
duplicated(df$qid2) %>% table # por pregunta 2

df %>% mutate(qpaste = paste(question1, question2)) %>% # pares de preguntas identicas
    mutate(isDu = duplicated(qpaste)) %>% slice(which(isDu == T))

# Es necesario eliminar el par id = 289199


# 2. Modelo básico ------------------------------------------------------



# 2. Modelo básico
sizePlot(df$is_duplicate) # el 37% de los datos contienen duplicados
df$predModBas <- 36.92/100 # la probabilidad de duplicados en el dataset

LogLossBinary(as.numeric(df$is_duplicate), df$predModBas) # un LogLogg de 0.659 para modelo básico

# 3. Diferencias Largo palabras (LP) o largo caracteres (LC) ------------
# LP (largo de palabras)
df$wordCountQ1 <- stringr::str_count(df$question1, pattern = "\\S+") # conteo palabras 
df$wordCountQ2 <- stringr::str_count(df$question2, pattern = "\\S+")
df$diffLp <- abs(df$wordCountQ1 - df$wordCountQ2) # diferencia del conteo de palabras

summary(df$diffLp) 
hist(df$diffLp, breaks = 50, col = "salmon")

# Analisis estadistico
boxplot(diffLp ~ is_duplicate, data = df, col = c("salmon", "dodgerblue3"))
shapiro.test(sample(df$diffLp, size = 5000))
var.test(diffLp ~ is_duplicate, data = df)
wilcox.test(diffLp ~ is_duplicate, data = df )

df %>% group_by(is_duplicate) %>% # Resumen estadistico
    summarise(meanWCdiff = mean(diffLp), sdWCdiff = sd(diffLp), medianWCdiff = median(diffLp))

# Prob de duplicados por tramos de palabras 
df %>% mutate(diffLpSegment = cut(diffLp, breaks = c(0,7,10,20,50,100,300), right = F)) %>% 
    group_by(diffLpSegment) %>% summarise(prob_duplicate = sum(is_duplicate)/n()) %>% 
    plot_ly(x = ~diffLpSegment, y = ~prob_duplicate, type = "bar")

df %>% group_by(is_duplicate) %>% summarise(quantile(diffLp)[4]+1.5*IQR(diffLp)) 
# Muestra desde que punto los outliers son para cada tipo de respuesta

# Eliminando stopwords
df$wcQ1clean <- stringr::str_count(df$q1Clean, pattern = "\\S+")
df$wcQ2clean <- stringr::str_count(df$q2Clean, pattern = "\\S+")
df$dlpClean <- abs(df$wcQ1clean-df$wcQ2clean)

boxplot(dlpClean ~ is_duplicate, data = df, ylim = c(0,10))
wilcox.test(dlpClean ~ is_duplicate, data = df)

df %>% mutate(dlpCleanSegment = cut(dlpClean, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20,50,100,300), right = F)) %>% 
    group_by(dlpCleanSegment) %>% summarise(prob_duplicate = sum(is_duplicate)/n()) %>% 
    plot_ly(x = ~dlpCleanSegment, y = ~prob_duplicate, type = "bar") # una disminucion suave sin corte claro

# LC (Largo de caracteres)
df$wordCountCharQ1 <- nchar(df$question1) # conteo de caracteres
df$wordCountCharQ2 <- nchar(df$question2)
df$diffLc <- abs(df$wordCountCharQ1-df$wordCountCharQ2)

summary(df$diffLc)
hist(df$diffLc, breaks = 50)

# Estadísticas básicas
boxplot(diffLc ~ is_duplicate, data = df)
shapiro.test(sample(df$diffLc, size = 5000))
var.test(diffLc ~ is_duplicate, data = df)
wilcox.test(diffLc ~ is_duplicate, data = df, )

df %>% group_by(is_duplicate) %>% 
    summarise(meanWCdiff = mean(diffLc), sdWCdiff = sd(diffLc), medianWCdiff = median(diffLc))

df %>% mutate(diffLcSegment = cut(diffLc, breaks = c(0,51,100,300,1000,5000), right = F)) %>%
    group_by(diffLcSegment) %>% summarise(prob_duplicate = sum(is_duplicate)/n()) %>% 
    plot_ly(x = ~diffLcSegment, y = ~prob_duplicate, type = "bar") # la caída es más abrupta en esta variable

# Eliminando stopwords
df$wcCharQ1 <- nchar(df$q1Clean)
df$wcCharQ2 <- nchar(df$q2Clean)
df$dlcClean <- abs(df$wcCharQ1 - df$wcCharQ2)

boxplot(dlcClean ~ is_duplicate, data = df)
wilcox.test(dlcClean ~ is_duplicate, data = df)

df %>% mutate(dlcCleanSegment = cut(dlcClean, breaks = c(0,51,100,300,1000,5000), right = F)) %>% 
    group_by(dlcCleanSegment) %>% summarise(count = sum(is_duplicate), prob_duplicate = sum(is_duplicate)/n()) %>% 
    plot_ly(x = ~dlcCleanSegment, y = ~prob_duplicate, type = "bar") 

# es mejor quitarle las stopwords, el grupo con sobre 51 caracteres tiene lad mitad de probabilidad 
# que en el caso sin stopwords