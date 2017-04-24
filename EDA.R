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

df[, c("q1Clean", "q2Clean") := list(cleanText(question1, removeExtraWords = tm::stopwords("en"), preservePunct = "all", removeNum = F),
    cleanText(question2, removeExtraWords = tm::stopwords("en"), preservePunct = "all", removeNum = F))]
df$is_duplicate <- as.logical(as.numeric(df$is_duplicate))

# EDA
table_NA(df) # No se observa datos vacíos
summary(df)

# 1. Duplicados?

duplicated(df$id) %>% table
duplicated(df$qid1) %>% table
duplicated(df$qid2) %>% table

df %>% mutate(qpaste = paste(question1, question2)) %>% # pares de preguntas identicas
    mutate(isDu = duplicated(qpaste)) %>% slice(which(isDu == T))

# Es necesario eliminar el par id = 289199

df$is_duplicate2 <- "Distinto"
df[is_duplicate == 1]$is_duplicate2 <- "Duplicados"
sizePlot(df$is_duplicate2) # el 37% de los datos contienen duplicados

# 2. Modelo básico
df$predModBas <- 0
tablePred <- table(df$is_duplicate, df$predModBas, exclude = NULL)
colnames(tablePred)[2] <- "1"
tablePred <- tablePred[-3,]

caret::confusionMatrix(tablePred)
Metrics::logLoss(as.logical(df$predModBas), as.logical(as.numeric(df$is_duplicate)))

Metrics::logLoss(c(1,0,1), c(0.999999,0.0000001,0.9999999))

table(df$is_duplicate, df$predModBas, exclude = NULL)

LogLossBinary = function(actual, predicted, eps = 1e-15) {
    predicted = pmin(pmax(predicted, eps), 1-eps)
    - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}

LogLossBinary(as.numeric(df$is_duplicate), df$predModBas)

# 3. Diferencias Largo palabras (LP) o largo caracteres (LC)
# LP
df$wordCountQ1 <- stringr::str_count(df$question1, pattern = "\\S+")
df$wordCountQ2 <- stringr::str_count(df$question2, pattern = "\\S+")
df$diffLp <- abs(df$wordCountQ1-df$wordCountQ2)

summary(df$diffLp)
hist(df$diffLp, breaks = 50, col = "salmon")

# Analisis estadistico
boxplot(diffLp ~ is_duplicate, data = df, col = c("salmon", "dodgerblue3"))
shapiro.test(sample(df$diffLp, size = 5000))
qqnorm(df$diffLp)
var.test(diffLp ~ is_duplicate, data = df)
wilcox.test(diffLp ~ is_duplicate, data = df )

df %>% group_by(is_duplicate) %>% # Resumen estadistico
    summarise(meanWCdiff = mean(diffLp), sdWCdiff = sd(diffLp), medianWCdiff = median(diffLp))

# Prob de duplicados por tramos
df %>% mutate(diffLpSegment = cut(diffLp, breaks = c(0,7,10,20,50,100,300), right = F)) %>% 
    group_by(diffLpSegment) %>% summarise(prob_duplicate = sum(is_duplicate)/n()) %>% 
    plot_ly(x = ~diffLpSegment, y = ~prob_duplicate, type = "bar")

df %>% filter(is_duplicate == T) %>% count(diffLp) %>% mutate(perc = n/sum(n) * 100)
df %>% filter(is_duplicate == F) %>% count(diffLp) %>% mutate(perc = n/sum(n) * 100)

q4 <- quantile(df$diffLp)[4]
q4 + 1.5*(IQR(df$diffLp))

df %>% group_by(is_duplicate) %>% summarise(quantile(diffLp)[4]+1.5*IQR(diffLp))

# Eliminando stopwords
df$wcQ1clean <- stringr::str_count(df$q1Clean, pattern = "\\S+")
df$wcQ2clean <- stringr::str_count(df$q2Clean, pattern = "\\S+")
df$dlpClean <- abs(df$wcQ1clean-df$wcQ2clean)

boxplot(dlpClean ~ is_duplicate, data = df, ylim = c(0,10))
wilcox.test(dlpClean ~ is_duplicate, data = df)

df %>% mutate(dlpCleanSegment = cut(dlpClean, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20,50,100,300), right = F)) %>% 
    group_by(dlpCleanSegment) %>% summarise(prob_duplicate = sum(is_duplicate)/n()) %>% 
    plot_ly(x = ~dlpCleanSegment, y = ~prob_duplicate, type = "bar")

# LC 
df$wordCountCharQ1 <- stringr::str_count(df$question1, pattern = "")
df$wordCountCharQ2 <- stringr::str_count(df$question2, pattern = "")
df$diffLc <- abs(df$wordCountCharQ1-df$wordCountCharQ2)

summary(df$diffLc)
hist(df$diffLc, breaks = 50)

boxplot(diffLc ~ is_duplicate, data = df)
shapiro.test(sample(df$diffLc, size = 5000))
qqnorm(df$diffLc)
var.test(diffLc ~ is_duplicate, data = df)
wilcox.test(diffLc ~ is_duplicate, data = df, )

df %>% group_by(is_duplicate) %>% 
    summarise(meanWCdiff = mean(diffLc), sdWCdiff = sd(diffLc), medianWCdiff = median(diffLc))

df %>% mutate(diffLcSegment = cut(diffLc, breaks = c(0,51,100,300,1000,5000), right = F)) %>%
    group_by(diffLcSegment) %>% summarise(prob_duplicate = sum(is_duplicate)/n()) %>% 
    plot_ly(x = ~diffLcSegment, y = ~prob_duplicate, type = "bar")

df %>% mutate(diffLcSegment = cut(diffLc, breaks = c(0,1,2,3,4,5,6,7,8,9,10,20,50,100,300), right = F)) %>%
    filter(diffLcSegment == "[1,2)") %>% count(diffLc)

# Eliminando stopwords
df$wcCharQ1 <- stringr::str_count(df$q1Clean, pattern = "")
df$wcCharQ2 <- stringr::str_count(df$q2Clean, pattern = "")
df$dlcClean <- abs(df$wcCharQ1-df$wcCharQ2)

boxplot(dlcClean ~ is_duplicate, data = df)
wilcox.test(dlcClean ~ is_duplicate, data = df)

df %>% mutate(dlcCleanSegment = cut(dlcClean, breaks = c(0,51,100,300,1000,5000), right = F)) %>% 
    group_by(dlcCleanSegment) %>% summarise(prob_duplicate = sum(is_duplicate)/n()) %>% 
    plot_ly(x = ~dlcCleanSegment, y = ~prob_duplicate, type = "bar")


mod1 <- glm(is_duplicate ~ dlcClean, data = df)
summary(mod1)

pred <- predict(mod1)
LogLossBinary(df$is_duplicate, pred)
head(df$dlcClean)

df$modBasico <- sum(df$is_duplicate)/nrow(df)

LogLossBinary(df$is_duplicate, df$modBasico)
