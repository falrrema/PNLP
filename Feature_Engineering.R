########################
# Feature Engineering 
#######################
setwd("~/Kaggle/PNLP")
setwd("U:/Fabian/Proyectos/PNLP")

library(dplyr)
library(data.table)
library(plotly)
library(tidyr)

source("helper_pnlp.R")
# Sys.setlocale(locale="es_ES.UTF-8") # Para visualizar caracteres especiales

# Leyendo datos 
df <- singleRead("data/train_features.csv")
df$filename <- NULL
glimpse(df) # mirada rapida similar a str()

# Análisis estadísticos
wilcox.test(common_words ~ is_duplicate, data = df ) # common_words
wilcox.test(fuzz_qratio ~ is_duplicate, data = df ) # fuzz_qratio
wilcox.test(fuzz_WRatio ~ is_duplicate, data = df ) # fuzz_WRatio
wilcox.test(fuzz_partial_ratio ~ is_duplicate, data = df ) # fuzz_partial_ratio
wilcox.test(fuzz_partial_token_set_ratio ~ is_duplicate, data = df ) # fuzz_partial_token_set_ratio
wilcox.test(fuzz_partial_token_sort_ratio ~ is_duplicate, data = df ) # fuzz_partial_token_sort_ratio
wilcox.test(fuzz_token_set_ratio ~ is_duplicate, data = df ) # fuzz_token_set_ratio
wilcox.test(fuzz_token_sort_ratio ~ is_duplicate, data = df ) # fuzz_token_sort_ratio
boxplot(fuzz_qratio ~ is_duplicate, data = df, col = c("salmon", "dodgerblue3"))

judgeCor <- cor(df[,9:21])
judgeColor <- dmat.color(judgeCor)
judgeOrder <- order.single(judgeCor)
cpairs(df[,9:21], panel.colors = judgeColor, pch=".", gap=.5)

# Nuevos features
df <- mutate_each(df, funs(./100), fuzz_qratio:fuzz_token_sort_ratio) %>%
    mutate(combinePartialFuzz = fuzz_partial_ratio*fuzz_partial_token_set_ratio*fuzz_partial_token_sort_ratio,
        combineTokenFuzz = fuzz_token_set_ratio*fuzz_token_sort_ratio)

# Definición de Training, Validation y Testing Set
library(caTools)
set.seed(31)
split <- sample.split(df$is_duplicate, SplitRatio = 0.8)
train <- subset(df, split == TRUE)
test <-  subset(df, split == FALSE)

fwrite(train, file = "data/train_sample_features.csv")
fwrite(test, file = "data/test_sample_features.csv")
