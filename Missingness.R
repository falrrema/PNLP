# Remove columns with over than 80% missing data, the following function receives a dataset
# calculates the percent of missingness for column and eliminates those with the specify missingness (e.g 80%)
# returns the cleaned data set
library(plotly)

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

#Barplot of % missingness for each column of the data set

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