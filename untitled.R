library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
calhousing <- read.csv("C:/Users/Anna Errichiello/Documents/GitHub/progetto Machine Learning/cal-housing.csv", sep = ",")
head(calhousing)
dir.create("data")
save(calhousing, file=file.path("data","calhousing.rda"))
calhousing <- data.frame (calhousing) # plug in your data here
colnames(calhousing)[1] <- "response"  # rename response var
X <- calhousing[, -1] # X variables
round(cor(X), 2) # Correlation Test
