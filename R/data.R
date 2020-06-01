library(tidyverse) 
library(corrplot)
#import the dataset
calhousing <- read.csv("C:/Users/Anna Errichiello/Documents/GitHub/progetto Machine Learning/cal-housing.csv", sep = ",")
head(calhousing)
dir.create("data")
save(calhousing, file=file.path("data","calhousing.rda"))

#convert string into numeric values
calhousing$ocean_proximity <- as.numeric(c("INLAND"=0,"NEAR BAY"= 1, "<1 OCEAN" = 0))
calhousing$ocean_proximity


# plug in your data here, I delete non numeric variable
calhousing <- data.frame(na.omit(calhousing)) 
x <- calhousing %>% select(longitude, latitude, housing_median_age,
                           total_rooms,households, median_income, 
                           median_house_value, ocean_proximity) %>% data.matrix()

y <- matrix(c(calhousing$median_house_value))

trainingIndex <- sample(1:nrow(calhousing), 0.90*nrow(calhousing)) # indices for 90% 
trainingData <- calhousing[trainingIndex, ] # training data

x_train <- trainingData %>% select(longitude, latitude, housing_median_age,
                                   total_rooms,households, 
                                   median_income, median_house_value, ocean_proximity) %>% data.matrix()

y_train <- matrix(c(trainingData$median_house_value))


testData <- calhousing[-trainingIndex, ] # test data
x_test <- testData %>% select(longitude, latitude, housing_median_age,
                              total_rooms,households, median_income, median_house_value, ocean_proximity) %>% data.matrix()

y_test <- matrix(c(testData$median_house_value))

#correlation test
round(cor(x), 2) 
#correlation plots
corrplot(round(cor(x), 2)) #correlation graph
symnum(cor(x))