#################### Linear Regresssion - Predicting Miles Per Gallon ############


# Problem Statement
# The input data set contains data about details of various car models. Based on the information provided, the
# goal is to come up with a model to predict Miles-per-gallon of a given model.



##################### Data Engineering & Analysis ##############################

setwd("E:/Mission Machine Learning/Git/predictingFuelEconomy")

auto_data <- read.csv("Data/auto-miles-per-gallon.csv")    

str(auto_data)

summary(auto_data)

head(auto_data)


# Data Cleansing
# 1. The ranges of values in each of the variables (columns) look ok without any kind of outliers
# 2. Horsepower is a number and R should have shown the quartiles like other numeric variables. It is being
# recognized as factor. Also, str() shows “?” as one of the values. So this means, the ? values should be
# imputed. We will replace the “?” with the mean value for Horsepower.

auto_data$HORSEPOWER <- as.numeric(auto_data$HORSEPOWER)

#as.numeric will have converted ? to NA
auto_data$HORSEPOWER[is.na(auto_data$HORSEPOWER)] <- mean(auto_data$HORSEPOWER, na.rm=TRUE)

summary(auto_data)

library(ggplot2)

ggplot(auto_data, aes(factor(CYLINDERS), MPG)) +
  geom_boxplot( aes(fill=factor(CYLINDERS)))

library(psych)

pairs.panels(auto_data)

############################## Modeling & Prediction ###################################

# ignore colume 8 - NAMES which is string. Regression works only on numbers.
lm_model <- lm( MPG ~ ., auto_data[,-8] )
summary(lm_model)


#################################### Testing ##################################################
# To test the accuracy of the equation, let us apply the equation on the same data set and predict the MPG
# for each record. Since we already know the actual value, let us compare the predicted value with the actual
# value and see the conformance/ error in the prediction.

predicted <- predict.lm(lm_model, auto_data)

summary(predicted)

#plot prediction vs actuals
plot( auto_data$MPG, predicted, col="red")

#find correlation between prediction and actuals
cor(auto_data$MPG, predicted)