
setwd("E:/Mission Machine Learning/Git/predictingFuelEconomy")

auto_data <- read.csv("Data/auto-miles-per-gallon.csv")

str(auto_data)

summary(auto_data)

head(auto_data)

auto_data$HORSEPOWER <- as.numeric(auto_data$HORSEPOWER)

#as.numeric will have converted ? to NA
auto_data$HORSEPOWER[is.na(auto_data$HORSEPOWER)] <- mean(auto_data$HORSEPOWER, na.rm=TRUE)

summary(auto_data)

library(ggplot2)

ggplot(auto_data, aes(factor(CYLINDERS), MPG)) +
  geom_boxplot( aes(fill=factor(CYLINDERS)))

library(psych)

pairs.panels(auto_data)

# ignore colume 8 - NAMES which is string. Regression works only on numbers.
lm_model <- lm( MPG ~ ., auto_data[,-8] )
summary(lm_model)

predicted <- predict.lm(lm_model, auto_data)
summary(predicted)

#plot prediction vs actuals
plot( auto_data$MPG, predicted, col="red")

#find correlation between prediction and actuals
cor(auto_data$MPG, predicted)