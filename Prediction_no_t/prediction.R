############################################## Part 1: The Price Index #####################################################

#Load necessary packages
library(tidyverse)
library(corrplot)
library(ggplot2)

#Set working directory
directory<- "~/Desktop/Price Index/house-prices-advanced-regression-techniques" 
setwd(directory)

#Load data set 
#Remark: the used data set shoul have been analysed descriptively and should contain all relevant variables
sale_dataset <- read.csv("sale_dataset.csv")
sale_test <- read.csv("sale_test.csv")

#Generate log Sale Price
sale_dataset$logsale<-log(sale_dataset$SalePrice)

#---------------------------------------------------------- Prediction: No Time Index t and Location Index i ---------------------------------------------------------------#

#Remark:
#Linear Regression with no time index t and no location index i

#Model 1: all independent variables

#generate model
model1<-lm(log(SalePrice)~Utilities + Condition1, data=sale_dataset)

#save predictions
sale_dataset$pred<-predict(model1,sale_dataset)

#residual plot
res<-resid(model1)
plot(fitted(model1), res) #can homoscedasticity be detected

#are the residuals normal distributed
qqnorm(res) 
hist(res)

#check the MSE: average squared difference between expected and observed value
#of train:
mse_train<-mean((data_$actual - data$pred)^2)

#of test:
mse_test<- 
  

#Model 2. independent variables of choice















































