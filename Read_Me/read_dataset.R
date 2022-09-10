############################################## Part 0: The Price Index #####################################################

#Load necessary packages
library(tidyverse)
library(corrplot)
library(ggplot2)

#Set working directory
directory<- "~/Desktop/Price Index/house-prices-advanced-regression-techniques" 
setwd(directory)


#---------------------------------------------------------- The Data Set ---------------------------------------------------------------#

#Load data set
train <- read.csv("train.csv")
test <- read.csv("test.csv")
sale <- read.csv("sample_submission.csv")


#Keep only similar variables to true data set
#thereof the following:
#Sale Price
#Utilities
#Condition1
#Bldgtype
#OverallQual
#OverallCond
#YearBuilt
#YearRemodAdd
#Heating
#CentralAir
#Bedroom
#Kitchen
#TotRmsAbvGrd
#PoolArea
#MiscFeature
#LotArea
#1stFlrSF
#2ndFlrSF

#generate vectors
characteristics<- c("Id", "SalePrice", "Utilities", "Condition1", "Bldgtype", "OverallQual"," OverallCond"," YearBuilt", "YearRemodAdd", "Heating",
                    "CentralAir", "Bedroom", "Kitchen", "TotRmsAbvGrd"," PoolArea", "MiscFeature", "LotArea", "1stFlrSF", "2ndFlrSF" )

#keep only sub-sample of training data set
sale_dataset<- train[, names(train) %in% characteristics]
sale_test<- test[, names(test) %in% characteristics]

#---------------------------------------------------------- EDA ---------------------------------------------------------------#

#Check shape of data set (columns and rows)
dim(sale_dataset)

#Check structure of data set
str(sale_dataset)

#Check distribution of data set variables
summary(sale_dataset)

#Check duplicates in data set
sum(duplicated(sale_dataset))

#Check correlations between the numerical house characteristics and the sale prices
corr<-cor(sale_dataset[, unlist(lapply(sale_dataset, is.numeric))]) 
corrplot(corr)

#Check number of missings for each variable and percentage
missings <-as.data.frame(colSums(is.na(sale_dataset)))
percentage<- as.data.frame(colSums(is.na(sale_dataset)) / nrow(sale_dataset))
missings<-cbind(missings, percentage)
#Remark: if a variable has a high number of missings following possibilities:
#Drop column with missings
#Fill with No, None (or sth. else that makes sense)
#Imputation

#Outlier detection
#Remark: necessary for numerical variables that show a high correlation (pos. or neg.) with sale price
#Scatterplot: OverallQual and Sale Price
ggplot(sale_dataset, aes(OverallQual, SalePrice))+geom_point()
#Remark: drop outliers?

#Relationship between categorical variables and Sale Price
#Remark: economic intuition necessary (e.g more rooms, higher Sale Price)
#Barplot: Condition1 and Sale Price
ggplot(sale_dataset, aes(x = Condition1, y = SalePrice)) + geom_bar(stat = "identity")

#Check distribution of variable of interest
ggplot(sale_dataset) + geom_histogram(aes(SalePrice))

#Log transform variable of interest and check distribution
ggplot(sale_dataset) + geom_histogram(aes(log(SalePrice))) #achieve normal distribution


#Encoding categorical variables
#Remark: does it make sense to to binary encoding (e.g housetype into variables with 1 or 0)
#Example: Encode Condition1 in binary variables
encoding<- as.data.frame(model.matrix(~Condition1-1, sale_dataset))
sale_dataset<-cbind(sale_dataset,encoding)

#-------------------------------------------------------------------------------------------------------------------------#

#for this example: merge test and Sale Price per ID
sale_test <- sale_test %>% left_join(sale, by="Id")

#-------------------------------------------------------------------------------------------------------------------------#

#save data set in working directory

write.csv(sale_dataset, file="~/Desktop/Price Index/house-prices-advanced-regression-techniques/sale_dataset.csv", row.names=FALSE)
write.csv(sale_test, file="~/Desktop/Price Index/house-prices-advanced-regression-techniques/sale_test.csv", row.names=FALSE)






















