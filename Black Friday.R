# Installing Packages

install.packages('data table')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('corrplot')
install.packages('xgboost')
install.packages('cowplot')
install.packages('Hmisc')

# Loading Data

library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)
library(xgboost)
library(cowplot)
library(Hmisc)

# Reading Datasets

getwd()  # for getting the working directory
setwd("E:/extra/R programming/project")

BFTrain <- fread('train.csv')
BFTest = fread('test.csv')
submission = fread('submission.csv')

# Checking the Datasets
names(BFTrain)
names(BFTest)
names(submission)

str(BFTrain)
str(BFTest)

sapply(BFTrain ,class)
sapply(BFTest ,class)

# Taking unique values of User_ID
#X_train <- distinct(select(train,User_ID))
#X_test <- distinct(select(test,User_ID))

# Univariate EDA

ggplot(BFTrain) + geom_histogram(aes(BFTrain$Purchase), binwidth = 100 ,fill = "pink")

p1 = ggplot(BFTrain)+ geom_histogram(aes(Product_Category_1), binwidth=0.5,fill="red")
p2 = ggplot(BFTrain)+ geom_histogram(aes(Product_Category_2), binwidth=0.5,fill="red")
p3 = ggplot(BFTrain)+ geom_histogram(aes(Product_Category_3), binwidth=0.5,fill="red")
plot_grid(p1,p2,p3,nrow = 1)

# Bivariate Analysis

ggplot(BFTrain)+geom_point(aes(Product_Category_1 ,Purchase),color="violet",alpha=0.3)+
  theme(axis.title = element_text(size = 10))

ggplot(BFTrain)+geom_point(aes(Product_Category_2 ,Purchase),color="violet",alpha =0.3)

ggplot(BFTrain)+geom_point(aes(Product_Category_3 ,Purchase),color="violet",alpha=0.3)  

# Imputing Missing Values
BFTrain[is.na(BFTrain)] <- 0
BFTest[is.na(BFTest)] <- 0
head(BFTrain$Product_Category_2)
head(BFTest$Product_Category_3)
head(BFTrain$Product_Category_2)
head(BFTest$Product_Category_3)

ggplot(BFTrain)+ geom_histogram(aes(Product_Category_2),binwidth=1,color="orange")

ggplot(BFTrain)+ geom_histogram(aes(Product_Category_3),binwidth=1,color="orange")

# Label Encoding and One-hot encoding
#names(X_train)
#X_train[,Gender_num := ifelse(Gender == "F",0,ifelse(Gender == "M",1,2))]
#X_train[,c('Gender'):= NULL]
#X_test[,Gender_num := ifelse(Gender == "F",0,ifelse(Gender == "M",1,2))]
#X_test[,c('Gender'):= NULL]

#X_train <- dummy.data.frame(X_train, names=c("City_Category"), sep="_")
#X_test <- dummy.data.frame(X_test, names=c("City_Category"), sep="_")


#Unique Data For Blackfriday & Data type modification
BFTrain$User_ID <- as.factor(BFTrain$User_ID)
BFTrain$Product_ID <- as.factor(BFTrain$Product_ID)
BFTrain$Marital_Status <- as.factor(ifelse(BFTrain$Marital_Status == 1, 'Married', 'Single'))
BFTrain$Age <- as.factor(BFTrain$Age)
BFTrain$Gender <- as.factor(ifelse(BFTrain$Gender=='M', 'Male', 'Female'))
BFTrain$Occupation <- as.factor(BFTrain$Occupation)
BFTrain$City_Category <- as.factor(BFTrain$City_Category)
BFTrain$Stay_In_Current_City_Years <- as.factor(BFTrain$Stay_In_Current_City_Years)

BFTest$User_ID <- as.factor(BFTest$User_ID)
BFTest$Product_ID <- as.factor(BFTest$Product_ID)
BFTest$Marital_Status <- as.factor(ifelse(BFTest$Marital_Status == 1, 'Married', 'Single'))
BFTest$Age <- as.factor(BFTest$Age)
BFTest$Gender <- as.factor(ifelse(BFTest$Gender=='M', 'Male', 'Female'))
BFTest$Occupation <- as.factor(BFTest$Occupation)
BFTest$City_Category <- as.factor(BFTest$City_Category)
BFTest$Stay_In_Current_City_Years <- as.factor(BFTest$Stay_In_Current_City_Years)

BF_dist <- distinct(BFTrain, User_ID, Age, Gender, Marital_Status, Occupation,
                    City_Category,Stay_In_Current_City_Years,Product_Category_1,
                    Product_Category_2,Product_Category_3)
head(BF_dist)


# feature representing the count of each user
user_count <- ddply(BFTrain, .(User_ID), nrow)
names(user_count)[2] <- "User_Count"
BFTrain <- merge(BFTrain, user_count, by="User_ID")
BFTest <- merge(BFTest, user_count, all.x=T, by="User_ID")

# feature representing the count of each product
product_count <- ddply(BFTrain, .(Product_ID), nrow)
names(product_count)[2] <- "Product_Count"
BFTrain <- merge(BFTrain, product_count, by="Product_ID")
BFTest <- merge(BFTest, product_count, all.x=T, by="Product_ID")
BFTest$Product_Count[is.na(BFTest$Product_Count)] <- 0

# feature representing the average Purchase of each product
product_mean <- ddply(BFTrain, .(Product_ID), summarize, Product_Mean=mean(Purchase))
BFTrain <- merge(BFTrain, product_mean, by="Product_ID")
BFTest <- merge(BFTest, product_mean, all.x=T, by="Product_ID")
BFTest$Product_Mean[is.na(test$Product_Mean)] <- mean(BFTrain$Purchase)

# subsetting columns for submission
submit <- BFTest[,c("User_ID","Product_ID")]

# target variable
y <- BFTrain$Purchase

# removing irrelevant columns
BFTrain <- subset(BFTrain, select=-c(Purchase,Product_ID))
BFTest <- subset(BFTest, select=c(colnames(BFTrain)))
str(BFTrain)
## xgboost with cross validation
model_xgb_1 <- xgboost(data=as.matrix(BFTrain),label=as.matrix(y),cv=5,objective="reg:linear",nrounds=500,max.depth=10,eta=0.1,colsample_bytree=0.5,seed=235,metric="rmse",importance=1)

pred <- predict(model_xgb_1, as.matrix(BFTest))
head(pred)
submission <- data.frame(submission, "Purchase" = pred)
write.csv(submission, "submission.csv", row.names=F)
