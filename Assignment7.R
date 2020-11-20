library(ISLR)
library(MASS)
library(boot)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(dplyr)

#read the white wine file
setwd("/Users/yiqingchen/Desktop/RPI/DataAnalytics/Lab/DataAnalytics2020-Yiqing-Chen/datasource")
getwd()
whitewine <- read.csv('winequality-white.csv',header = TRUE, sep=";")
summary(whitewine)
attach(whitewine)
View(whitewine)
str(whitewine)
head(whitewine)

#visualizations
boxplot(quality)
ggplot(whitewine,aes(x=quality))+geom_histogram(binwidth=1)
ggplot(whitewine,aes(x=fixed.acidity))+geom_histogram(binwidth=1)
ggplot(whitewine,aes(x=total.sulfur.dioxide))+geom_histogram(binwidth=1)
ggplot(whitewine,aes(x=alcohol))+geom_histogram(binwidth=1)
ggplot(whitewine,aes(x=pH))+geom_histogram(binwidth=1)
qqnorm(fixed.acidity,main = "fixed acidity");qqline(fixed.acidity)
qqnorm(volatile.acidity,main = "volatile acidity");qqline(volatile.acidity)
qqnorm(total.sulfur.dioxide,main = "total sulfur dioxide");qqline(total.sulfur.dioxide)
qqnorm(alcohol,main = "alcohol");qqline(alcohol)
qqnorm(pH,main='pH');qqline(pH)
plot(ecdf(quality),do.points=FALSE,verticals=TRUE)
plot(ecdf(alcohol),do.points=FALSE,verticals=TRUE)
plot(ecdf(fixed.acidity),do.points=FALSE,verticals=TRUE)

#filter data
whitewine1 <- data.frame(whitewine)
whitewine1 <- whitewine[which(quality >= 4 & quality <= 7),]
boxplot(whitewine1$quality)
detach(whitewine)
attach(whitewine1)

#random forest classification
set.seed(100)
train <- sample(nrow(whitewine1),0.7*nrow(whitewine1),replace = TRUE)
TrainSet_ww <- whitewine1[train,]
ValidationSet_ww <- whitewine1[-train,]
model_ww1 <- randomForest(as.factor(quality)~.,data = TrainSet_ww,importance = TRUE)
model_ww1
predTrain_ww1 <- predict(model_ww1, TrainSet_ww, type = 'class')
table(predTrain_ww1,TrainSet_ww$quality)
predValidation_ww1 <- predict(model_ww1, ValidationSet_ww, type = 'class')
table_ww1 <- table(predValidation_ww1,ValidationSet_ww$quality)
table_ww1
model_ww1_accuracyrate = sum(diag(table_ww1))/sum(table_ww1)
model_ww1_accuracyrate
importance(model_ww1)
varImpPlot(model_ww1)
model_ww1_1 <- randomForest(as.factor(quality)~+free.sulfur.dioxide+alcohol+volatile.acidity+density,data = TrainSet_ww,importance = TRUE)
model_ww1_1
predTrain_ww1_1 <- predict(model_ww1_1, TrainSet_ww, type = 'class')
table(predTrain_ww1_1,TrainSet_ww$quality)
predValidation_ww1_1 <- predict(model_ww1_1, ValidationSet_ww, type = 'class')
table_ww1_1 <- table(predValidation_ww1,ValidationSet_ww$quality)
table_ww1_1
model_ww1_accuracyrate_1 = sum(diag(table_ww1_1))/sum(table_ww1_1)
model_ww1_accuracyrate_1

#linear regression
model_ww2 <-lm(quality~.,data=TrainSet_ww)
summary(model_ww2)
plot(model_ww2)
plot(resid(model_ww2))
predww2 <- predict(model_ww2,ValidationSet_ww)
#round(predww2)
plot(round(predww2))
table_ww2 <- table(round(predww2),ValidationSet_ww$quality)
model_ww2_accuracyrate = sum(diag(table_ww2))/sum(table_ww2)
model_ww2_accuracyrate

#rpart CART 
model_ww3 <- rpart(quality~.,data = TrainSet_ww, method = 'class')
rpart.plot(model_ww3)
pred_ww3 <- predict(model_ww3,ValidationSet_ww,type = 'class')
table_ww3 <- table(ValidationSet_ww$quality,pred_ww3)
model_ww3_accuracyrate = sum(diag(table_ww3))/sum(table_ww3)
model_ww3_accuracyrate

#bank marketing read file
bank <- read.csv("bank-full.csv",header = TRUE, sep=";")
summary(bank)
View(bank)
str(bank)
head(bank)
attach(bank)

#Visualizations & Stat
as.factor(bank$y)
y2=ifelse(bank$y=="yes",1,0)
bank2 <- cbind(bank, y2 = y2)
ggplot(bank2,aes(x=y2))+geom_histogram(binwidth=0.1)
ggplot(bank,aes(x=age))+geom_histogram(binwidth=1)
ggplot(bank,aes(x=pdays))+geom_histogram(binwidth=20)
ggplot(bank,aes(x=duration))+geom_histogram(binwidth=100)
unique(marital)
unique(job)
unique(education)
unique(default)
unique(housing)
unique(loan)
unique(contact)
unique(poutcome)

#filter data
bank1 <- data.frame(bank)
bank1 <- bank[which(education!= "unknown" & job != "unknown" & poutcome != "unknown" & contact!= "unknown"),]
View(bank1)
detach(bank)
attach(bank1)

#random forest classification
set.seed(100)
train2 <- sample(nrow(bank1),0.7*nrow(bank1),replace = TRUE)
TrainSet_ba <- bank1[train2,]
ValidationSet_ba <- bank1[-train2,]
model_ba1 <- randomForest(as.factor(y)~.,data = TrainSet_ba,importance = TRUE)
model_ba1
predTrain_ba1 <- predict(model_ba1, TrainSet_ba, type = 'class')
table(predTrain_ba1,TrainSet_ba$y)
predValidation_ba1 <- predict(model_ba1, ValidationSet_ba, type = 'class')
table_ba1 <- table(predValidation_ba1,ValidationSet_ba$y)
table_ba1
model_ba1_accuracyrate = sum(diag(table_ba1))/sum(table_ba1)
model_ba1_accuracyrate
importance(model_ba1)
varImpPlot(model_ba1)

#CRAN RPART
model_ba2 <- rpart(y~.,data = TrainSet_ba, method = 'class')
rpart.plot(model_ba2)
pred_ba2 <- predict(model_ba2,ValidationSet_ba,type = 'class')
table_ba2 <- table(ValidationSet_ba$y,pred_ba2)
table_ba2
model_ba2_accuracyrate = sum(diag(table_ba2))/sum(table_ba2)
model_ba2_accuracyrate

#KNN
library(class)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
bank1_norm <- as.data.frame(lapply(bank1[,c(1,6,10,12,13,14,15)], normalize))
ran <- sample(1:nrow(bank1), 0.7 * nrow(bank1)) 
bank1_train <- bank1_norm[ran,] 
bank1_test <- bank1_norm[-ran,]
bank1_y_category <- bank1[ran,17]
bank1_test_category<- bank1[-ran,17]
model_ba3 <- knn(train = bank1_train, test = bank1_test, cl = bank1_y_category,k=60)
table_ba3 <- table(model_ba3,bank1_test_category)
table_ba3
model_ba3_accuracyrate = sum(diag(table_ba3))/sum(table_ba3)
model_ba3_accuracyrate

