#read the data file
getwd()
setwd("/Users/yiqingchen/Desktop/RPI/DataAnalytics/Lab/DataAnalytics2020-Yiqing-Chen")
getwd()
cancer<-read.csv('final.csv')
View(cancer)
str(cancer)
summary(cancer)

#add a new column of the cancer growth severity level based on the incidence and death rate change
#based on the five-number summary: min - median: mild; median - 3rd quarter: medium; 3rd quarter - max: severe
#<0 is getting better
library(dplyr)
cancer <- cancer %>%
  mutate(incidence_growth_severity = case_when(Incidence >= 0.4500 ~ 'Severe',
                                Incidence >= 0.3196 ~ 'Medium',
                                Incidence < 0 ~ 'GettingBetter',
                                TRUE ~ 'Mild'))
cancer <- cancer %>%
  mutate(death_growth_severity = case_when(Deaths >= 0.4585 ~ 'Severe',
                                               Deaths >= 0.3046 ~ 'Medium',
                                           Deaths < 0 ~ 'GettingBetter',
                                               TRUE ~ 'Mild'))

#similarly, I want to assign GDP Per capita growth groups and PM2.5 Growth Group to different countries and 
#see if the growth level can be predicted based on the incidence and death growth rate
cancer <- cancer %>%
  mutate(GDP_per_capita_growth = case_when(GDP_Per_Capita_Change >= 0.8452 ~ 'Rapid',
                                           GDP_Per_Capita_Change >= 0.3987 ~ 'Medium',
                                           GDP_Per_Capita_Change < 0 ~ 'Decline',
                                               TRUE ~ 'Mild'))

#the groups are a little different for PM2.5 since a negative change means getting better. 
cancer <- cancer %>%
  mutate(PM2_5_growth = case_when(PM2_5_Change >= 0.01691 ~ 'GettingWorse',
                                  PM2_5_Change >= -0.15010 ~ 'Medium',
                                           TRUE ~ 'GettingBetter'))

#Finally, i will add a column which is a ratio of incidence and death (incidence/death). 
#If the number is large, it means that although the number of people who got sick is large, 
#the medical level is better than the countries with low ratios to prevent death. 
cancer <- cancer %>% mutate(IToD_Ratio = Incidence/Deaths)

View(cancer)
attach(cancer)

#make them as factors
cancer$incidence_growth_severity <- as.factor(incidence_growth_severity)
cancer$death_growth_severity <- as.factor(death_growth_severity)
cancer$PM2_5_growth <- as.factor(PM2_5_growth)
cancer$GDP_per_capita_growth <- as.factor(GDP_per_capita_growth)

#Visualizations and EDA
library(ggplot2)
library(hrbrthemes)

#histogram graph among variables
hist(Incidence, breaks=10,
     main="Histogram of Incidence Change From 2006 to 2016")
hist(Deaths, breaks=10,
     main="Histogram of Death Change From 2006 to 2016")
hist(GDP_Per_Capita_Change,breaks=10,
     main="Histogram of GDP Per Capita Change From 2006 to 2016")
hist(CPI_Change,breaks=8,
     main="Histogram of CPI Change From 2006 to 2016")
hist(PM2_5_Change,breaks=10,
     main="Histogram of PM2.5 Change From 2006 to 2016")
hist(Population_Change,breaks=8,
     main="Histogram of Population Change From 2006 to 2016")

#value counts among the assigned groups
summary(cancer)
ggplot(cancer, aes(x=incidence_growth_severity)) + geom_bar() 
ggplot(cancer, aes(x=death_growth_severity)) + geom_bar() 
ggplot(cancer, aes(x=PM2_5_growth)) + geom_bar() 
ggplot(cancer, aes(x=GDP_per_capita_growth)) + geom_bar() 

#group counts based on gender
library(tidyr)
#Another way: c1 <- cancer %>% group_by(Sex) %>% count(incidence_growth_severity)
c1 <- with(cancer, table(incidence_growth_severity,Sex))
barplot(c1, beside = TRUE, legend = TRUE)
c2 <- with(cancer, table(death_growth_severity,Sex))
barplot(c2, beside = TRUE,legend=TRUE)

#correlations
library(Hmisc)
library('corrplot')
#create a seperate dataframe and assign numbers to categorical variables
corrr <- cancer[,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]
levels(corrr$incidence_growth_severity) <- 1:4
levels(corrr$death_growth_severity) <- 1:4
levels(corrr$GDP_per_capita_growth) <- 1:4
levels(corrr$PM2_5_growth) <- 1:3
corrr$incidence_growth_severity <- as.numeric(corrr$incidence_growth_severity)
corrr$death_growth_severity <- as.numeric(corrr$death_growth_severity)
corrr$GDP_per_capita_growth <- as.numeric(corrr$GDP_per_capita_growth)
corrr$PM2_5_growth <- as.numeric(corrr$PM2_5_growth)
correlation <- cor(as.matrix(corrr), method = "pearson", use = "complete.obs")
corrplot(correlation, method="number")
corrplot(correlation, method="color")



#model1 & 2 - Random Forest Classification
# if GDP Per Capita Change, CPI change, PM2.5 change and population change can determine the 
# incidence or death growth severity group
library(randomForest)
set.seed(100)
#using a 0.7/0.3 partition
t <- sample(nrow(cancer),0.7*nrow(cancer),replace = TRUE)
train <- cancer[t,]
test <- cancer[-t,]
model1 <- randomForest(incidence_growth_severity~GDP_Per_Capita_Change+CPI_Change+PM2_5_Change+Population_Change,
                       data = train,importance = TRUE)
model1
predtest1 <- predict(model1, test, type = 'class')
tbl_model1 <- table(predtest1,test$incidence_growth_severity)
tbl_model1
model1_accuracyrate = sum(diag(tbl_model1))/sum(tbl_model1)
model1_accuracyrate #62.30769
importance(model1)
varImpPlot(model1)

model2 <- randomForest(death_growth_severity~GDP_Per_Capita_Change+CPI_Change+PM2_5_Change+Population_Change,
                       data = train,importance = TRUE,ntree=450)
model2
predtest2 <- predict(model2, test, type = 'class')
tbl_model2 <- table(predtest2,test$death_growth_severity)
tbl_model2
model2_accuracyrate = sum(diag(tbl_model2))/sum(tbl_model2)
model2_accuracyrate #60.76923
importance(model2)
varImpPlot(model2)

#model3 & 4 - rpart classification tree
#using the same variable
library(rpart)
library(rpart.plot)
model3 <- rpart(incidence_growth_severity~GDP_Per_Capita_Change+CPI_Change+PM2_5_Change+Population_Change,
                       data = train, method = 'class')
rpart.plot(model3)
predtest3 <- predict(model3, test, type = 'class')
tbl_model3 <- table(predtest3,test$incidence_growth_severity)
tbl_model3
model3_accuracyrate = sum(diag(tbl_model3))/sum(tbl_model3)
model3_accuracyrate #56.15385

model4 <- rpart(death_growth_severity~GDP_Per_Capita_Change+CPI_Change+PM2_5_Change+Population_Change,
                data = train, method = 'class')
rpart.plot(model4)
predtest4 <- predict(model4, test, type = 'class')
tbl_model4 <- table(predtest4,test$incidence_growth_severity)
tbl_model4
model4_accuracyrate = sum(diag(tbl_model4))/sum(tbl_model4)
model4_accuracyrate #55.76023

#model5 & 6 - Random Forest Classification
#using the all numeric columns
model5 <- randomForest(incidence_growth_severity~GDP_Per_Capita_2006+GDP_Per_Capita_2016+GDP_Per_Capita_Change+
                         CPI_2006+CPI_2016+CPI_Change+PM2_5_2006+PM2_5_2016+PM2_5_Change+
                         Population_2006+Population_2016+Population_Change,
                       data = train,importance = TRUE)
model5
predtest5 <- predict(model5, test, type = 'class')
tbl_model5 <- table(predtest5,test$incidence_growth_severity)
tbl_model5
model5_accuracyrate = sum(diag(tbl_model5))/sum(tbl_model5)
model5_accuracyrate #61.15385
importance(model5)
varImpPlot(model5)

model6 <- randomForest(death_growth_severity~GDP_Per_Capita_2006+GDP_Per_Capita_2016+GDP_Per_Capita_Change+
                         CPI_2006+CPI_2016+CPI_Change+PM2_5_2006+PM2_5_2016+PM2_5_Change+
                         Population_2006+Population_2016+Population_Change,
                       data = train,importance = TRUE,ntree=400)
model6
predtest6 <- predict(model6, test, type = 'class')
tbl_model6 <- table(predtest6,test$death_growth_severity)
tbl_model6
model6_accuracyrate = sum(diag(tbl_model6))/sum(tbl_model6)
model6_accuracyrate #0.65
importance(model6)
varImpPlot(model6)

#model7 & 8 - Multiple Linear Regression
#I will first try to use the population change, CPI change, PM2.5 change and GDP Per Capita change to predict
#the incidence and death change respectively
model7 <-lm(Incidence~GDP_Per_Capita_Change+CPI_Change+PM2_5_Change+Population_Change,
               data = train)
summary(model7)
plot(model7)
abline(model7)
plot(resid(model7))
abline(0,0)
predtest7 <- predict(model7,test)
library(Metrics)
model7_MSE <- mse(predtest7,test$Incidence)
model7_MSE #0.08827189
model7_RMSE <- rmse(predtest7,test$Incidence)
model7_RMSE #0.2971059
model7_SSE <- sse(predtest7,test$Incidence)
model7_SSE #22.95069

model8 <-lm(Deaths~GDP_Per_Capita_Change+CPI_Change+PM2_5_Change+Population_Change,
            data = train)
summary(model8)
plot(model8)
abline(model8)
plot(resid(model8))
abline(0,0)
predtest8 <- predict(model8,test)
model8_MSE <- mse(predtest8,test$Deaths)
model8_MSE #0.8509825
model8_RMSE <- rmse(predtest8,test$Deaths)
model8_RMSE #0.291716
model8_SSE <- sse(predtest8,test$Deaths)
model8_SSE #22.125540

#model9 & 10 - Multiple Linear Regression
#includes all the numeric features as the independent variables to see performance difference
model9 <-lm(Incidence~GDP_Per_Capita_2006+GDP_Per_Capita_2016+GDP_Per_Capita_Change+
              CPI_2006+CPI_2016+CPI_Change+PM2_5_2006+PM2_5_2016+PM2_5_Change+
              Population_2006+Population_2016+Population_Change,
            data = train)
summary(model9)
plot(model9)
abline(model9)
plot(resid(model9))
abline(0,0)
predtest9 <- predict(model9,test)
model9_MSE <- mse(predtest9,test$Deaths)
model9_MSE #0.07958529
model9_RMSE <- rmse(predtest9,test$Incidence)
model9_RMSE #0.2803649
model9_SSE <- sse(predtest9,test$Incidence)
model9_SSE #20.43716

model10 <-lm(Deaths~GDP_Per_Capita_2006+GDP_Per_Capita_2016+GDP_Per_Capita_Change+
              CPI_2006+CPI_2016+CPI_Change+PM2_5_2006+PM2_5_2016+PM2_5_Change+
              Population_2006+Population_2016+Population_Change,
            data = train)
summary(model10)
plot(model10)
abline(model10)
plot(resid(model10))
abline(0,0)
predtest10 <- predict(model10,test)
model10_MSE <- mse(predtest10,test$Deaths)
model10_MSE #0.07598023
model10_RMSE <- rmse(predtest10,test$Deaths)
model10_RMSE #0.2756451
model10_SSE <- sse(predtest10,test$Deaths)
model10_SSE #19.75486

#model11 & 12 - KNN
#can we reverse and predict the GDP Per Capital growth stage and PM2.5 growth level with the incidence and death
#change rate and other features?
library(class)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
cancer_norm <- as.data.frame(lapply(cancer[,c(3,4,8,9,10,11,12,13,14,15,16,21)], normalize))
ran <- sample(1:nrow(cancer), 0.7 * nrow(cancer)) 
train_knn <- cancer_norm[ran,] 
test_knn <- cancer_norm[-ran,]
#for gdp per capita change level
target_model11 <- cancer[ran,19]
test_target_model11<- cancer[-ran,19]
model11 <- knn(train = train_knn, test = test_knn, cl = target_model11,k=40)
tbl_model11 <- table(model11,test_target_model11)
tbl_model11
model11_accuracyrate = sum(diag(tbl_model11))/sum(tbl_model11)
model11_accuracyrate #43.13725

#for PM2.5 change level
cancer_norm1 <- as.data.frame(lapply(cancer[,c(3,4,5,6,7,8,9,10,14,15,16,21)], normalize))
ran1 <- sample(1:nrow(cancer), 0.7 * nrow(cancer)) 
train_knn1 <- cancer_norm1[ran1,] 
test_knn1 <- cancer_norm1[-ran1,]
target_model12 <- cancer[ran1,20]
test_target_model12<- cancer[-ran1,20]
model12 <- knn(train = train_knn1, test = test_knn1, cl = target_model12,k=50)
tbl_model12 <- table(model12,test_target_model12)
tbl_model12
model12_accuracyrate = sum(diag(tbl_model12))/sum(tbl_model12)
model12_accuracyrate #0.627451

#model13 & 14 - KNN
#include only the change rate variables to compare performance
cancer_norm2 <- as.data.frame(lapply(cancer[,c(3,4,10,13,16,21)], normalize))
ran2 <- sample(1:nrow(cancer), 0.7 * nrow(cancer)) 
train_knn2 <- cancer_norm2[ran2,] 
test_knn2 <- cancer_norm2[-ran2,]
#for gdp per capita change level
target_model13 <- cancer[ran2,19]
test_target_model13<- cancer[-ran2,19]
model13 <- knn(train = train_knn2, test = test_knn2, cl = target_model13,k=80)
tbl_model13 <- table(model13,test_target_model13)
tbl_model13
model13_accuracyrate = sum(diag(tbl_model13))/sum(tbl_model13)
model13_accuracyrate #43.13725

#for PM2.5 change level
cancer_norm3 <- as.data.frame(lapply(cancer[,c(3,4,7,10,16,21)], normalize))
ran3 <- sample(1:nrow(cancer), 0.7 * nrow(cancer)) 
train_knn3 <- cancer_norm3[ran3,] 
test_knn3 <- cancer_norm3[-ran3,]
target_model14 <- cancer[ran3,20]
test_target_model14<- cancer[-ran3,20]
model14 <- knn(train = train_knn3, test = test_knn3, cl = target_model14,k=60)
tbl_model14 <- table(model14,test_target_model14)
tbl_model14
model14_accuracyrate = sum(diag(tbl_model14))/sum(tbl_model14)
model14_accuracyrate #56.86275

