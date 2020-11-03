install.packages('gdata')
library(gdata) 
library(rpart)
library(ggplot2)
library(rpart.plot)
library(dplyr)
library('stringr')
library('readxl')
#queen1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1) 

queen1<-read_xls('/Users/yiqingchen/Desktop/RPI/DataAnalytics/Lab/DataAnalytics2020-Yiqing-Chen/rollingsales_queens2.xls')
#is.na(queen1)
#queen1 <- queen1[complete.cases(queen1),]
#is.na(queen1$TAX.CLASS.AT.TIME.OF.SALE)
View(queen1)
str(queen1)
summary(queen1)
names(queen1) <- str_replace_all(names(queen1),' ','.')
names(queen1)
names(queen1) <- str_replace_all(names(queen1),'\n','.')
names(queen1)
attach(queen1)

GROSS.SQUARE.FEET<-as.numeric(queen1$GROSS.SQUARE.FEET)
AND.SQUARE.FEET<-as.numeric(queen1$LAND.SQUARE.FEET) #gsub(",","",)
TOTAL.UNITS <-as.numeric(queen1$TOTAL.UNITS)
SALE.PRICE<-sub("\\$","",queen1$SALE.PRICE)
SALE.PRICE<-as.numeric(SALE.PRICE)

dim(queen1)
queen1 <- unique(queen1)
dim(queen1)

#SALE.PRICE<-sub("\\$","",queen1$SALE.PRICE) 
#GROSS.SQUARE.FEET<-as.numeric(gsub(",","",queen1$GROSS.SQUARE.FEET))
#LAND.SQUARE.FEET<-as.numeric(gsub(",","",queen1$LAND.SQUARE.FEET)) #gsub(",","",)
#TOTAL.UNITS <-as.numeric(gsub(",","",queen1$TOTAL.UNITS))

#SALE.PRICE<-as.numeric(gsub("[\\$,]","",SALE.PRICE))
sapply(queen1,is.numeric)

queen1<-queen1[which(queen1$GROSS.SQUARE.FEET!=0 & queen1$LAND.SQUARE.FEET!=0 & queen1$SALE.PRICE!=0),]
queen2 <- data.frame(queen1)
#queen1 <- queen2[which(SALE.PRICE >=100000),]
queen1 <- filter(queen2,SALE.PRICE>=10000)
#queen2 <- filter(queen2, GROSS.SQUARE.FEET <= 100000)
#queen1 <- filter(queen2, LAND.SQUARE.FEET <= 100000)

#SALE.PRICE <- queen1$SALE.PRICE
#queen1 <- queen1[SALE.PRICE > quantile(SALE.PRICE, .25) - 1.5*IQR(SALE.PRICE) & 
                   #SALE.PRICE < quantile(SALE.PRICE, .75) + 1.5*IQR(SALE.PRICE), ]

hist(LAND.SQUARE.FEET)
hist(log(LAND.SQUARE.FEET))
hist(GROSS.SQUARE.FEET)
hist(log(GROSS.SQUARE.FEET))
hist(TOTAL.UNITS)
hist(log(TOTAL.UNITS))
hist(SALE.PRICE)
hist(log(SALE.PRICE))
qqplot(GROSS.SQUARE.FEET,LAND.SQUARE.FEET)
qqplot(log(queen1$GROSS.SQUARE.FEET),log(queen1$LAND.SQUARE.FEET))
boxplot(queen1$LAND.SQUARE.FEET,varwidth = FALSE, range = 0, log = 'y', xlab = 'LAND SQUARE FEET')
boxplot(queen1$GROSS.SQUARE.FEET,varwidth = FALSE, range = 0, log = 'y', xlab = 'GROSS SQUARE FEET')
boxplot(queen1$SALE.PRICE,varwidth = FALSE, range = 0, log = 'y', xlab = 'SALE PRICE')
boxplot(log(queen1$GROSS.SQUARE.FEET))
boxplot(log(queen1$LAND.SQUARE.FEET))
boxplot(log(queen1$SALE.PRICE))
boxplot(log(queen1$TOTAL.UNITS))
ggplot(data = queen1,aes(x = GROSS.SQUARE.FEET, y = SALE.PRICE)) + geom_point()+labs(x = 'Gross Square Feet', y = 'Sale Price')
ggplot(data = queen1,aes(x = LAND.SQUARE.FEET, y = SALE.PRICE)) + geom_point()+labs(x = 'Land Square Feet', y = 'Sale Price')
ggplot(data = queen1,aes(x = TOTAL.UNITS, y = SALE.PRICE)) + geom_point()+labs(x = 'Total Units', y = 'Sale Price')


# perform models - MLR
queens <- sort(sample(nrow(queen1),nrow(queen1)*.6))
train <- queen1[queens,]
test <- queen1[-queens,]

mlr1 <-lm(train$SALE.PRICE~train$GROSS.SQUARE.FEET+train$LAND.SQUARE.FEET+train$TOTAL.UNITS)
summary(mlr1)
plot(mlr1)
plot(resid(mlr1))
abline(mlr1,col="red",lwd=2)


LS <- log(train$SALE.PRICE)
LG <- log(train$GROSS.SQUARE.FEET)
LL <- log(train$LAND.SQUARE.FEET)
LT <- log1p(train$TOTAL.UNITS)

mlr2 <-lm(LS~LG+LL+LT)
summary(mlr2)
plot(mlr2)
plot(resid(mlr2))
abline(mlr2,col="red",lwd=2)

# k-means 
set.seed(300)
k.max <- 12
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
train[14:16]<-as.data.frame(lapply(train[14:16],normalize))
test[14:16]<-as.data.frame(lapply(test[14:16],normalize))
wss <- sapply(1:k.max,function(k){kmeans(train[,14:16],k,nstart=20,iter.max=20)$tot.withinss})
wss
plot(1:k.max,wss,type='b',xlab='Number of Clusters(k)',ylab="Within Cluster Sum of Squares")
icluster <- kmeans(train[,14:16],5,nstart=20)
table(icluster$cluster,train$TAX.CLASS.AT.PRESENT)

# knn
library(class)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
queen2[14:16]<-as.data.frame(lapply(queen2[14:16],normalize))
ind <- sample(2,nrow(queen2),replace = TRUE, prob = c(0.6,0.4))
KNNTrain <- queen2[ind==1,]
KNNTest <- queen2[ind==2,]
sqrt(2918)
KNNpred <- knn(train = KNNTrain[14:16], test = KNNTest[14:16], cl = KNNTrain$NEIGHBORHOOD,k=60)
table(KNNpred)
KNNpred

#prediction
pmlr1 <- predict(mlr1,test,interval = 'prediction')
pmlr1
plot(pmlr1)
summary(pmlr1)
pmlr1 <- data.frame(pmlr1)
pmlr1 %>%
  ggplot(aes(pmlr1$upr, fit)) + geom_abline() + geom_point()

wss <- sapply(1:k.max,function(k){kmeans(test[,14:16],k,nstart=20,iter.max=20)$tot.withinss})
wss
plot(1:k.max,wss,type='b',xlab='Number of Clusters(k)',ylab="Within Cluster Sum of Squares")
icluster <- kmeans(test[,14:16],5,nstart=20)
table(icluster$cluster,test$TAX.CLASS.AT.PRESENT)

library(gmodels)
install.packages('gmodels')
CrossTable(x=test[14:16],y=KNNpred,prop.chisq = FALSE)
mean(KNNpred == test$NEIGHBORHOOD)

icluster$betweenss/icluster$totss*100

table(icluster$cluster,test$TAX.CLASS.AT.PRESENT)
#Total number of correctly classified instances are: 3188+3+1= 3192
#Total number of incorrectly classified instances are: 59+1+11+1+36+59+1+1+3+122+14+166+1 = 475
#Accuracy = 3192/(3192+475) = 0.88 i.e our model has achieved 88% accuracy!



