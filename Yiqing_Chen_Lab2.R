getwd()
setwd("Desktop/RPI/DataAnalytics/Lab/DataAnalytics2020-Yiqing-Chen")

#Import Database
EPI2010 <- read.csv('EPI2010_data.csv',skip = 1)

#Measure of Central Tendency
EPI <- EPI2010$EPI
tf <- is.na(EPI)
EPI <- EPI[!tf]
summary(EPI)
prod(EPI)^(1/length(EPI)) #geometric mean
1 / mean(1/EPI) #harmonic mean
DALY <- EPI2010$DALY
tf <- is.na(DALY)
DALY <- DALY[!tf]
summary(DALY)

#Generate Histogram
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
hist(DALY,seq(0.,100.,1.0),prob=TRUE) 

#Generate Boxplot and qqplot
ENVHEALTH <- EPI2010$ENVHEALTH
ECOSYSTEM <- EPI2010$ECOSYSTEM
boxplot(ENVHEALTH,ECOSYSTEM)
qqplot(ENVHEALTH,ECOSYSTEM)

#Regression Exercise
EPI_data <- read.csv('EPI_data.csv')
attach(EPI_data)
ENVHEALTH <- EPI_data$ENVHEALTH
DALY <- EPI_data$DALY
AH <- EPI_data$AIR_H
WH <- EPI_data$WATER_H
boxplot(ENVHEALTH,DALY,AH,WH)
lmENVH <-lm(ENVHEALTH~DALY+AH+WH)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)
cENVH

#Predict New Values
DALYNEW <-c(seq(5,95,5))
AIR_HNEW <-c(seq(5,95,5))
WATER_HNEW <-c(seq(5,95,5))
NEW <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV <- predict(lmENVH,NEW,interval='prediction')
pENV
cENV <- predict(lmENVH,NEW,interval='confidence')
cENV

#Repeat AIR_E
AE <- EPI_data$AIR_E
boxplot(AE,DALY,AH,WH)
lmAIRE <- lm(AE~DALY+AH+WH)
lmAIRE
summary(lmAIRE)
cAIRE <- coef(lmAIRE)
cAIRE
pAIRE <- predict(lmAIRE,NEW,interval='prediction')
pAIRE
cAIRE2 <- predict(lmAIRE,NEW,interval='confidence')
cAIRE2

#Repeat CLIMATE
CL <- EPI_data$CLIMATE
boxplot(CL,DALY,AH,WH)
lmCL<-lm(CL~DALY+AH+WH)
lmCL
summary(lmCL)
cCL<-coef(lmCL)
cCL
pCL <- predict(lmCL,NEW,interval='prediction')
pCL
cCL2 <- predict(lmCL,NEW,interval='confidence')
cCL2

#Lab2Part2

#Exercise 1
#Multiple Regression
getwd()
setwd("Desktop/RPI/DataAnalytics/Lab/DataAnalytics2020-Yiqing-Chen")
MR <- read.csv('dataset_multipleRegression.csv')
UNEM <- MR$UNEM
HGRAD <- MR$HGRAD
ROLL <- MR$ROLL
lmR <- lm(ROLL~UNEM+HGRAD)
lmR
summary(lmR)
cR <- coef(lmR)
cR
new1 <- data.frame(UNEM=7,HGRAD=90000)
pR <- predict(lmR,new1)
pR

#Repeat Multiple Regression 
INC <- MR$INC
lmRo <- lm(ROLL~UNEM+HGRAD+INC)
lmRo
summary(lmRo)
cR1 <-coef(lmRo)
cR1
new2 <- data.frame(UNEM=7,HGRAD=90000,INC = 25000)
pR2 <- predict(lmRo,new2)
pR2

#Exercise 2
#Classification - KNN
aba <- read.csv('abalone.csv')
View(aba)
colnames(aba) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
summary(aba)
str(aba)
summary(aba$rings)
#As shown above, the “rings” variable has a range between 1-29. 
#This is the variable that we want to predict, and predicting this many levels 
#might not give us the insight we’re looking for.
#For now, we’ll break the rings variable 
#into 3 levels" “young” for abalones less than 8, “adult” for abalones between 8-11, 
#and “old” for abalones older than 11.
aba$rings <- as.numeric(aba$rings)
aba$rings <- cut(aba$rings,br=c(-1,8,11,35),labels = c('young','adult','old'))
aba$rings <- as.factor(aba$rings)
summary(aba$rings)
#remove the "sex" variable in abalone, because KNN requires all numeric variables for prediction
aba$sex <- NULL
#normalize the data using min max normalization
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
help(lapply)
aba[1:7]<-as.data.frame(lapply(aba[1:7],normalize))
summary(aba$shucked_wieght)
#After Normalization, each variable has a min of 0 and a max of 1.
#in other words, values are in the range from 0 to 1.
#We’ll now split the data into training and testing sets.
ind <- sample(2,nrow(aba),replace = TRUE, prob = c(0.7,0.3))
KNNTrain <- aba[ind==1,]
KNNTest <- aba[ind==2,]
sqrt(2918)
#make k equal to the square root of 2918, the number of observations in the training set.
#sqrt(2918) ~= 54.01852  round it to 55 and use k = 55 # We usually take an Odd number for k value, 
#knn model 
#knn() is in the "class" library. Make sure to install it first on your RStudio.
library(class)
help(knn)
KNNpred <- knn(train = KNNTrain[1:7], test = KNNTest[1:7], cl = KNNTrain$rings,k=55)
table(KNNpred)
KNNpred

#Exercise 3
#Clustering - KMeans
#iris dataset is from UCI ML repository.
library(ggplot2)
head(iris) #first 6 rows
str(iris) #structure
#dataset has 150 observations equally distributed observations among 
#the three species: Setosa, Versicolor and Verginica.
summary(iris)
help(sapply)
sapply(iris[,-5],var)
#plot Sepal.Length Vs Sepal.Width using ggplot 
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,col=Species))+geom_point()
#plot Petal.Length Vs Sepal.Width using ggplot 
ggplot(iris,aes(x=Petal.Length,y=Petal.Width,col=Species))+geom_point()
set.seed(300)
k.max <- 12
#tot.withinss = Total within-cluster sum of square 
#iter.max = the maximum number of iterations allowed
#nstart = if centers is a number, how many random sets should be chosen.
#center =either the number of clusters, say k, or a set of initial (distinct) cluster centres. If a number, a random set of (distinct) rows in x is chosen as the initial centres.
wss <- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart=20,iter.max=20)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss,type='b',xlab='Number of Clusters(k)',ylab="Within Cluster Sum of Squares")
icluster <- kmeans(iris[,3:4],3,nstart=20)
table(icluster$cluster,iris$Species)
#In the table we can see that most of the observations have been clustered correctly 
#however, 2 of the versicolor have been put in the cluster with all the virginica 
#and 4 of the verginica have been put in cluster 3 which mostly has versicolor.

#My own prediction
new.iris <- data.frame(iris[,-5])
summary(new.iris)
wss1 <- sapply(1:10,function(k){kmeans(iris[,1:4],k,nstart=20,iter.max=1000)$tot.withinss})
wss1 # within sum of squares.
plot(1:10,wss1,type='b',xlab='Number of Clusters(k)',ylab="Within Cluster Sum of Squares")
icluster <- kmeans(iris[,1:4],3,nstart=20)
table(icluster$cluster,iris$Species)

#Exercise 4
#Dplyr
# Dplyr for Data Manipulating
# Tidyr for Data Cleaning
# Pipe Operator    %>% 
install.packages('dplyr')
library(dplyr)
EPI1 <- data.frame(EPI_data$EPI)
sample_n(EPI1,5) #random 5 rows
DALY1 <- data.frame(EPI_data$DALY)
sample_n(DALY1,5) #random 5 rows
sample_frac(EPI1,0.1) # sample with a 10% of rows from the total number of rows
sample_frac(DALY1,0.1) # sample with a 10% of rows from the total number of rows
new_decs_EPI <- arrange(EPI1, desc(EPI1))
new_decs_EPI
new_decs_DALY <- arrange(DALY1,desc(DALY))
new_decs_DALY
double_EPI <- mutate(EPI1*2)
double_EPI
double_DALY <- mutate(DALY1*2)
double_DALY
summarise(EPI_data, mean_EPI = mean(EPI,na.rm = TRUE))
summarise(EPI_data, mean_DALY = mean(DALY,na.rm = TRUE))
