#Logistic Regression 
# In Logistic regression 
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heartdata <-read.csv(url,header = FALSE)
head(heartdata) # head shows the first 6 rows of the data. 

colnames(heartdata) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca", "thal","hd")
head(heartdata)

str(heartdata) # use the str() function to see the structure. use help(str) to learn more about the str() fuction

heartdata[heartdata =="?"] <-NA # change the question (?) makrs to NA in our dataset
#heartdata[data =="?"] <- NA

str(heartdata)
# here we change the "0" to "F" to represent the female.
heartdata[heartdata$sex == 0,]$sex <- "F"  
# Now you need to convert the "1" to "M" to represent the male.  
# (here we change the "1" to "M" to represent the male)
heartdata[heartdata$sex == 1,]$sex <- "M"

# next we need to convert the columns into factors.
heartdata$sex <- as.factor(heartdata$sex)
# repeat this convertion of column into factors for the "cp", "fbs", "restecg", "exang", "slope".
heartdata$cp <- as.factor(heartdata$cp)
heartdata$fbs <- as.factor(heartdata$fbs)
heartdata$restecg <-as.factor(heartdata$restecg)
heartdata$exang <- as.factor(heartdata$exang)
heartdata$slope <- as.factor(heartdata$slope)

# Since the "ca" colum had the "?" in it, instead of "NA", R-Language thinks it is a column 
# of strings, We correct that assumption by telling R-Language that it is a column of integers
heartdata$ca <- as.integer(heartdata$ca)
# then convert the "ca" column into factor using the as.factor function
heartdata$ca <- as.factor(heartdata$ca)
# reapet the same process for the "thal" colum, 
heartdata$thal <-as.integer(heartdata$thal)
heartdata$thal <-as.factor(heartdata$thal)

# We need to make the "hd"(heart disease), a factor that is easy to read for humans, 
# using the ifelse() to convert the "0"s to "Healthy" and "1"s to "Unhealthy"
heartdata$hd <-ifelse(test = heartdata$hd == 0, yes = "Healthy", no = "Unhealthy")
heartdata$hd <- as.factor(heartdata$hd)
# Now check the data with the str() function. and observe the data to 
#check the appropriate changes were made.
str(heartdata)
# now check how many samples of rows of data have the "NA" values. if there are "NA"s available 
# later we can decided if we should impute the values for the "NA"s, we can check that by using
# is.na() function.
nrow(heartdata[is.na(heartdata$ca) | is.na(heartdata$thal),])
# there are 6 rows that have "NA"s.

heartdata[is.na(heartdata$ca) | is.na(heartdata$thal),]
# now we can view the samples with NAs by selecting those rows from the data.frame
nrow(heartdata) # check number of rows
# if there are "NA"s available we can remove those rows
heartdata <-heartdata[!(is.na(heartdata$ca) | is.na(heartdata$thal)),]
nrow(heartdata)
# now we need to make sure that healthy and diseased samples come from both genders. If only the male sample have
# the heart disease, we should remove all females from the model. we can do that using xtab() function, we pass 
# the heartdata to the function and use the model syntax to select the columns, we select the "hd" and "sex" columns,
# in the heartdata that we want to build a table from. Which means we want heart disease and sex.
xtabs(~heartdata$hd +heartdata$sex, data = heartdata)

# let's check for the "chest-pain" denoted as "cp" by all the patients
xtabs(~ heartdata$hd + heartdata$cp, data=heartdata)

# Now repeat the process for the others
xtabs(~ heartdata$hd + heartdata$fbs, data=heartdata)
xtabs(~ heartdata$hd + heartdata$restecg, data=heartdata)
# using glm(), the Generalized Lenear Models.
logis <- glm(heartdata$hd ~ heartdata$sex, data = heartdata, family = "binomial")

summary(logis)

install.packages("caret")
install.packages("caret", dependencies=c("Depends", "Suggests"))
library(caret)
library(lattice)
library(ggplot2)

# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris
head(dataset)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]
# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)
# take a peek at the first 6 rows of the data
head(dataset)
# list the levels for the class
levels(dataset$Species)
# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

# summarize attribute distributions
summary(dataset)
# plots
# split input and output
x <- dataset[,1:4]
y <- dataset[,5]
# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# barplot for class breakdown
plot(y)
# Multivariate Plots
# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")
# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")
# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#Build Models
# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
# compare accuracy of models
dotplot(results)
# summarize Best Model
print(fit.lda)
# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
# Read: What is Kappa: https://www.r-bloggers.com/k-is-for-cohens-kappa/

library(MASS)
library(nlme)
data(oats)
names(oats) = c('block', 'variety', 'nitrogen', 'yield')
oats$mainplot = oats$variety
oats$subplot = oats$nitrogen

summary(oats)

library(nlme)
m1.nlme = lme(yield ~ variety*nitrogen,
              random = ~ 1|block/mainplot,
              data = oats)

summary(m1.nlme)
anova(m1.nlme)


