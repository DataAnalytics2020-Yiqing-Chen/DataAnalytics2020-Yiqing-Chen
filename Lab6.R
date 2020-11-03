# In Class Work

# Validation set example with Auto Dataset
library(ISLR)
library(MASS)
library(boot)
set.seed(1)
help("sample")
train = sample(392,196)
# We use the subset option in the lm() function to fit a linear regression using,
# only the observations corresponding to the training set.
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
# Now we use predict() function to estimate the response for all 392 observations,
# and we use the mean() function to calculate the MSE of the 196 observations in the 
# validation set. Note that the -train selects only the observations that are not in,
# the training set.
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# Therefore the estimated test MSE for the linear regression fit is 23.26601
# We can use the poly() function to estimate test error for the quadratic and cubic regression.
# Quadratic regression line
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
# Cubic regression line
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# The error rates are: 18.71646 for quadratics and 18.79401 for cubic
# If we choose different training set instead, then we will obtain somewhat different errors,
# on the validation set.
set.seed(2)
train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# the error rate is 25.72651
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
# the error rate is 20.43036
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# the error rate is 20.38533
# Using this split of the observations into a training set and validation set, 
# we find that the validation set error rates for the models with linear, quadratic,
# and cubic terms are 25.73, 20.43 and 20.39 respectively.
# The model that predict mpg using a quadratic function of horsepower performs better,
# than a models that only involves only a linear function of horsepower, and there is a,
# little evidence in favor of a model that uses a cubic function of horsepower.

# K-Fold Cross Validation Example
# The cv.glm() function can also be used to implement k-fold CV.
# We once again, set a random seed and initialize a vector in which, 
# we will store the CV errors corresponding to the polynomial fits of orders on to ten.
# here the K = 10
set.seed(17)
help("rep")
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10) $delta[1]
}
cv.error.10
# Notice the computation time is much shorter than LOOCV!, 
# This depends on your laptop performance
# We still see little evidence that using cubic or higher-order polynomials terms, 
# leads to lower test error than simply using a quadratics fit.

#Random Forest Example
install.packages('randomForest')
library(randomForest)
data1 <- read.csv(file.choose(),header = TRUE)
head(data1)
colnames(data1) <- c("BuyingPrice","Maintenance","NumDoors","NumPersons","Bootspace","Safety","Condition")
head(data1)
str(data1)
levels(data1$Condition)
summary(data1)
help(levels)
#Create the train and validation dataset
set.seed(100)
train <- sample(nrow(data1),0.7*nrow(data1),replace = TRUE)
TrainSet <- data1[train,]
ValidationSet <- data1[-train,]
summary(TrainSet)
summary(ValidationSet)

help("randomForest")
model1 <- randomForest(as.factor(Condition)~.,data = TrainSet,importance = TRUE)
model1
# By default, number of tree is 500 and number of variables tried at each split is 2 in this case.

# Fine tuning the parameters of the random forest 
# we have increased the mtry to 6 from 2
# mtry = number of variables randomly sampled as candidates at each split
# note that the default values are different for classification (sqrt(p) where p is number of variables in x) and 
# regression (p/3)
model2 <- randomForest(as.factor(Condition)~.,data = TrainSet, ntree = 500, mtry = 6, importance = TRUE )
model2

# First we will conduct prediction using training set after that we will do prediction using Validation set
# Predicting on training dataset
predTrain <- predict(model2, TrainSet, type = 'class')
# We can use table() to see the classification accuracy
table(predTrain,TrainSet$Condition)
# Predicting on Validation dataset
predValid <- predict(model2, ValidationSet, type = 'class')
table(predValid, ValidationSet$Condition)

# We can also use importance() function to check important variables
# the below functions show the drop in mean accuracy for each of the variables
importance(model2)
varImpPlot(model2)

# Now we will use a for loop to check for different values of mtry.
# using a for loop to identify the right mtry for the model
a = c()
i = 5
for (i in 3:8){
  model3 <- randomForest(as.factor(Condition)~.,data = TrainSet, ntree = 500, mtry = i, importance = TRUE )
  predValid <- predict(model3, ValidationSet, type = 'class')
  a[i-2] = mean(predValid == ValidationSet$Condition)
}
a
plot(3:8,a)

#Compare with the classification tree
library(rpart)
install.packages('caret')
library(caret)
library(e1071)

model_dt <- train(Condition~.,data = TrainSet,method = 'rpart')
model_dt_1 <- predict(model_dt,newdata = TrainSet)
table(model_dt_1,TrainSet$Condition)
mean(model_dt_1==TrainSet$Condition)
model_dt_vs <- predict(model_dt,newdata = ValidationSet)
table(model_dt_vs,ValidationSet$Condition)
mean(model_dt_vs==ValidationSet$Condition)

#Lab Titanic RandomForest on Survived
library(titanic)
data <- titanic_train
data <- na.omit(data)
train_test_split = function(data, fraction = 0.8, train = TRUE) {
  total_rows = nrow(data)
  train_rows = fraction * total_rows
  sample = 1:train_rows
  if (train == TRUE) {
    return (data[sample, ])
  } else {
    return (data[-sample, ])
  }
}
train <- train_test_split(data, 0.8, train = TRUE)
test <- train_test_split(data, 0.8, train = FALSE)
titanic_rf <- randomForest(as.factor(Survived)~.,data= train, importance = TRUE)
predtitanic <- predict(titanic_rf, newdata = test)
table(predtitanic,test$Survived)
mean(predtitanic==test$Survived)
importance(titanic_rf)
varImpPlot(titanic_rf)
