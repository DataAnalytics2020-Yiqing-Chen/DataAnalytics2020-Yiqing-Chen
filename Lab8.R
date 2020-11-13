#in-class SVM
data("iris")
head(iris) 
str(iris)
library(ggplot2)
library(e1071)
# we will separate the Species based on the color and plot with Petal.Legth Vs Petal.Width
# We can clearly see the separation of Setosa however, there is an overlapping in Versicolor and Virginica.
# Now we plot using the qplot() fuction,  X=Petal.Length, Y = Petal.Width using the color separation respect to Species.
qplot(Petal.Length, Petal.Width, data=iris, color = Species)
# Now we can use the built in svm() function that comes in the e1071 library
# here we will name our first svm model as #svm_model1
help("svm")
svm_model1 <- svm(Species~., data = iris)
summary(svm_model1)
# We have 51 support vectors, 8 of them belong to Setosa
plot(svm_model1, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
pred1 <- predict(svm_model1, iris)
table1 <- table(Predicted = pred1, Actual = iris$Species)
table1
#there are two misclassifications in Virginica and Versicolor
Model1_accuracyRate = sum(diag(table1))/sum(table1)
Model1_accuracyRate
Model1_MissClassificationRate = 1 - Model1_accuracyRate
Model1_MissClassificationRate

#model2 - kernel = 'linear'
# Now we will use other methods such as linear, polynomial... for kernels 
svm_model2 <- svm(Species~., data = iris, kernel = "linear")
summary(svm_model2)
plot(svm_model2, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
pred2 <- predict(svm_model2, iris)
table2 <- table(Predicted = pred2, Actual = iris$Species)
table2
Model2_accuracyRate = sum(diag(table2))/sum(table2)
Model2_accuracyRate
Model2_MissClassificationRate = 1 - Model2_accuracyRate
Model2_MissClassificationRate

#model3 - kernel = 'polynomial'
svm_model3 <- svm(Species~., data = iris, kernel = "polynomial")
summary(svm_model3)
plot(svm_model3, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
pred3 <- predict(svm_model3, iris)
table3 <- table(Predicted = pred3, Actual = iris$Species)
table3
Model3_accuracyRate = sum(diag(table3))/sum(table3)
Model3_accuracyRate
Model3_MissClassificationRate = 1 - Model3_accuracyRate
Model3_MissClassificationRate

#Lab8 SVM
set.seed (1)
# We now use the svm() function to fit the support vector classifier for a given value of the cost parameter.
# Here we demonstrate the use of this function on a two-dimensional example so that we can plot the resulting 
# decision boundary.
# We begin by generating the observations, which belong to two classes.
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y
# We begin by checking whether the classes are linearly separable.
plot(x, col=(3-y))
# They are not. Next, we fit the support vector classifier. 
# Note that in order for the svm() function to perform classification
# we must encode the response as a factor variable.
# We now create a data frame with the response coded as a factor.
dat <- data.frame(x = x,y  = as.factor(y))
svmfit <- svm(y ~., data=dat, kernel="linear", cost=10,scale=FALSE)
# The argument scale=FALSE tells the svm() function not to scale each feature to 
# have mean zero or standard deviation one;
# depending on the application, one might prefer to use scale=TRUE.

# We can now plot the support vector classifier obtained:
plot(svmfit , dat)
# We see that in this case only one observation is misclassified. 
# Note that the two arguments to the plot.svm() function are the output of the call to svm(), 
# as well as the data used in the call to svm(). 
# The region of feature space that will be assigned to the −1 class is shown in light blue, 
# and the region that will be assigned to the +1 class is shown in purple.

# We can determine the identities of those support vectors by:
svmfit$index
# You can see 1,2,5,7,14,16 and 17
summary(svmfit)
# What if we instead used a smaller value of the cost parameter? 
# now cost = 0.1
svmfit <- svm(y ~., data=dat, kernel="linear", cost = 0.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index
# Unfortunately, the svm() function does not explicitly output the coefficients of the linear decision boundary 
# obtained when the support vector classifier is fit, nor does it output the width of the margin
# The e1071 library includes a built-in function, tune(), to perform cross-validation. 
# By default, tune() performs ten-fold cross-validation on a set of models of interest.
# The following command indicates that we want to compare SVMs with a linear kernel, 
# using a range of values of the cost parameter. 

set.seed (1)
tune.out <- tune(svm, y ~.,data=dat,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
# We can easily access the cross-validation errors for each of these models using the summary() command:
summary(tune.out)
# We see that cost=0.1 results in the lowest cross-validation error rate.
# The tune() function stores the best model obtained, which can be accessed as follows:
bestmod=tune.out$best.model 
summary(bestmod)
# The predict() function can be used to predict the class label on a set of test observations,
# at any given value of the cost parameter. We begin by generating a test data set.
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
# Now we predict the class labels of these test observations. 
# Here we use the best model obtained through cross-validation in order to make predictions.
ypred <-predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)

#Thus, with this value of cost, 17 of the test observations are correctly classified. 
# What if we had instead used cost= 0.01? 
svmfit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)

# In this case 6 observations are misclassified.
# Now consider a situation in which the two classes are linearly separable.
# Then we can find a separating hyperplane using the svm() function. 
# We first further separate the two classes in our simulated data so that they are linearly separable:
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

# Now the observations are just barely linearly separable.
# We fit the support vector classifier and plot the resulting hyperplane,  
# using a very large value of cost so that no observations are misclassified.
dat=data.frame(x=x,y=as.factor(y))
svmfit <-svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit,dat)

# No training errors were made and only three support vectors were used.
# However, we can see from the figure that the margin is
# very narrow (because the observations that are not support vectors, indicated as circles, are very 
# close to the decision boundary). It seems likely that this model will perform poorly on test data. 
# We now try a smaller value of cost:

svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)
# Using cost=1, we misclassify a training observation, but we also obtain a much wider margin and make 
# use of seven support vectors. 
# It seems likely that this model will perform better on test data than the model with cost=1e5.

# We now examine the Khan data set, which consists of a number of tissue samples 
# corresponding to four distinct types of small round blue cell tumors
# For each tissue sample, gene expression measurements are available. 
#The data set consists of training data, xtrain and ytrain, and testing data, xtest and ytest.
library(ISLR)
names(Khan)
# Let's examine the dimension of the data:
# This data set consists of expression measurements for 2,308 genes.
# The training and test sets consist of 63 and 20 observations respectively
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

# We will use a support vector approach to predict cancer subtype using gene expression measurements.
# In this data set, there are a very large number of features relative to the number of observations.
# This suggests that we should use a linear kernel, because the additional flexibility that will 
# result from using a polynomial or radial kernel is unnecessary.
dat <- data.frame(x=Khan$xtrain , y = as.factor(Khan$ytrain ))
out <- svm(y ~., data=dat, kernel="linear",cost=10)
summary(out)

# We see that there are no training errors. In fact, this is not surprising, because the large number 
# of variables relative to the number of observations implies that it is easy to find hyperplanes that 
# fully separate the classes. 
# We are most interested not in the support vector classifier’s performance on the training observations, 
# but rather its performance on the test observations.

dat.te=data.frame(x=Khan$xtest , y = as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
# We see that using cost=10 yields two test set errors on this data

# kernlab
install.packages("kernlab")
library("kernlab")
data("iris")
irismodel <- ksvm(Species ~ ., data = iris,
                  type = "C-bsvc", kernel = "rbfdot",
                  kpar = list(sigma = 0.1), C = 10,
                  prob.model = TRUE)
irismodel
predict(irismodel, iris[c(3, 10, 56, 68,
                          + 107, 120), -5], type = "probabilities")
predict(irismodel, iris[c(3, 10, 56, 68,
                          + 107, 120), -5], type = "decision")
k <- function(x, y) {
   (sum(x * y) + 1) * exp(0.001 * sum((x -
                                           y)^2))
}
class(k) <- "kernel"
data("promotergene")
gene <- ksvm(Class ~ ., data = promotergene,
                 kernel = k, C = 10, cross = 5)
gene
x <- rbind(matrix(rnorm(120), , 2), matrix(rnorm(120,
                                                mean = 3), , 2))
y <- matrix(c(rep(1, 60), rep(-1, 60)))
svp <- ksvm(x, y, type = "C-svc", kernel = "rbfdot",
               kpar = list(sigma = 2))
plot(svp)

#klaR
install.packages("klaR")
library("MASS")
library("klaR")
data("B3")
Bmod <- svmlight(PHASEN ~ ., data = B3,
                   svm.options = "-c 10 -t 2 -g 0.1 -v 0")
predict(Bmod, B3[c(4, 9, 30, 60, 80, 120),
                   -1])

#SVM Path
install.packages("svmpath")
library("svmpath")
data("svmpath")
attach(balanced.overlap)
svmpm <- svmpath(x, y, kernel.function = radial.kernel,
                  param.kernel = 0.1)
predict(svmpm, x, lambda = 0.1)
predict(svmpm, lambda = 0.2, type = "alpha")
