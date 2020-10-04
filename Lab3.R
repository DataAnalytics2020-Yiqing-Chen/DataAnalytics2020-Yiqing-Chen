getwd()
setwd("Desktop/RPI/DataAnalytics/Lab/DataAnalytics2020-Yiqing-Chen")

#Regression Tree
library(rpart)
library(ggplot2)
library(rpart.plot)
install.packages(rpart.plot)
data('msleep')
str(msleep)
help('msleep')
str(data)
# creating a new data frame with the following columns included.
msleepdf1 <- msleep[,c(3,6,10,11)] # 3 = vore ,6=sleep_total, 10=brainwt, 11=bodywt
str(msleepdf1)
head(msleepdf1)
# Building Regression Decision Tree that predicts the total sleeping
# hours of the mammals based on the other variables available on the dataset
help('rpart')
sleepmodel_1 <- rpart(sleep_total~.,data = msleepdf1,method = 'anova')
# method we are using here is anova because our target here is sleep_total is a numerical one.
sleepmodel_1
# let's visualize this using rpart.plot()
help(rpart.plot)
rpart.plot(sleepmodel_1,type=3,fallen.leaves = TRUE)
# type = 3, Draw separate split labels for the left and right directions. See the documentation 
# fallen.leaves = TRUE,  Default TRUE to position the leaf nodes at the bottom of the graph. 
# It can be helpful to use FALSE if the graph is too crowded and the text size is too small.
rpart.plot(sleepmodel_1,type=3,digits=3,fallen.leaves = TRUE) # with 3 digits
rpart.plot(sleepmodel_1,type=3,digits=4,fallen.leaves = TRUE)

#Classification Tree
install.packages('C50')
require(C50)
data('iris')
head(iris)
str(iris)
table(iris$Species)
# It is important to set the seed in order to get the same randomly generated numbers 
# when we run the model over and over.
set.seed(9850)
# generate random numbers
grn <- runif(nrow(iris))
# creating a randomized iris dataset, shuffling the dataset
# we use the order() function along with the random numbers we generated
irisrand <- iris[order(grn),]
# observe that rows are now randomly shuffled
str(irisrand)
classificationmodel1 <- C5.0(irisrand[1:100,-5],irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)
# now we will do the prediction using the predict() function
# We are using the remaining last 50 rows for #here starting from 101 row to 150th row
prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1
# we will use the confusion matrix to understand our prediction
# Read the documentation for the table() function in RStudio help
table(irisrand[101:150,5],prediction1)
# you can write the same above line by defining what is the "predicted"
# table(irisrand[101:150,5],Predicted = prediction1)
# We can plot the classification model tree #using the plot() function
plot(classificationmodel1)

#Naive Bayes Classifier
library('e1071')
install.packages('e1071')
classifier<-naiveBayes(iris[,1:4],iris[,5])
table(predict(classifier,iris[,-5]),iris[,5],dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x,1.462,0.1736640),0,8,col='red',main='Petal Length Distribution for the 3 Different species')
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green") 

#Lab3_Ctree1
require(rpart)
swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) 
text(swiss_rpart)
install.packages('party')
require(party)
treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)
cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))
treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)
cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.
install.packages('tree')
library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)

#Lab3_Ctree2
# Conditional Inference Tree for Mileage
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
# plot tree
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)

#Lab3_Ctree3
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosisâ€")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")


