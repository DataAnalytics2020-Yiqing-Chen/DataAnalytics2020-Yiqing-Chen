#In-Class Work
#Loess Example
# LOESS Exaplme from:
# http://r-statistics.co/Loess-Regression-With-R.html
data(economics, package="ggplot2") # load data
economics$index <- 1:nrow(economics) # create index variable
economics <- economics[1:80, ] # retail 80rows for better graphical understanding
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span
# Predict Loess
smoothed10 <- predict(loessMod10)
smoothed25 <- predict(loessMod25)
smoothed50 <- predict(loessMod50)
# From above plot, you would notice that as the span increases, the smoothing of the curve also increases.
# Code for Plot
# Plot it
plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction",
     xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")

#LOWESS example using the Cars dataset
# Fitting a curve to the data
# Local regression or local polynomial regression, also known as moving regression is a generalization
# of moving average and polynomial regression. It is one of the most common methods,
# initially developed for scatterplot smoothing, are LOESS (locally estimated scatterplot smoothing) and
# LOWESS (locally weighted scatterplot smoothing),
# LOWESS example using the Cars dataset
data("cars")

str(cars) # we see 50 observation and 2 variables
summary(cars)
# now we create a plot, speed Vs distance
plot(speed ~ dist, data = cars)
# When we look at the plot, we see that there is a positive relationship between these two variables
help("lowess")
lowess(cars$speed ~ cars$dist)
# Now we will use the lowess() function along with the line() function to draw the lines
lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue")
# here the f value is the the smoother span. f= 2/3 = 0.666 
# the default value for smoother span is 0.666 in RStudio.

#This gives the proportion of points in the plot which influence the smooth at each value.
# Larger values give more smoothness.
# Change the "f" value and observe the shape of the line.
# lines(lowess(cars$speed ~ cars$dist, f=0.75), col="gray") # f = 0.75
lines(lowess(cars$speed ~ cars$dist, f=0.8), col="red") # f = 0.8
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="green") # f = 0.9
lines(lowess(cars$speed ~ cars$dist, f=0.1), col= 5) # f = 0.1
lines(lowess(cars$speed ~ cars$dist, f=0.01), col= 6) # f = 0.01
# Observe that, when we try to have a very lower values for "f", in this example, it will try to overfit points.

#Logistic Regression
# Logistic Regression Example
# Introduction to Statistical Learning with R 7th Edition – Chapter 4 Lab: 4.6.1
library(ISLR)
data("Smarket")
head(Smarket)
# data set consists ofpercentage returns for the S&P 500 stock index over 1, 250 days, from the
#beginning of 2001 until the end of 2005
names(Smarket)
# For each date, we have recorded the percentage returns for each of the five previous
# trading days, "Lag1" through "Lag5".
# We have also recorded "Volume" (the number of shares traded on the previous day, in billions of dollars.
# "Today" is the percentage return on the date
# "Direction" (whether the market was Up or Down on this date)
dim(Smarket)
summary(Smarket)
cor(Smarket)
# cor() function produces a matrix that contains all of the pairwise
# correlations among the predictors in a data set. The first command below
# gives an error message because the "Direction" variable is qualitative.
cor(Smarket[,-9]) # we ommit the 9th column: "Direction"
# the correlations between the lag variables and today’s
# returns are close to zero. In other words, there appears to be little
# correlation between today’s returns and previous days’ returns. The only
# substantial correlation is between Year and Volume
attach(Smarket)
plot(Volume)

# Now we will fit a Logistic Regression model in order to predict "Direction"
# using "Lag1" through "Lag5" and "Volume".
# The glm() function fits generalized glm() linear models,
# a class of models that includes logistic regression.
help("glm") # Read the glm() function documentation.
# glm() function is similar to that of lm(), except that we must pass in
# the argument family=binomial in order to tell R to run a logistic regression
# rather than some other type of generalized linear model
glm.fit.model1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fit.model1)
# You can see that the smallest p-value here is associated with Lag1.
# The negative coefficient for this predictor suggests that if the market
# had a positive return yesterday, then it is less likely to go up .
# However, at a value of 0.145, the p-value is still relatively large,
# and so there is no clear evidence of a real association
# between "Lag1" and "Direction".

# The predict() function can be used to predict the probability that the
# market will go up, given values of the predictors.
# The The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed
# to other information such as the logit.
# If no data set is supplied to the predict() function, then the probabilities are computed for the training
# data that was used to fit the logistic regression model.
# Here we have printed only the first ten probabilities. We know that these values correspond to
# the probability of the market going up, rather than down, because the
# contrasts() function indicates that R has created a dummy variable with a 1 for Up.
glm.probs <- predict(glm.fit.model1, type="response")
glm.probs[1:10]
contrasts(Direction)

# In order to make a prediction as to whether the market will go up or
# down on a particular day, we must convert these predicted probabilities
# into class labels, Up or Down.
# The following two commands create a vector of class predictions based on
# whether the predicted probability of a market increase is greater than or less than 0.5
help("rep") # Read the documentation for rep() function which replicates elements of vectors.
glm.pred <- rep("Down", 1250) # this command creates a vector of 1,250 "Down" elements
glm.pred[glm.probs > 0.5] = "Up"
# The above command transforms to "Up" all of the elements for which the predicted probability
# of a market increase exceeds 0.5

# Now we can use the table() function.
# Given these predictions, the table() function
# can be used to produce a confusion matrix in order to determine how many
# observations were correctly or incorrectly classified.
table(glm.pred, Direction)

# The diagonal elements of the confusion matrix indicate correct predictions,
# while the off-diagonals represent incorrect predictions
# Hence our model correctly predicted that the market would go up on 507 days and that
# it would go down on 145 days, for a total of 507 + 145 = 652 correct predictions
(507+145)/1250
# 0.5216
# Also we can use the mean() function and that can be used to compute the fraction of
# days for which the prediction was correct.
# In this case, logistic regression correctly predicted the movement of the market 52.2% of the time.
mean(glm.pred == Direction)
# 0.5216

# Next, in order to implement this strategy, we will first create a vector corresponding
# to the observations from 2001 through 2004. We will then use this vector
# to create a held out data set of observations from 2005.
train <- (Year <2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# We now we will fit a logistic regression model using only the subset of the observations
# that correspond to dates before 2005, using the subset argument.
# We then obtain predicted probabilities of the stock market going up for
# each of the days in our test set—that is, for the days in 2005.
glm.fit.model2 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 +Volume, data=Smarket,
                      family = binomial, subset = train)
glm.prob2 = predict(glm.fit.model2, Smarket.2005, type="response")
# Notice that we have trained and tested our model on two completely separate
# data sets: training was performed using only the dates before 2005,
# and testing was performed using only the dates in 2005.

# Finally, we compute the predictions for 2005 and compare them to the actual movements
# of the market over that time period.
glm.pred2 <- rep("Down", 252)
glm.pred2[glm.prob2 > 0.5] = "Up"
table(glm.pred2, Direction.2005)

mean(glm.pred2 == Direction.2005)
mean(glm.pred2 != Direction.2005)
# The != notation means not equal to, and so the last command computes
# the test set error rate. The results are rather disappointing: the test error
# rate is 52 %, which is worse than random guessing! Of course this result
# is not all that surprising, given that one would not generally expect to be
# able to use previous days’ returns to predict future market performance.

glm.fit.model3 <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial,
                      subset = train)
glm.probs3 <- predict(glm.fit.model3, Smarket.2005, type = "response")
glm.pred3 <- rep("Down", 252)
glm.pred3[glm.probs3 > 0.5] = "Up"
table(glm.pred3, Direction.2005)
mean(glm.pred3 == Direction.2005)
# 0.5595238 approximately 0.56

# Linear Discriminant Analysis Example using Iris dataset.
# In order to use the lda() function, you need to have the MASS library.
# Multiclass Classification
library(MASS)
names(iris)
dim(iris) # check the dimensions of the iris dataset, you will see 150 rows and 5 columns
head(iris)

# Creating the training dataset using the Random sampling using the
sample()
# we will allocate half of the dataset to train the model that we are planning to build.
# setting the seed value
set.seed(555)
Train <- sample(1:nrow(iris), nrow(iris)/2)
iris_Train <- iris[Train,] # Traning dataset
irist_Test <- iris[-Train,] # Testing dataset

# Read the lda() function documentation on RStudio
help(lda)
# now we will use the lda() function to fit the model
fit1 <- lda(Species ~ Sepal.Length + Sepal.Width +
              Petal.Length + Petal.Width, data = iris_Train)
# We will use the predict() function to conduct the prediction based on the fit1 model we built
# with the Testing dataset
predict1 <- predict(fit1, iris_Train)
predict1_class <- predict1$class

# generating the confusion matrix using the table() function
table1 <- table(predict1_class, iris_Train$Species)
table1
# Calculating the Accuracy of the prediction
sum(diag(table1))/sum(table1)

# Lab11
require(ggplot2)        # or load package first
data(diamonds)
head(diamonds)          # look at the data!
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~ cut)
ggplot(diamonds) + geom_histogram(aes(x=price)) + geom_vline(xintercept=12000)

ggplot(
  data = diamonds,
  mapping = aes(color = cut_number(carat, 5), x = price)
) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, colour = cut)) +
  geom_boxplot()

# Work on the Textbook (Intro to Statistical Learning with R - 7th Edition) Chapter 4.6.3 on 
# Linear Discriminant Analysis Lab example 
library(MASS)
library(ISLR)
# Introduction to Statistical Learning with R 7th Edition – Chapter 4 Lab: 4.6.1
# We will perform LDA on the Smarket dataset
# data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, from the
# beginning of 2001 until the end of 2005

# Read the lda() function documentation on RStudio
help(lda)
# now we will use the lda() function to fit the model
# Notice that the
# lda() syntax for the lda() function is identical to that of lm(), and to that of
# glm() except for the absence of the family option.

data("Smarket")
attach(Smarket)
head(Smarket)
# data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, from the
# beginning of 2001 until the end of 2005
names(Smarket)
str(Smarket)
dim(Smarket)
# For each date, we have recorded the percentage returns for each of the five previous
# trading days, "Lag1" through "Lag5".
# We have also recorded "Volume" (the number of shares traded on the previous day, in billions of dollars.
# "Today" is the percentage return on the date
# "Direction" (whether the market was Up or Down on this date)

# Next, we will first create a vector corresponding
# to the observations from 2001 through 2004. We will then use this vector
# to create a held out data set of observations from 2005.
train <- (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# We fit the model using only the observations before 2005.

lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
# The LDA output indicates that ˆπ1 = 0.492 and ˆπ2 = 0.508; in other words,
# 49.2% of the training observations correspond to days during which the
# market went down

# The predict() function returns a list with three elements. The first element,
# class, contains LDA’s predictions about the movement of the market.
# The second element, posterior, is a matrix whose kth column contains the
# posterior probability that the corresponding observation belongs to the kth class,
# x contains the linear discriminants

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)

#Lab1_Loess1
data(cars)
cars.lo <- loess(dist ~ speed, cars)
predict(cars.lo, data.frame(speed = seq(5, 30, 1)), se = TRUE)
# to allow extrapolation
cars.lo2 <- loess(dist ~ speed, cars, control = loess.control(surface = "direct"))
predict(cars.lo2, data.frame(speed = seq(5, 30, 1)), se = TRUE)

# and
# http://archive.ics.uci.edu/ml/datasets/Student+Performance
# http://archive.ics.uci.edu/ml/datasets/NoisyOffice
# http://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

#Lab1_Loess2
require(graphics)

plot(cars, main = "lowess(cars)")
lines(lowess(cars), col = 2)
lines(lowess(cars, f = .2), col = 3)
legend(5, 120, c(paste("f = ", c("2/3", ".2"))), lty = 1, col = 2:3)

# and
# http://archive.ics.uci.edu/ml/datasets/Student+Performance
# http://archive.ics.uci.edu/ml/datasets/NoisyOffice
# http://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

#Lab1_Loess3
require(graphics)

with(cars, {
  plot(speed, dist)
  lines(supsmu(speed, dist))
  lines(supsmu(speed, dist, bass = 7), lty = 2)
})

# and
# http://archive.ics.uci.edu/ml/datasets/Student+Performance
# http://archive.ics.uci.edu/ml/datasets/NoisyOffice
# http://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

#Lab1_Loess4
require(graphics)
data(cars)

attach(cars)
plot(speed, dist, main = "data(cars)  &  smoothing splines")
cars.spl <- smooth.spline(speed, dist)
(cars.spl)
## This example has duplicate points, so avoid cv = TRUE

lines(cars.spl, col = "blue")
lines(smooth.spline(speed, dist, df = 10), lty = 2, col = "red")
legend(5,120,c(paste("default [C.V.] => df =",round(cars.spl$df,1)),
               "s( * , df = 10)"), col = c("blue","red"), lty = 1:2,
       bg = 'bisque')
detach()


## Residual (Tukey Anscombe) plot:
plot(residuals(cars.spl) ~ fitted(cars.spl))
abline(h = 0, col = "gray")

## consistency check:
stopifnot(all.equal(cars$dist,
                    fitted(cars.spl) + residuals(cars.spl)))

## Visualize the behavior of  .nknots.smspl()
nKnots <- Vectorize(.nknots.smspl) ; c.. <- adjustcolor("gray20",.5)
curve(nKnots, 1, 250, n=250)
abline(0,1, lty=2, col=c..); text(90,90,"y = x", col=c.., adj=-.25)
abline(h=100,lty=2); abline(v=200, lty=2)

n <- c(1:799, seq(800, 3490, by=10), seq(3500, 10000, by = 50))
plot(n, nKnots(n), type="l", main = "Vectorize(.nknots.smspl) (n)")
abline(0,1, lty=2, col=c..); text(180,180,"y = x", col=c..)
n0 <- c(50, 200, 800, 3200); c0 <- adjustcolor("blue3", .5)
lines(n0, nKnots(n0), type="h", col=c0)
axis(1, at=n0, line=-2, col.ticks=c0, col=NA, col.axis=c0)
axis(4, at=.nknots.smspl(10000), line=-.5, col=c..,col.axis=c.., las=1)

##-- artificial example
y18 <- c(1:3, 5, 4, 7:3, 2*(2:5), rep(10, 4))
xx  <- seq(1, length(y18), len = 201)
(s2  <- smooth.spline(y18)) # GCV
(s02  <- smooth.spline(y18, spar = 0.2))
(s02. <- smooth.spline(y18, spar = 0.2, cv = NA))
plot(y18, main = deparse(s2$call), col.main = 2)
lines(s2, col = "gray"); lines(predict(s2, xx), col = 2)
lines(predict(s02, xx), col = 3); mtext(deparse(s02$call), col = 3)

## The following shows the problematic behavior of 'spar' searching:
(s2  <- smooth.spline(y18, control =
                        list(trace = TRUE, tol = 1e-6, low = -1.5)))
(s2m <- smooth.spline(y18, cv = TRUE, control =
                        list(trace = TRUE, tol = 1e-6, low = -1.5)))
## both above do quite similarly (Df = 8.5 +- 0.2)

#Lab2_lda1
library(kknn)
data(ionosphere)
# class is categorical var.
i2<-ionosphere[,-1]
i2<-i2[,-1]
# class is categorical var.
library(MASS)
ifit <- lda(class ~ ., data=i2, na.action="na.omit", CV=TRUE)
summary(ifit)

ctable <- table(i2$class, ifit$class)
diag(prop.table(ctable, 1))
# total percent correct
sum(diag(prop.table(ctable)))

