getwd()
setwd("Desktop/RPI/DataAnalytics/Lab/DataAnalytics2020-Yiqing-Chen")
multivariate <- read.csv('multivariate.csv')
attach(multivariate)
mm <- lm(Homeowners~Immigrant)
mm
help(lm)
summary(mm)$coef
# The output above shows the estimate of the regression beta coefficients (column Estimate) and their significance levels (column Pr(>|t|).
# The intercept is 107494.898 and the coefficient of Immigrant variable is -6656.839.
# The estimated regression equation can be written as follow:
# Homeowners = 107494.898 + (-6656.839)*Immigrant 
# We can rewrite it as: Homeowners = 107494.898 - 6656.839*Immigrant.
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
# Predictions can be easily made using the R function predict().
# In the following example, we predict Homeowners for two Immigrant values: 0 and 20.
# you can pass the 0 and 20 values as a concatenated list for Immigrants as follows:
newImmigrantdata <- data.frame(Immigrant = c(0,20))
head(newImmigrantdata)
newp <- predict(mm, newdata = newImmigrantdata)
newp
attributes(mm)
abline(mm,col=3,lwd=3)
mm$coefficients

#Ch2 Graphic Cookbook
#Creating Plots
plot(mtcars$wt,mtcars$mpg)
install.packages("ggplot2")
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
help(qplot)
help(ggplot)

plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)
lines(pressure$temperature,pressure$pressure/2,col='red')
points(pressure$temperature,pressure$pressure/2,col='blue')
qplot(pressure$temperature,pressure$pressure,geom='line')
qplot(temperature,pressure,data=pressure,geom='line')
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

#Creating Bar Graph
barplot(BOD$demand,names.arg= BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generate a table of counts
qplot(mtcars$cyl) #cyl is continuous here
qplot(factor(mtcars$cyl)) #treat cyl as discrete
#Bar Graph of Counts
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

#Creating Histogram
#View the distribution of one-dimensional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg,breaks=10) #specify approximate number of bins with breaks
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)
qplot(mpg,data=mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=5)

#Creating Box-plot
plot(ToothGrowth$supp,ToothGrowth$len)
#Formula Syntax
boxplot(len~supp,data=ToothGrowth)
#put interaction of two variables on x-axis
boxplot(len~supp+dose,data=ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len,geom='boxplot')
qplot(supp,len,data=ToothGrowth,geom='boxplot')
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
#using three separate vectors
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom='boxplot')
qplot(interaction(supp,dose),len,data=ToothGrowth,geom='boxplot')
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()
