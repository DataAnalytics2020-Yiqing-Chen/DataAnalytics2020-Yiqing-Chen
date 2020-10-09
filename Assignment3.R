#Question 1a
nyt3 <- read.csv('nyt3.csv')
nyt7 <- read.csv('nyt7.csv')
nyt13 <- read.csv('nyt13.csv')
nyt18 <- read.csv('nyt18.csv')
nyt27 <- read.csv('nyt27.csv')
View(nyt3)
boxplot(nyt3$Age,nyt7$Age,nyt13$Age,nyt18$Age,nyt27$Age)
boxplot(nyt3$Impressions,nyt7$Impressions,nyt13$Impressions,nyt18$Impressions,nyt27$Impressions)

#Question1b
ggplot(nyt3,aes(x=nyt3$Age))+geom_histogram(binwidth=8)
ggplot(nyt7,aes(x=nyt7$Age))+geom_histogram(binwidth=8)
ggplot(nyt13,aes(x=nyt13$Age))+geom_histogram(binwidth=8)
ggplot(nyt18,aes(x=nyt18$Age))+geom_histogram(binwidth=8)
ggplot(nyt27,aes(x=nyt27$Age))+geom_histogram(binwidth=8)
ggplot(nyt3,aes(x=nyt3$Impressions))+geom_histogram(binwidth=1)
ggplot(nyt7,aes(x=nyt7$Impressions))+geom_histogram(binwidth=1)
ggplot(nyt13,aes(x=nyt13$Impressions))+geom_histogram(binwidth=1)
ggplot(nyt18,aes(x=nyt18$Impressions))+geom_histogram(binwidth=1)
ggplot(nyt27,aes(x=nyt27$Impressions))+geom_histogram(binwidth=1)

#Question1c
plot(ecdf(nyt3$Age),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt7$Age),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt13$Age),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt18$Age),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt27$Age),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt3$Impressions),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt7$Impressions),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt13$Impressions),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt18$Impressions),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt27$Impressions),do.points=FALSE,verticals=TRUE)
qqplot(qt(ppoints(100),df=3),nyt3$Age,xlab = "Q-Q Plot for Age")
qqnorm(nyt3$Age);qqline(nyt3$Age)
qqplot(qt(ppoints(100),df=3),nyt7$Age,xlab = "Q-Q Plot for Age")
qqplot(qt(ppoints(100),df=3),nyt13$Age,xlab = "Q-Q Plot for Age")
qqplot(qt(ppoints(100),df=3),nyt18$Age,xlab = "Q-Q Plot for Age")
qqplot(qt(ppoints(100),df=3),nyt27$Age,xlab = "Q-Q Plot for Age")
qqplot(qt(ppoints(100),df=3),nyt3$Impressions,xlab = "Q-Q Plot for Impressions")
qqplot(qt(ppoints(100),df=3),nyt7$Impressions,xlab = "Q-Q Plot for Impressions")
qqplot(qt(ppoints(100),df=3),nyt13$Impressions,xlab = "Q-Q Plot for Impressions")
qqplot(qt(ppoints(100),df=3),nyt18$Impressions,xlab = "Q-Q Plot for Impressions")
qqplot(qt(ppoints(100),df=3),nyt28$Impressions,xlab = "Q-Q Plot for Impressions")
#https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot

#Question1d
library(dplyr)
set.seed(1234)
age3 <- sample_n(data.frame(nyt3),5000)
shapiro.test(age3$Age)
wilcox.test(age3$Age,mu=20,conf.int=TRUE)
age7 <- sample_n(data.frame(nyt7),5000)
shapiro.test(age7$Age)
age13 <- sample_n(data.frame(nyt13),5000)
shapiro.test(age13$Age)
age18 <- sample_n(data.frame(nyt18),5000)
shapiro.test(age18$Age)
age27 <- sample_n(data.frame(nyt27),5000)
shapiro.test(age27$Age)

shapiro.test(age3$Impressions)
shapiro.test(age7$Impressions)
shapiro.test(age13$Impressions)
shapiro.test(age18$Impressions)
shapiro.test(age27$Impressions)

library(dplyr)
#delete the 0s from Age column
#remove the outliers from Age and Impressions columns
#normalize the copied datasets

ageno03 <- nyt3 %>% filter(Age != 0)
ageno03 <- ageno03[ageno03$Age > quantile(ageno03$Age, .25) - 1.5*IQR(ageno03$Age) & 
          ageno03$Age < quantile(ageno03$Age, .75) + 1.5*IQR(ageno03$Age), ]
ageno03 <- ageno03[ageno03$Impressions > quantile(ageno03$Impressions, .25) - 1.5*IQR(ageno03$Impressions) & 
                     ageno03$Impressions < quantile(ageno03$Impressions, .75) + 1.5*IQR(ageno03$Impressions), ]
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
ageno03[1,3]<-as.data.frame(lapply(ageno03[1,3],normalize))

ageno07 <- nyt7 %>% filter(Age != 0)
ageno07 <- ageno07[ageno07$Age > quantile(ageno07$Age, .25) - 1.5*IQR(ageno07$Age) & 
                     ageno07$Age < quantile(ageno07$Age, .75) + 1.5*IQR(ageno07$Age), ]
ageno07 <- ageno07[ageno07$Impressions > quantile(ageno07$Impressions, .25) - 1.5*IQR(ageno07$Impressions) & 
                     ageno07$Impressions < quantile(ageno07$Impressions, .75) + 1.5*IQR(ageno07$Impressions), ]
ageno07[1,3]<-as.data.frame(lapply(ageno07[1,3],normalize))

#perform the graphics again
boxplot(ageno03$Age,ageno07$Age)
boxplot(ageno03$Impressions,ageno07$Impressions)
ggplot(ageno03,aes(x=ageno03$Age))+geom_histogram(binwidth=8)
ggplot(ageno03,aes(x=ageno03$Impressions))+geom_histogram(binwidth=1)
ggplot(ageno07,aes(x=ageno07$Age))+geom_histogram(binwidth=8)
ggplot(ageno07,aes(x=ageno07$Impressions))+geom_histogram(binwidth=1)
plot(ecdf(ageno03$Age),do.points=FALSE,verticals=TRUE)
plot(ecdf(ageno03$Impressions),do.points=FALSE,verticals=TRUE)
plot(ecdf(ageno03$Age),do.points=FALSE,verticals=TRUE)
plot(ecdf(ageno07$Impressions),do.points=FALSE,verticals=TRUE)
qqnorm(ageno03$Age);qqline(ageno03$Age,lwd = 2, col = 3)
qqnorm(ageno07$Age);qqline(ageno07$Age,lwd = 2, col = 3)
qqnorm(ageno03$Impressions);qqline(ageno03$Impressions,lwd = 2, col = 3)
qqnorm(ageno07$Impressions);qqline(ageno07$Impressions,lwd = 2, col = 3)

#redo the shapiro test to test normality
ageno03s<- sample_n(data.frame(ageno03),5000)
wilcox.test(ageno03s$Age,mu=20,conf.int=TRUE)
shapiro.test(ageno03s$Age)
shapiro.test(ageno03s$Impressions)

ageno07s<- sample_n(data.frame(ageno07),5000)
shapiro.test(ageno07s$Age)
shapiro.test(ageno07s$Impressions)

ageno1 <- nyt3[apply(data.frame(nyt3$Age)!=0, 1, all),] #another way to eliminate the 0s


