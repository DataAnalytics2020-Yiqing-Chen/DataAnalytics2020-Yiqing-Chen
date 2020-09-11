#data frame exercuse
days <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun') #Days
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4) #Temperature in F during the winter
snowed <- c('T','T','F','F','T','T','F') # Snowed on that day; T = true; F= false
help('data.frame')
RPI_Weather_Week <- data.frame(days,temp,snowed) #creating the data frame

RPI_Weather_Week
head(RPI_Weather_Week) #first 6 rows
str(RPI_Weather_Week) #structure of the dataframe
summary(RPI_Weather_Week)

RPI_Weather_Week[1,] #shows the first row and all column
RPI_Weather_Week[,1] #shows the first column and all rows

RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset = snowed == TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

# RPI_Weather_Week[descending_snowed,]
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
# Creating Dataframes
# creating an empty dataframe
empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df
# importing data and exporting data
# writing to a csv file:
setwd("Desktop/RPI/DataAnalytics/Lab/DataAnalytics2020-Yiqing-Chen")
getwd()
write.csv(df,file='saved_df1.csv')
df2 <- read.csv("saved_df1.csv")
df2

install.packages("readxl")
library("readxl")
read_excel("EPI2010_data.xls")

read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")

EPI_data <- read.csv("EPI2010_data.csv",skip = 1)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI_data$EPI
tf <- is.na(EPI)
E <- EPI[!tf]

#Exercise 1
summary(EPI)
fivenum(EPI)
stem(EPI) #stem and leaf plot
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
help(stem)

plot(ecdf(EPI),do.points=FALSE,verticals=TRUE) #cumulative density function
par(pty='s')
qqnorm(EPI);qqline(EPI) #quantile-quantile
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab = "Q-Q Plot for tdsn")
qqline(x)

EPI_data$DALY
tf <- is.na(DALY)
D <- DALY[!tf]

summary(DALY)
fivenum(DALY)
stem(DALY) #stem and leaf plot
hist(DALY)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)

plot(ecdf(DALY),do.points=FALSE,verticals=TRUE) #cumulative density function
par(pty='s')
qqnorm(DALY);qqline(DALY) #quantile-quantile
qqplot(qt(ppoints(250),df=5),DALY,xlab = "Q-Q Plot for tdsn")

EPI_data$WATER_H
tf <- is.na(WATER_H)
WH <- WATER_H[!tf]

summary(WATER_H)
fivenum(WATER_H)
stem(WATER_H) #stem and leaf plot
hist(WATER_H)
hist(WATER_H,seq(30.,95.,1.0),prob=TRUE) #error
lines(density(WATER_H,na.rm=TRUE,bw=1.))
rug(WATER_H)

plot(ecdf(WATER_H),do.points=FALSE,verticals=TRUE) #cumulative density function
par(pty='s')
qqnorm(WATER_H);qqline(WATER_H) #quantile-quantile
qqplot(qt(ppoints(250),df=5),WATER_H,xlab = "Q-Q Plot for tdsn")

boxplot(EPI,DALY)
plot(ecdf(DALY),do.points=FALSE,verticals=TRUE)
qqplot(EPI,DALY)
boxplot(EPI,ENVHEALTH)
plot(ecdf(ENVHEALTH),do.points=FALSE,verticals=TRUE)
qqplot(EPI,ENVHEALTH)
boxplot(ECOSYSTEM,DALY)
plot(ecdf(ECOSYSTEM),do.points=FALSE,verticals=TRUE)
qqplot(ECOSYSTEM,DALY)
boxplot(AIR_H,WATER_H)
plot(ecdf(WATER_H),do.points=FALSE,verticals=TRUE)
qqplot(AIR_H,WATER_H)
boxplot(BIODIVERSITY,EPI)
plot(ecdf(BIODIVERSITY),do.points=FALSE,verticals=TRUE)
qqplot(BIODIVERSITY,EPI)

help(distributions)

EPILand <- EPI[!Landlock]
Eland<- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland,seq(30.,95.,1.0),prob=TRUE)
boxplot(Eland)

#Exercise 2
NSW <- EPI[!No_surface_water]
NSW <- NSW[!is.na(NSW)]
hist(NSW)
hist(NSW,seq(30.,95.,1.0),prob=TRUE)

DE <- EPI[!Desert]
DE <- DE[!is.na(DE)]
hist(DE)
hist(DE,seq(30.,95.,1.0),prob=TRUE)

HPD <- EPI[!High_Population_Density]
HPD <- HPD[!is.na(HPD)]
hist(HPD)
hist(HPD,seq(30.,95.,1.0),prob=TRUE)

EPI_South_Asia <- EPI_data[EPI_data$EPI_regions== "South Asia",]
EPI_South_Asia <- EPI[!EPI_regions == "South Asia"]
EPI_South_Asia

fix(EPI_data)
library("XQuartz")

#GPW3_GRUMP
GPW3 <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW3)
attach(GPW3)
fix(GPW3)
GPW3$Resolution
tf <- is.na(Resolution)
R <- Resolution[!tf]

summary(Resolution)
fivenum(Resolution)
stem(Resolution) #stem and leaf plot
hist(Resolution)
hist(Resolution,seq(30.,95.,1.0),prob=TRUE) #Error, look at the sequence
lines(density(Resolution,na.rm=TRUE,bw=1.))
rug(Resolution)
help(stem)

plot(ecdf(Resolution),do.points=FALSE,verticals=TRUE) #cumulative density function
par(pty='s')
qqnorm(Resolution);qqline(Resolution) #quantile-quantile



#Water_Treatment
WT <- read.csv("water-treatment.csv")
View(WT)
attach(WT)
fix(WT)
WT$COND.D
WT <- is.na(COND.D)
CONDD <- COND.D[!tf]

summary(COND.D)
fivenum(COND.D)
stem(COND.D) #stem and leaf plot
hist(COND.D)
hist(COND.D,seq(30.,95.,1.0),prob=TRUE) #Error, look at the sequence
lines(density(COND.D,na.rm=TRUE,bw=1.))
rug(COND.D)
help(stem)

plot(ecdf(COND.D),do.points=FALSE,verticals=TRUE) #cumulative density function
par(pty='s')
qqnorm(COND.D);qqline(COND.D) #quantile-quantile