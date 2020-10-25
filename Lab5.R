install.packages("titanic")
library(titanic)
data <- titanic_train
View(data)

#pre-processing
data = select(data, Survived, Pclass, Age, Sex, SibSp, Parch)
data <- na.omit(data)
data$Survived <- factor(data$Survived)
data$Pclass <- factor(data$Pclass,order = TRUE, levels = c(3,2,1))

#Visualizations
ggplot(data, aes(x = Survived)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

ggplot(data, aes(x = Survived, fill=Sex)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

ggplot(data, aes(x = Survived, fill=Pclass)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_classic()

ggplot(data, aes(x = Age)) +
  geom_density(fill='coral')

# Discretize age to plot survival
data$Discretized.age = cut(data$Age, c(0,10,20,30,40,50,60,70,80,100))
# Plot discretized age
ggplot(data, aes(x = Discretized.age, fill=Survived)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', aes(label=stat(count)), position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()
data$Discretized.age = NULL

#rpart titanic
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
rpart.fit <- rpart(train$Survived~.,data = train, method = 'class')
rpart.plot(rpart.fit, extra = 106)
rpart.p <- predict(rpart.fit,test,type = 'class')
rpart.table <- table(test$Survived, rpart.p)
rpart_accuracy = sum(diag(rpart.table)) / sum(rpart.table)
rpart_accuracy
View(rpart.p)
plot(rpart.p)

#ctree
require(party)
train_c = select(train,Survived,Pclass,Age)
train_c = na.omit(train_c)
train_c$Survived = is.numeric(train_c$Survived)
train_c$Pclass = is.numeric(train_c$Pclass)
train_c$Age = is.numeric(train_c$Age)
ctree_t <-ctree(train_c$Survived ~ train_c$Pclass+train_c$Age, data = train_c)

#hclust
install.packages('stats')
require(stats)
data_h = select(data, Survived, Pclass, Age)
data_h <- na.omit(data_h)
data_h$Survived = is.numeric(data_h$Survived)
data_h$Pclass = is.numeric(data_h$Pclass)
data_h$Age = is.numeric(data_h$Age)
data_h <- scale(data_h)
g_h <- dist(data_h,method = 'euclidean')
titanic_hclust <- hclust(g_h,method = 'complete')
plot(titanic_hclust)