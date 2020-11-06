# in-class PCA

data("USArrests")

states=row.names(USArrests) 
states
# The columns of the data set contain the four variables.
names(USArrests)
# We first briefly examine the data. We notice that the variables have vastly different means.
str(USArrests)

# Note that the apply() function allows us to apply a function—in this case, the mean() function 
# to each row or column of the data set.
# The second input here denotes whether we wish to compute the mean of the rows, 1, or the columns, 2.
# We see that there are on average three times as many rapes as murders, 
# and more than eight times as many assaults as rapes.
apply(USArrests, 2, mean)
# We can also examine the variances of the four variables using the apply() function.
apply(USArrests, 2, var)

# Not surprisingly, the variables also have vastly different variances: the UrbanPop variable measures the percentage of the population 
# in each state living in an urban area, which is not a comparable number to the number of rapes in each state per 100,000 individuals. 
# If we failed to scale the variables before performing PCA, 
# then most of the principal components that we observed would be driven by the Assault variable, 
# since it has by far the largest mean and variance. Thus, it is important to standardize the variables to have mean zero and 
# standard deviation one before performing PCA.

# We now perform principal components analysis using the prcomp() function, which is one of several functions in R that perform PCA.
# By default, the prcomp() function centers the variables to have mean zero. By using the option scale=TRUE, 
# we scale the variables to have standard deviation one.
# The output from prcomp() contains a number of useful quantities.
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

# The center and scale components correspond to the means and standard deviations of the 
# variables that were used for scaling prior to implementing PCA.
pr.out$center
pr.out$scale

# The rotation matrix provides the principal component loadings; each column of pr.out$rotation contains 
# the corresponding principal component loading vector
# We see that there are four distinct principal components.
pr.out$rotation

# Using the prcomp() function, we do not need to explicitly multiply the data by 
# the principal component loading vectors in order to obtain the principal component score vectors.
# Rather the 50 × 4 matrix x has as its columns the principal component score vectors. 
# That is, the kth column is the kth principal component score vector.
dim(pr.out$x)

# We can plot the first two principal components as follows:
biplot(pr.out, scale=0)
# The scale=0 argument to biplot() ensures that the arrows are scaled to represent the loadings;
# other values for scale give slightly different biplots with different interpretations.

# The prcomp() function also outputs the standard deviation of each principal component. 
# For instance, on the USArrests data set, we can access these standard deviations as follows:
pr.out$sdev

# The variance explained by each principal component is obtained by squaring these:
pr.var = pr.out$sdev^2
pr.var

# To compute the proportion of variance explained by each principal component, we simply
# divide the variance explained by each principal component by the total variance explained
# by all four principal components.
pve = pr.var/sum(pr.var)
pve
# We cab see the first principal component explains 62.0% of the variance in the data.
# The next principal component explains 24.74% of the variance, and so forth...

#PCA with IRIS
data("iris")
head(iris)
# creating another data frame from iris that contains the columns from 1 to 4 
irisdata1 <- iris[,1:4]
irisdata1
help("princomp")
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
# cor = a logical value indicating whether the calculation should 
#use the correlation matrix or the covariance matrix.
# (The correlation matrix can only be used if there are no constant variables.)
# score = a logical value indicating whether the score on  
# each principal component should be calculated.
summary(principal_components)
# in the summary you can see that it has four Principal Components it is because the input data has
# four different features.
plot(principal_components)
plot(principal_components, type = "l")
help("biplot")
biplot(principal_components)

# Lab 7 PCA

# PCA on Wine dataset from UCI
# Read the data using the read.table()
# Read the documentation for the UCI wine dataset, in the documentation, 
# Cvs stands for the "cultivars" (varieties) of the class of the wine,
# cultivar are similar to wine classes Pinot Noir,Shiraz,Muscat
# Goal is to identify the membership of the wine in 1 of 3 cultivars.
# There are 13 variables in the dataset such as Alcohol, Malic Acid, Ash, Alkalinity of Ash, Magnesium, ...
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
# Header row is not available in the data, therefore, we need to add the variable names 
head(wine_data)
nrow(wine_data) # there are 178 rows
colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data)
# Using the Heatmap() function, we can check the correlations,
# In the heatmap(), the "Dark Colors" represent the "Correlated"
# In the heatmap(), the "Light Colors" represent the "Not Correlated"
help("heatmap") # Read the heatmap() function Documentation in RStudio.
# Now we will use the heatmap() function to show the correlation among variables.
heatmap(cor(wine_data),Rowv = NA, Colv = NA) 

# Our goal is to identify the 3 variate based on the chemical data on the wine dataset. 
# In order to make it easy identify the 3 cultivars, we will declare 3 classes that represents 
# each cultivar (Cv1,Cv2,Cv3) by using the factor() function in R 
help(factor)
# declaring the cultivar_classes using the factor() function each cultivar Cv1,Cv2 and Cv3.
cultivar_classes <- factor(wine_data$Cvs) 
cultivar_classes
# We will normalize the wine data to a common scale using scale() function so that the PCA process will not 
# overweight variables that happen to have the larger values.
help(scale)
# We will not normalize the Cvs variable (first colume) so we exclude the Cvs column with with -1 
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)
# We can choose to have 8 variables from the total of 13 (choosing 8 out of 13) with only about 8% of loss of cumulative contribution value.

