library(stats)
library(mice)
library(reshape2)
library(ggplot2)

#Load the data used in the find_kmeans_cluster.R
source('find_kmeans_cluster.R')

#Select only numeric columns
numeric_cols <- sapply(train, is.numeric)
train.num <- train[,numeric_cols]

#Remove constants and values with variance less than 10%
train.num <- train.num[,apply(train.num, 2, var, na.rm=TRUE) > 0.1]
train.num <- train.num[,apply(train.num, 2, var, na.rm=TRUE) != 0]

#Create a function which calculates the missing values ratio
missing.ratio <-function(x){
  r <- sum(is.na(x))/length(x)
  return(r)
}
#Select values which  have more than 60% of their records
train.cln <- train.num[,apply(train.num, 2, missing.ratio) < 0.4]


#What is the pattern regarding missing values in the data
miss_pattern <- md.pattern(train.cln)
miss <- data.frame(miss_pattern)

#We see that we have 38888 records with complete values
#I am comfortable with that many records
train.complete <- train.cln[complete.cases(train.cln),]


#Perform PCA with prcomp()
train.pca <- prcomp(train.complete,
                 center = TRUE,
                 scale. = TRUE)

#Perform PCA with princomp()
train.prin <- princomp(train.complete,
                    cor = TRUE,
                    scores = TRUE)

#Inspect the coeffecient values of the PCA
print(train.pca)

#Create a plot to visualize the results of the PCA 
plot(train.pca, type = "l")

#What are the descriptive statistics of each Principal Component
cum_prop <- summary(train.pca)

#variances of the components
pc.variance <- train.pca$sdev^2
plot(pc.variance,type = 'l', xlab = 'Principal Component', ylab = 'Variance')

#variance explained
variance.explained <- train.pca$sdev^2 / sum(train.pca$sdev^2)
plot(variance.explained,type = 'l', xlab = 'Principal Component', ylab = 'Variance Explained')
