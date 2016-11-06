library(stats)
source('clean_pci_data.R')

#Perform PCA with prcomp()
train.pca <- prcomp(train_x,
                 center = TRUE,
                 scale. = TRUE)

#Inspect the coeffecient values of the PCA
print(train.pca)

#Create a plot to visualize the results of the PCA 
plot(train.pca, type = "l")

#What are the descriptive statistics of each Principal Component
cum_prop <- summary(train.pca)

#variances of the components
pc.variance <- train.pca$sdev^2
plot(pc.variance,type = 'l', xlab = 'Principal Component', ylab = 'Variance')

#explained variance
variance.explained <- train.pca$sdev^2 / sum(train.pca$sdev^2)
plot(variance.explained,type = 'l', xlab = 'Principal Component', ylab = 'Variance Explained')
