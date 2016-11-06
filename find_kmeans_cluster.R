library(ggplot2)

# Create a function to help evaluate different K-Means cluster models

mk_cluster <- function(data,y_var,x_var){
  # Subset the data based on the variables entered into the function
  mydata <- data[,c(y_var,x_var)]
  
  # Determine number of clusters
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                       centers=i)$withinss)
  # Use this graph to look for the optimal number of nodes
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares") 
  
  # Get input from the user on how many nodes to use
  k <- readline(prompt="How many clusters should the model use?: ")
  
  #Create the K-Mean model
  k_model <- kmeans(mydata,k)
  
  #Determine the mean of each data point based on their respective cluster assignment
  aggregate(mydata,by=list(k_model$cluster),FUN=mean)
  # append cluster assignment
  mydata <- data.frame(mydata, k_model$cluster) 
  
  #Plot the resulting data assignment
  ggplot(mydata, aes(mydata[y_var], mydata[x_var], color = k_model.cluster)) + geom_point()
}