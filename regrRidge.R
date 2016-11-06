pacman::p_load(dplyr, glmnet, lars, leaps, faraway, class)
source('clean_pci_data.R')

CharityLearn = read.csv("data/cup98LRN.txt",header = TRUE)
CharityLearn = fn_ConvertCategorical(CharityLearn) %>% fn_correctNumerics(CharityLearn) %>% fn_DropCharacter(CharityLearn)

vblsDependent <- c("TARGET_B", "TARGET_D")
Xs <- as.matrix(CharityLearn)

#(A) Run data through different models to find best-fitting model to predict one of our target variables
vblsDependent <- c("TARGET_D") #"TARGET_B", 
Xs <- as.matrix(CharityLearn)

# Package: glmnet with gaussian distribution
for(vblDependent in vblsDependent) {
  Ys <- as.matrix(CharityLearn[, vblDependent])
  fitRidge <- glmnet(Xs, Ys, family="gaussian", alpha=0, lambda=0.001)    #fit model
  predictionsRidge = predict(fitRidge, Xs, type = "link")         #make predictions
  rmseRidge <- mean((Ys - predictionsRidge)^2)               #summarize accuracy
  print(str(vblDependent) + str(rmseRidge))
}

# Package: glmnet
# Fit by calling the glmnet function with ??=0 (When alpha equals 1 you fit a lasso model; when alpha is between 0 and 1 you get elasticnet model)
for(vblDependent in vblsDependent) {
  Ys <- as.matrix(CharityLearn[, vblDependent])
  fit.ridge=glmnet(Xs, Ys, alpha=0)
  #plot(fit.ridge,xvar="lambda",label=TRUE)  #plot
  frcnDevExplained <- deviance(fit.ridge)   #get fraction of deviance explained
}
