pacman::p_load(dplyr, glmnet, lars, leaps, faraway, class)
source('clean_pci_data.R')

CharityLearn = read.csv("data/cup98LRN.txt",header = TRUE)
CharityLearn = fn_ConvertCategorical(CharityLearn) %>% fn_correctNumerics(CharityLearn) %>% fn_DropCharacter(CharityLearn)

vblsDependent <- c("TARGET_B", "TARGET_D")
Xs <- as.matrix(CharityLearn)

# Package: glmnet
# Fit by calling the glmnet function with ??=0.5 (When alpha equals 1 you fit a lasso model; when alpha is 0 you get a ridge model)
for(vblDependent in vblsDependent) {
  #Ys <- as.matrix(CharityLearn[, vblDependent])
  fit.elasticnet=glmnet(Xs, Ys, alpha=0.5)
  
  predictionsElasticNet = predict(fit.elasticnet, Xs, type = "link")             #make predictions
  rmseElasticNet <- mean((Ys - predictionsElasticNet)^2)              #summarize accuracy
  
  #plot(fit.elasticnet, xvar="lambda",label=TRUE)  #plot
  frcnDevExplained <- deviance(fit.elasticnet)   #get fraction of deviance explained
}
