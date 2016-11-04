pacman::p_load(dplyr, glmnet, lars, leaps, faraway, class)
source('clean_pci_data.R')

CharityLearn = read.csv("data/cup98LRN.txt",header = TRUE)
CharityLearn = fn_ConvertCategorical(CharityLearn) %>% fn_correctNumerics(CharityLearn) %>% fn_DropCharacter(CharityLearn)

vblsDependent <- c("TARGET_B", "TARGET_D")
Xs <- as.matrix(CharityLearn)

# Package: lars
for(vblDependent in vblsDependent) {
  Ys <- as.matrix(CharityLearn[, vblDependent])
  fitLASSO <- lars(Xs, Ys, type="lasso")             #fit model
  print("here")
  best_stepLASSO <- fitLASSO$df[which.min(fitLASSO$RSS)]       #identify best fitting (i.e., lowest RSS)
  predictionsLASSO <- predict(fitLASSO, Xs, s=best_stepLASSO, type="fit")$fit        #make predictions
  rmseLASSO <- mean((Ys - predictionsLASSO)^2)                          #summarize accuracy
  print(str(vblDependent) + str(rmseLASSO))
}

# LASSO Regression with glmnet 
# Fit by calling the glmnet function with ??=1 (When alpha equals 0 you fit a ridge model; when alpha is between 0 and 1 you get elasticnet model)
for(vblDependent in vblsDependent){
  Ys <- as.matrix(CharityLearn[, vblDependent])
  # fit model & visualize lambdas
  fit.lasso=glmnet(Xs,Ys,alpha=1)
  #plot(fit.lasso, xvar="lambda",label=TRUE)
  # visualize deviation explained
  plot(fit.lasso, xvar="dev",label=TRUE) #percentage of deviance explained, equivalant to r squared  
}
  
for(vblDependent in vblsDependent) {
  # Model selection: coefficient function extractor works on a cross validation object, picks the coefficient vector corresponding to the best mode
  cv.lasso = cv.glmnet(Xs,Ys,alpha=0)
  coef(cv.lasso)
  
  # Train/ validation set with CrossValidation.r
  pred = predict(cv.lasso,Xs)
  dim(pred)
  rmse = sqrt(apply((Ys-pred)^2,2,mean))
  lam.best = cv.lasso$lambda[order(rmse)[1]]
  lam.best
  
  print(coef(lasso.tr,s=lam.best))
}
