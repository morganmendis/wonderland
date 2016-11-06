## Written by Erica L. Dretzka in October 2016
## Recreation of 1998 KDD competition
## Rehearsing Feature & Model Selection 
## Eventually compare versus Python's Yellowbrick

############################################################
## Data:: http://kdd.ics.uci.edu/databases/kddcup98/kddcup98.html
## Data Dictionary:: https://kdd.ics.uci.edu/databases/kddcup98/epsilon_mirror/cup98dic.txt
## http://rstatistics.net/variable-importance-of-predictors-that-contribute-most-significantly-to-a-response-variable-in-r/
## Data transformation:: https://rexplorations.wordpress.com/2015/11/03/johnson-transformation-for-non-normal-data/
## Forward stepwise regression:: http://stackoverflow.com/questions/22913774/forward-stepwise-regression
## Ridge & LASSO:: http://machinelearningmastery.com/penalized-regression-in-r/
## kNN:: 
## Decision Tree:: 
## Artifical Neural Network::
## SVM::
############################################################

pacman::p_load(dplyr, ggplot2, caret, Matrix, mlbench, party, lattice, MASS, Johnson, glmnet, lars)
setwd("C:/Users/579562/Documents/Projects/Hackathons/2016-11 Charity/Learning Dataset")
CharityLearn <- read.csv("cup98LRN.txt", stringsAsFactors = FALSE)

#(i) Derive continuous columns from categorical where possible & meaningful
fn_ConvertCategorical <- function(df){
  #(i.01) Replace NAs
  df[is.na(df)] <- 0
  
  #(i.02) Create lists of column names with particular charcteristics
  colsYN = list("COLLECT1", "VETERANS", "BIBLE", "CATLG", "HOMEE", "PETS", "CDPLAY", "STEREO", "PCOWNERS", "PHOTO", "CRAFTS", "FISHER", "GARDENIN", "BOATS", "WALKER", "KIDSTUFF", "CARDS", "PLATES")
  colsGender = list("CHILD03", "CHILD07", "CHILD12", "CHILD18", "GENDER")
  colsBinary = list("HOMEOWNR", "AGEFLAG")
  
  #(i.03) Loop through YN, Gender & Binary indicator columns, encoding into numerical
  for(col in colsYN){
    df[(df[,col]=="N") | (df[,col]==" "),col] <- 0    #no
    df[df[,col]=="Y",col] <- 1    #yes
  }
  for(col in colsGender){
    df[df[,col]==" ",col] <- 0    #not provided
    df[df[,col]=="M",col] <- .20    #male
    df[df[,col]=="U",col] <- .40    #unknown
    df[df[,col]=="F",col] <- .60    #female
    df[df[,col]=="J",col] <- .80    #joint account, unknown gender
    df[df[,col]=="B",col] <- 1.0    #both (m & f - applicable to children fields)
  }
  for(col in colsBinary){
    df[(df[,col]=="I") | (df[,col]=="U"),col] <- 0    #Inferred from Date of Birth Field (Age) or Unkown (Homeowner)
    df[df[,col]==" ",col] <- 0    #Inferred from Date of Birth Field (Age) or Unkown (Homeowner)
    df[(df[,col]=="E") | (df[,col]=="H"),col] <- 1    #Exact (Age) or Home Owner
  }
  
  #(i.04) Generate new ID columnsfrom DOMAIN, PEPSTRFL & Major Donor Matrix columns
  df[substring(df[,"DOMAIN"], 1, 1) == "X", "Nbrhd_Urban"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "U", "Nbrhd_Urban"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "U", "Nbrhd_Urban"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "C", "Nbrhd_City"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "C", "Nbrhd_City"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "S", "Nbrhd_Suburban"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "S", "Nbrhd_Suburban"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "T", "Nbrhd_Town"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "T", "Nbrhd_Town"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "R", "Nbrhd_Rural"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "R", "Nbrhd_Rural"] <- 0
  df[df[,"PEPSTRFL"] == "X","PEPSTRFL"] <- 1  #has PEP star RFA status
  
  df[, "Nbrhd_SocioEconStts"] <- substring(df[,"DOMAIN"], 2, 2)
  
  #Recency of giving for Major Donor Matrix
  df[substring(df[,"MDMAUD"], 1, 1) == "C", "MajDnrMtx_Crrnt"] <- 1
  df[substring(df[,"MDMAUD"], 1, 1) == "L", "MajDnrMtx_Lpsd"] <- 1
  df[substring(df[,"MDMAUD"], 1, 1) == "I", "MajDnrMtx_Inact"] <- 1
  df[substring(df[,"MDMAUD"], 1, 1) == "D", "MajDnrMtx_Drmnt"] <- 1
  df[is.na(df)] <- 0
  #Frequency of giving for Major Donor Matrix
  df[df[,"MDMAUD"], "MajDnrMtx_Freq"] <- substring(df[,"MDMAUD"], 2, 2)
  
  #Clean up memory
  rm(colsYN, colsBinary, colsGender)
  return(df)
}

#(ii) Identify character & date columns for later processing
fn_DropCharacter <- function(df){
  #(ii.01) FindCreate list of all character columns
  colsChar <- list()
  for(col in colnames(df)){
    if(class(df[,col]) == "character"){
      colsChar <- c(colsChar, col)
    }
  }
  
  #(ii.02) FindCreate list of all date columns
  colsDate <- list()
  for(col in colnames(df)){
    if(class(df[,col]) == "date" | class(df[,col])=="datetime"){
      colsDate <- c(colsDate, col)
    }
  }
  rm(col)
  result <- c(colsChar, colsDate)
  return(result)
}

#(iii) clean data
fn_clean  <- function(df, colsChar) {
  #(iii.01) Convert date to POSIX
  #for(col in colsDate) {
  #  df[, col] <- as.Date(df[, col], "%m/%d/%Y", tz = "")
  #  df[, col][is.na(df[,col])] <- as.Date("1/1/2100", "%m/%d/%Y", tz = "")
  #}
  
  forConversion <- c("MajDnrMtx_Crrnt", "MajDnrMtx_Lpsd", "MajDnrMtx_Inact","MajDnrMtx_Drmnt", "Nbrhd_Urban", "Nbrhd_City", "Nbrhd_Suburban", "Nbrhd_Town", "Nbrhd_Rural", "COLLECT1", "VETERANS", "BIBLE", "CATLG", "HOMEE", "PETS", "CDPLAY", "STEREO", "PCOWNERS", "PHOTO", "CRAFTS", "FISHER", "GARDENIN", "BOATS", "WALKER", "KIDSTUFF", "CARDS", "PLATES", "PEPSTRFL", "GENDER", "AGEFLAG", "HOMEOWNR", "CHILD03", "CHILD07", "CHILD12", "CHILD18", "NOEXCH", "ZIP")
  for(col in forConversion){
    df[, col] <- as.numeric(df[, col])
  }
  
  #(iii.03) Strip character columns in a separate dataframe
  df <- df[, ! names(df) %in% colsChar, drop = F]
  df[is.na(df)] <- 0
  df
}

#(iv) Transform variables
fn_transformVbl <- function(df) {
  for(col in colnames(df)){
    if(length(unique(df[,col])) > 2){
      df[, col] = log(df[, col])
      is.na(df[, col]) <- sapply(df[, col], is.infinite) 
    }
  }
  df[is.na(df)] <- 0
  df
}

#(v) Check for normality 
fn_checkNormality <- function(df, chkNrmlCols) {
  #Shapiro-Wilk test for normality::: ONLY WORKS FOR < 5K SAMPLES
  for(chkNrmlCol in chkNrmlCols){
    qqnorm(CharityLearn_Nrml[CharityLearn_Nrml$IC2 > 0, "IC2"])    #Normal QQ Plot. Non-normal distributions have extreme values in the data  
  }
}

#(vi) Data Exploration Techniques
fn_explore <- function(df, colsScatter){
  pairs(~AGE+NUMCHLD+INCOME+WEALTH1+TARGET_D,data=CharityLearn_Nrml)  
}

#(A) Clean dataframe
CharityLearn <- fn_ConvertCategorical(CharityLearn)
CharacterCols <- fn_DropCharacter(CharityLearn)
CharityLearn <- fn_clean(CharityLearn, CharacterCols)

#(B) Get # of Std Deviations 
zscr <- scale(CharityLearn, center = TRUE, scale = TRUE)
CharityLearn_Nrml <- CharityLearn
CharityLearn_Nrml <- fn_transformVbl(CharityLearn_Nrml)

rm(zscr, fn_clean, fn_DropCharacter, fn_ConvertCategorical, CharityLearn, CharacterCols)

#(C) Run data through different models to find best-fitting model to predict one of our target variables
vblsDependent <- c("TARGET_B", "TARGET_D")
Xs <- as.matrix(CharityLearn_Nrml)

#(C.01) Forward stepwise with LM model
#WARNING: this takes days to run.
#for(vblDependent in vblsDependent){
#  min.model = lm(TARGET_D ~ 1, data=CharityLearn_Nrml)
#  biggest <- formula(lm(TARGET_D ~ .,CharityLearn_Nrml))
  #now, step through the variables to determine which combo best predicts
  #iteratively adding variables until the scope 
#  fwd.model = step(min.model, direction='forward', scope=biggest)
#}

#(C.02) Ridge Regression with glmnet
#penalized with L2-norm (sum of squared coefficients), thus shrinking coeff values, making those w/ minor effect~ 0
for(vblDependent in vblsDependent) {
  Ys <- as.matrix(CharityLearn_Nrml[, vblDependent])
  fit <- glmnet(Xs, Ys, family="gaussian", alpha=0, lambda=0.001)    #fit model
  predictions = predict(fit, Xs, type = "link")         #make predictions
  rmseRidge <- mean((Ys - predictions)^2)               #summarize accuracy
  print(vblDependent + str(rmseRidge))
}

#(C.03) LASSO (Least Absolute Shrinkage and Selection Operator) with glmnet
#penalized with L1-norm (sum of absolute coefficients), thus shrinking coeff values, making those w/ minor effect~ 0
for(vblDependent in vblsDependent) {
  Ys <- as.matrix(CharityLearn_Nrml[, vblDependent])
  fit <- lars(Xs, Ys, type="lasso")             #fit model
  best_step <- fit$df[which.min(fit$RSS)]       #identify best fitting (i.e., lowest RSS)
  predictions <- predict(fit, Xs, s=best_step, type="fit")$fit        #make predictions
  rmseLASSO <- mean((Ys - predictions)^2)                          #summarize accuracy
  print(vblDependent + str(rmseRidge))
}

#(C.04) ElasticNet with glmnet
for(vblDependent in vblsDependent) {
  Ys <- as.matrix(CharityLearn_Nrml[, vblDependent])
  fit <- glmnet(Xs, Ys, family="gaussian", alpha=0.5, lambda=0.001)    #fit model
  predictions = predict(fit, Xs, type = "link")             #make predictions
  rmseElasticNet <- mean((Ys - predictions)^2)              #summarize accuracy
}

#(C.02) Random Forest
cf1 <- cforest(TARGET_D ~ . , data= CharityLearn_Nrml, control=cforest_unbiased(mtry=2,ntree=50)) # fit the random forest

#(C.03) k-Nearest Neighbors

#(C.04) Support Vector Machine
