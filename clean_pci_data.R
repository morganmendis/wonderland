library(mice)
library(reshape2)

# Read in the Charity Data
train = read.csv("data/cup98LRN.txt",header = TRUE)

#Select only numeric columns
numeric_cols <- sapply(train, is.numeric)
train.num <- train[,numeric_cols]

#Remove constants and values with variance less than 10%
train.num <- train.num[,apply(train.num, 2, var, na.rm=TRUE) > 0.1]
train.num <- train.num[,apply(train.num, 2, var, na.rm=TRUE) != 0]

source(clean_tools.R)

#Select values which  have more than 60% of their records
train.cln <- train.num[,apply(train.num, 2, missing.ratio) < 0.4]

rm(train.num) #Clean environment

#What is the pattern regarding missing values in the data
miss_pattern <- md.pattern(train.cln)
miss <- data.frame(miss_pattern)

#We see that we have 38888 records with complete values
#I am comfortable with that many records
train.complete <- train.cln[complete.cases(train.cln),]

rm(train.cln) #Clean environment

