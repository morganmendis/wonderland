library(mice)
library(reshape2)

set.seed(240)

# Read in the Charity Data
raw = read.csv("data/cup98LRN.txt",header = TRUE)


samp <- sample(nrow(raw), 0.6 * nrow(raw))

train <- raw[samp, ]
test <- raw[-samp, ]

train_y <-train[,c("TARGET_B","TARGET_D")]

x_vars <- colnames(train[!names(train) %in% c("TARGET_B","TARGET_D","CONTROLN")])
train_x <- train[,x_vars]
rm(x_vars)

#Select only numeric columns
numeric_cols <- sapply(train_x, is.numeric)
train.num <- train[,numeric_cols]

#Remove constants 
train.num <- train.num[,apply(train.num, 2, var, na.rm=TRUE) != 0]

source('clean_tools.R')

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

