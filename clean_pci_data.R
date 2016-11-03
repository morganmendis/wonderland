#Adjust settings
options(warn=-1) #Disable warning messages
set.seed(240) #Set seed for reproducibility

library(mice)
library(reshape2)

# Read in the Charity Data
raw = read.csv("data/cup98LRN.txt",header = TRUE)

#Identify target variables, create separate matrix
raw_y <- raw[,c("TARGET_B","TARGET_D")]

#Identify explanatory variables
x_vars <- colnames(raw[!names(raw) %in% c("TARGET_B","TARGET_D","CONTROLN")])

#Create separate Matrix of explanatory variables
raw_x <- raw[,x_vars]

#Select only numeric columns
numeric_cols <- sapply(raw_x, is.numeric)
raw_x_num <- raw_x[,numeric_cols]

#Remove constants 
raw_x_num <- raw_x_num[,apply(raw_x_num, 2, var, na.rm=TRUE) != 0]

#Remove columns with less than 10% variance
raw_x_num <- raw_x_num[,apply(raw_x_num, 2, var, na.rm=TRUE) > 0.1] 

source('clean_tools.R')

#Select features which  have more than 60% of their records
raw_x_rob <- raw_x_num[,apply(raw_x_num, 2, missing.ratio) < 0.4]


#Concatenate target and explanatory variables 
#Assign to a single dataframe
raw_clean <- cbind(raw_y,raw_x_rob)


#What is the pattern regarding missing values in the data?
miss_pattern <- md.pattern(raw_clean)
miss <- data.frame(miss_pattern) 


#We see that we have 33,888 records with complete values
#Seems adequate, subset dataframe to only complete cases
raw_complete <- raw_clean[complete.cases(raw_clean),]

#Partition the data
#60% for training models, 40% for testing models
samp <- sample(nrow(raw_complete), 0.6 * nrow(raw_complete))

train <- raw_complete[samp, ]
test <- raw_complete[-samp, ]

x_vars <- colnames(raw_complete[!names(raw_complete) %in% c("TARGET_B","TARGET_D")])


#Separate target and explanatory variables into separte data frames
train_y <-train[,c("TARGET_B","TARGET_D")]
train_x <- train[,x_vars]

test_y <-test[,c("TARGET_B","TARGET_D")]
test_x <- test[,x_vars]

print("During data cleansing:")
print(paste(ncol(raw)-ncol(raw_complete),"columns, were removed."))
print(paste(nrow(raw)-nrow(raw_complete),"observations, were removed."))
print(paste((ncol(raw_complete)/ncol(raw))*100,"% of features remain."))
print(paste((nrow(raw_complete)/nrow(raw))*100,"% of observations remain."))

rm(raw,raw_complete,samp,x_vars, raw_clean, raw_x_rob, raw_x_num, miss, miss_pattern, numeric_cols) #Clean environment


