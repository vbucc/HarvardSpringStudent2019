#' Author: Ted Kwartler
#' Date: 6-18-2018
#' Purpose: Lending Club Modeling, Cross Validation, Choose Algo
#' 

# WD
setwd("/cloud/project/lessons/8_Mar27_FinancialModeling/wk_8_data")

# Libraries
library(rpart)
library(randomForest)
library(dplyr)
library(caret)

# I/O
df <- read.csv('20K_sampleLoans.csv') 

# Keep the pertinent information; as you explore you can add others or delete
keeps <-c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")

df <- df[,keeps]

## Data Prep
#Make % a numeric
df$revol_util <- gsub('%', '', df$revol_util) %>% as.character() %>% as.numeric()
df$int_rate   <- gsub('%', '', df$int_rate) %>% as.character() %>% as.numeric()

# As you explore, you may want to perform more intersting engineering e.g. 
# days since earliest_cr_line which is Sys.Date() - df$earliest_cr_line 


# Baseline Accuracy
table(df$y)
  
# Partitioning w/o CV
set.seed(1234)
num <- (nrow(df) %/% 10) * 8
idx <- sample(1:nrow(df), num)

# Train, Validation 
trainDF      <- df[idx,]
validationDF <- df[-idx,]

## Tree Model
fit1  <- rpart(as.factor(y)~., data=trainDF)
pred1 <- predict(fit1, trainDF, type='class')

# Quick Eval
table(pred1, trainDF$y)

# Top Vars
fit1$variable.importance

# Save the model
pth<-'oneTreeFit1.rds'
saveRDS(fit1, pth)

# End

