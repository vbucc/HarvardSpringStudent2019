#' Author: Ted Kwartler
#' Date: 6-18-2018
#' Purpose: Lending Club Modeling, Cross Validation, Choose Algo
#'

# Options
options(scipen=999)

# WD
setwd("/cloud/project/lessons/8_Mar27_FinancialModeling/wk_8_data")

# Libraries
library(rpart)
library(randomForest)
library(dplyr)
library(caret)
library(e1071)
library(vtreat)
library(MLmetrics)

# Data
df <- read.csv('20K_sampleLoans.csv') 

# Keep the pertinent information; as you explore you can add others or delete
keeps <-c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")

df <- df[,keeps]

## Data Prep
#Make % a numeric
df$revol_util <- gsub('%', '', df$revol_util) %>% as.character() %>% as.numeric()
df$int_rate   <- gsub('%', '', df$int_rate) %>% as.character() %>% as.numeric()

# Get IDX
set.seed(1234)
num <- (nrow(df) %/% 10) * 8
idx <- sample(1:nrow(df), num)
validationDF <- df[-idx,]

# Load models & Treatment Plans
oneTree          <- readRDS("oneTreeFit1.rds")

rfFit            <- readRDS("RFfit2.rds")
rfTreamtmentInfo <- readRDS("RFmanualTreatment.rds")

cvGLM            <- readRDS("LogRegCV_fit3.rds")
dataPlan         <- readRDS("treatmentPlan_fit3.rds")

# Make 3 appropriate validation data sets
# Decision Tree no changes
oneTreeValidation <- validationDF 

# RF manual changes
cats              <- rfTreamtmentInfo$cats
cats              <- validationDF[, cats]
nums              <- validationDF[, rfTreamtmentInfo$nums]
nums[is.na(nums)] <- -1
cats[is.na(cats)] <- 'missing'
rfValidation <- data.frame(cats, nums)

# GLM Vtreat changes
treatedvalidationDF <- prepare(dataPlan, validationDF)

# Get some Preds
oneTreePreds <- predict(oneTree, oneTreeValidation, type='class')
rfFitPreds   <- predict(rfFit, rfValidation, type='class')
cvGLMpreds   <- predict(cvGLM, treatedvalidationDF, type='raw')

# Some differences appear
head(data.frame(oneTree = oneTreePreds,
                RF = rfFitPreds,
                CVGLM = cvGLMpreds), 25)

# Get Accuracy
actuals <- validationDF$y
Accuracy(oneTreePreds, actuals)
Accuracy(rfFitPreds, actuals)
Accuracy(cvGLMpreds, actuals)

# Now get Probs
oneTreeProbs <- predict(oneTree, oneTreeValidation, type='prob')
rfFitProbs   <- predict(rfFit, rfValidation, type='prob')
cvGLMprobs   <- predict(cvGLM, treatedvalidationDF, type='prob')

head(oneTreeProbs)
head(rfFitProbs)
head(cvGLMprobs)

# Increase cutoff threshold for use case  
oneTreeSuccess <- ifelse(oneTreeProbs[,2] >= 0.8,1,0)
rfFitSucess    <- ifelse(rfFitProbs[,2] >= 0.8,1,0)
cvGLMSucess    <- ifelse(cvGLMprobs[,2] >= 0.8,1,0)

table(oneTreeSuccess, actuals)
table(rfFitSucess, actuals)
table(cvGLMSucess, actuals)

# So what model is the best??
# End