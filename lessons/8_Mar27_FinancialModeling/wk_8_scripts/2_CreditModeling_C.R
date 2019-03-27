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
library(e1071)
library(vtreat)

# I/O
df <- read.csv('20K_sampleLoans.csv') 

# Keep the pertinent information; as you explore you can add others or delete
keeps <-c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")

df <- df[,keeps]

## Data Prep
#Make % a numeric
df$revol_util <- gsub('%', '', df$revol_util) %>% as.character() %>% as.numeric()
df$int_rate   <- gsub('%', '', df$int_rate) %>% as.character() %>% as.numeric()

# Now easy variable treatment plan
dataPlan <-designTreatmentsC(dframe = df, 
                              varlist = keeps,
                              outcomename = 'y', 
                              outcometarget = 1)

# Now apply the plan to the data
treatedDF <- prepare(dataPlan, df)

ncol(df)
ncol(treatedDF)

names(df)
names(treatedDF)

# Partitioning w/o CV
set.seed(1234)
num <- (nrow(treatedDF) %/% 10) * 8
idx <- sample(1:nrow(treatedDF), num)

# Train, Validation 
trainDF      <- treatedDF[idx,]
validationDF <- treatedDF[-idx,]

# Now let's do a logistic regression with a 3 fold CV
crtl <- trainControl(method = "cv", 
                     number = 3, # usually use 10
                     verboseIter = TRUE)

# Fit lm model using 10-fold CV: model
fit3 <- train(as.factor(y) ~ ., 
               data = trainDF,
               method="glm", family="binomial",
               trControl = crtl)
fit3
preds <- predict(fit3)
table(preds, trainDF$y)

# what happens when we put in original data?
newPreds <- predict(fit3, df)

# So let's treat our data
validationData        <- df[-idx, ]
validationDataTreated <- prepare(dataPlan, validationData)

ncol(validationData)
ncol(validationDataTreated)

newPreds <- predict(fit3, validationDataTreated )

table(newPreds, validationDataTreated$y)

# Save the treatment plan
pth<-'treatmentPlan_fit3.rds'
saveRDS(dataPlan, pth)

# Save the model
# In R GLM model objects are huge, saving a lot of extra info.  This gets rid of a lot of it but retains the ability to make predictions.
trimTrain <- function(object, ...) {
  removals <- c("results", "pred", "bestTune", "call", "dots",
                "metric", "trainingData", "resample", "resampledCM",
                "perfNames", "maxmimize", "times")
  for(i in removals)
    if(i %in% names(object)) object[i] <- NULL
    c_removals <- c('method', 'number', 'repeats', 'p', 'initialWindow', 
                    'horizon', 'fixedWindow', 'verboseIter', 'returnData', 
                    'returnResamp', 'savePredictions', 'summaryFunction', 
                    'selectionFunction', 'index', 'indexOut', 'indexFinal',
                    'timingSamps', 'trim', 'yLimits')
    for(i in c_removals)
      if(i %in% names(object$control)) object$control[i] <- NULL  
    if(!is.null(object$modelInfo$trim))
      object$finalModel <- object$modelInfo$trim(object$finalModel)
    object
}

fit3 <- trimTrain(fit3)

pth<-'LogRegCV_fit3.rds'
saveRDS(fit3, pth)

# End