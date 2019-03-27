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

# Partitioning w/o CV
set.seed(1234)
num <- (nrow(df) %/% 10) * 8
idx <- sample(1:nrow(df), num)

# Train, Validation 
trainDF      <- df[idx,]
validationDF <- df[-idx,]

## RandomForest
# RF cant handle factors with more than 53 levels. So let's check.  We could treat as numeric but you would need to remember the mapping (ie factorA = 0, factorB=1 ...)for future predictions.  For simplicity we will just omit if factor levels >53

## Segregate by var type
# Y Var
y <- trainDF$y

# ID the factor variables
(chk <- sapply(df, is.factor))
cats <- trainDF[, chk]
nums <- trainDF[, !c(chk)]

# Check the number of levels for factors
t(t(catLevels <-sapply(cats, nlevels)))

# For now, let's omit
drops <- names(cats)[catLevels>53]
cats[,drops] <-NULL

# Deal with missing
nums[is.na(nums)] <- -1
cats[is.na(cats)] <- 'missing'

# Reassemble 
RFtrainDF <- data.frame(nums, cats, y)
names(RFtrainDF) <-c(names(nums), names(cats), 'y')

# Examine to make sure assembled correctly
head(RFtrainDF)

# Check the warning for fit2A
fit2A <- randomForest(y~., 
                      data = RFtrainDF, 
                      ntree = 1, 
                      mtry=1)

# Now for real with better params
RFtrainDF$y <- as.factor(RFtrainDF$y)
fit2B <- randomForest(y~., 
                      data = RFtrainDF, 
                      ntree = 100, 
                      mtry=2)

varImp(fit2B)
varImpPlot(fit2B,type=2)

################
# What happens if we send untreated original data to the model?
badPreds <- predict(fit2B, validationDF, type='class')
badPreds[1:10]
# Why does this occur?
###############

## In order to get predictions on new data, it must be "treated" to be exactly as expected (the way the variables were when the model was trained).  Here we do on the validation set
RFvalidationDFnums <- validationDF[, names(nums)] # Segregate
RFvalidationDFcats <- validationDF[, names(cats)] # Segregate

# Deal with missing in the same way
RFvalidationDFnums[is.na(RFvalidationDFnums)] <- -1
RFvalidationDFcats[is.na(RFvalidationDFcats)] <- 'missing'

# Reassemble into the "treated" validation set
RFvalidationDF<- data.frame(RFvalidationDFnums, RFvalidationDFcats)

# Make predictions
pred2 <- predict(fit2B, RFvalidationDF, type='class')
table(pred2, validationDF$y)

origPreds <-predict(fit2B)
table(origPreds,trainDF$y)

# Save manual data treatment info
manualTreatment <- list(cats = names(cats), 
                        nums = names(nums),
                        catNAvalue = 'missing',
                        numsNAvalue = -1)

# Save manual treatment info
pth<-'RFmanualTreatment.rds'
saveRDS(manualTreatment, pth)

# Save the model
pth<-'RFfit2.rds'
saveRDS(fit2B, pth)

# End
