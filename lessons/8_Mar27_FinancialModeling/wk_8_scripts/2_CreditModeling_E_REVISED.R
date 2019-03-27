#' Author: Ted Kwartler
#' Date: 11-3-2018
#' Purpose: Lending Club Modeling best model, Cross Validation & score new notes
#' 

# Options
options(scipen=999)

# libs
library(caret)
library(e1071)
library(vtreat)
library(dplyr)
library(rbokeh)
library(MLmetrics)

# Data Directory
setwd("/cloud/project/lessons/8_Mar27_FinancialModeling/wk_8_data")

# Training Data
originalNotes <- read.csv("20K_sampleLoans.csv")
newNotes      <- read.csv("OpenNotesJune18_v2.csv")

# Keep the pertinent information; as you explore you can add others or delete
keeps <-c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "y")

originalNotes <- originalNotes[,keeps]

# Change "months" to integer
originalNotes$term <- gsub('months', '', originalNotes$term) %>% 
                           as.character() %>% as.integer()
# Drop Percent symbols and make numeric
originalNotes$revol_util <- gsub('%', '', originalNotes$revol_util) %>% 
                                as.character() %>% as.numeric()
originalNotes$int_rate   <- gsub('%', '', originalNotes$int_rate) %>% 
                                as.character() %>% as.numeric()

# Partition 10% for evaluating 
set.seed(1234)
num          <- (nrow(originalNotes) %/% 10) * 9
idx          <- sample(1:nrow(originalNotes), num)
trainingDF   <- originalNotes[idx, ]
validationDF <- originalNotes[-idx,]

# Make a plan on all the data
dataPlan <-designTreatmentsC(dframe  = trainingDF, 
                             varlist = keeps,
                             outcomename = 'y', 
                             outcometarget = 1)

# Prepare all partitions
treatedTrain <- prepare(dataPlan, trainingDF)
treatedTest  <- prepare(dataPlan, validationDF)
treatedNew   <- prepare(dataPlan, newNotes)

# Model w/best method from our previous exploration, increase CV
crtl <- trainControl(method = "cv", 
                     number = 10,
                     verboseIter = TRUE)

# Fit lm model using 10-fold CV: model
finalFit <- train(as.factor(y) ~ ., 
                  data = treatedTrain, 
                  method="glm", 
                  family="binomial",
                  trControl = crtl)
finalFit

# Consistent Results?
originalProbs <- predict(finalFit, treatedTest, type = 'prob')
cutoffProbs   <- ifelse(originalProbs[,2]>=0.8,1,0) 

table(cutoffProbs, treatedTest$y)
Accuracy(treatedTest$y, cutoffProbs)

# Now on new notes we can actually buy
newProbs <- predict(finalFit, treatedNew, type = 'prob')
head(newProbs)
table(ifelse(newProbs[,2]>=0.8,1,0))

# Organize data
scoredNotes <- data.frame(id           = 1:nrow(treatedNew),
                          risk         = newProbs[,1],
                          successProb  = newProbs[,2],
                          reward       = newNotes$int_rate,
                          LCgrade      = newNotes$grade)

# Sort  by least risky and examine
scoredNotes<-scoredNotes[order(scoredNotes$risk),]
head(scoredNotes, 10)

# Now suppose that we only want "A" grades w/ 98% probability of paying off
bestNotes <- subset(scoredNotes, scoredNotes$LCgrade == "A" & 
                    scoredNotes$risk <= 0.2)

# Compare with test set as subset
testSetComparison <- data.frame(y     = treatedTest$y,
                                grade = validationDF$grade,
                                risk  = originalProbs[,1])
testSetComparison <- subset(testSetComparison, 
                            testSetComparison$grade == "A" & 
                            testSetComparison$risk <= 0.2 )
# How many were 0?
sum(testSetComparison$y==0)
sum(testSetComparison$y==0) / nrow(testSetComparison)

# Make a mkt Plot
mktPlot <- figure(legend_location = "bottom_right") %>% 
                         ly_points(risk, reward, 
                                   data = bestNotes,
                                   color = LCgrade, glyph = LCgrade,
                                   hover = list(id, risk, reward, LCgrade)) 
mktPlot

# Make a CAPM Plot
mktPlot2 <- mktPlot %>% ly_abline(a=7.0, b = 0, type = 2, color = 'red',
                                  legend = "historical SP500 return") %>%
                        ly_abline(a=5.86, b = 0, type = 2, color = 'green', 
                                  legend = "historical 5yr T-bill")
mktPlot2


# End