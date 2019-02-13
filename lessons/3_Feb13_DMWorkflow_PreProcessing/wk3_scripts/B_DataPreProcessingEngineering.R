#' Author: Ted Kwartler
#' Date: 9-9-2018
#' Purpose: Fundraising PreProcessing

# Setwd
setwd("/cloud/project/lessons/3_Feb13_DMWorkflow_PreProcessing/finalWk3/wk3_data")

# Libs
library(vtreat)
library(dplyr)

# Read in the data
donors <- read.csv('fakeDonorBureau_v2.csv')

# Examine; Here you would perform EDA
summary(     )

# Get the names
names(donors)
levels(donors$Y1_Donation)
head(donors$Y2_DonatedAmt)

# Declare the variable CATEGORICAL treatment inputs
informativeVars <- names(donors)[3:19]
y1Target        <- names(donors)[20] #not using y2 yet
y1Success       <- 'Yes'

# Automated variable processing
# for **categorical** outcomes
# i. e. will the prospective donor give Y/N
# DATA, NAMES OF INFORMATIVE VARS, TARGET VAR, SUCCESS CLASS
plan <- designTreatmentsC(donors,
                          informativeVars,
                          y1Target,
                          y1Success)

# Apply the plan
treatedData <- prepare(plan, donors)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(treatedData)

# Start over
rm(list=ls())

# Data
donors <- read.csv('fakeDonorBureau_v2.csv')

# Declare the variable NUMERIC treatment inputs
informativeVars <- names(donors)[3:19]
y1Target        <- names(donors)[21]

# for **numeric** outcomes
# how much will the prospective donor give?
# DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR
plan <- designTreatmentsN(donors,
                          informativeVars,
                          y1Target)

# Apply the plan
treatedData <- prepare(plan, donors)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(treatedData)

# Start over
rm(list=ls())

# Data
donors <- read.csv('fakeDonorBureau_v2.csv')

# Fictitious Data Enrichment
thirdPartyData <- data.frame(uniqueID = donors$uniqueID,
                             likesDogs = rep(c('Yes','No'),
                                             nrow(donors)%/%2),
                             creditScore = rnorm(nrow(donors),650))

# Append more records than in original data to make more realistic
newDat <- data.frame(uniqueID = c('Ted','Nora'),
                     likesDogs = c('Yes','Yes'),
                     creditScore = c(742, 783))

thirdPartyData <- rbind(newDat, thirdPartyData)

# Reorder to make more realistic
idx <- sample(1:nrow(thirdPartyData), nrow(thirdPartyData))
thirdPartyData <- thirdPartyData[idx, ]

# Examine
head(thirdPartyData)

# Perform a join to the existing data
# Bring new data to the 3120 donors; no "Ted" and "Nora"
leftData <- left_join(donors, thirdPartyData, by = 'uniqueID')

# Bring donors to the 3122 new data points
rightData <- right_join(donors, thirdPartyData, by = 'uniqueID')
rightData[c(580:581),] #NA automatically filled in

# What's in common?
innerData <- inner_join(donors, thirdPartyData, by = 'uniqueID') #here identical to leftData

# Declare the variable CATEGORICAL treatment inputs
informativeVars <- names(leftData)[c(4:19, 22:23)]
y1Target        <- names(donors)[20] #performing categorical analysis
y1Success       <- 'Yes'

## A taste of whats to come...
plan <- designTreatmentsC(leftData,
                          informativeVars,
                          y1Target,
                          y1Success)
treatedLeftData <- prepare(plan, leftData)

###### TIME FOR YOUR FIRST MODEL; If you want you can howl at the moon in joy!
#'        _
#'       / \      _-'/
#'    __/|  \-''- _ /
#'__-' { |          \
#'    /             \
#'    /       "o.  |o }
#'    |            \ ;
#'                  ',
#'      \_         __\
#'        ''-_    \.//
#'           / '-____'
#'          /
#'        _'
#'      _-'
#'                      

fit <- glm(Y1_Donation ~ ., treatedLeftData, family='binomial')

# Our first model!
summary(fit)

# Make some real predictions
donationProbability <- predict(fit, type='response')

head(donationProbability)

# End
