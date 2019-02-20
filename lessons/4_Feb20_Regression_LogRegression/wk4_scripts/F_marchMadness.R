#' Author: Ted Kwartler
#' Date: 7-6-2018
#' Purpose: Examine single variable logistic regression
#' 

# wd
setwd("/cloud/project/lessons/4_Feb20_Regression_LogRegression/wk4_data")

# Data
bball <- read.csv('ncaa.csv')

#Examine
bball[1:10,1:6]
bball[1:10, 45:51]

# No Partition, can score the 2013 data for a realistic test set
drops     <- c(1,2,47:50)
adminVars <- bball[,drops]
xVars     <- bball[,!(names(bball) %in% names(bball)[drops])]

# Fit a single predictor model
blockFit <- glm(R1.Class.1.win ~ blocks_avg + 0, data = xVars, family ='binomial')
summary(blockFit)

# Score a single record
singleTeam <- data.frame(blocks_avg = bball$blocks_avg[1])
blockProbs <- predict(blockFit,singleTeam, type = "response")

# Extract useful equation variables
blockCoeff <- 0.02961
l <- blockCoeff * singleTeam #represent the logOdds
e <- 2.718 # natural log

# Calculate the normal odds
exp(l) #exponentiate the log odds to get odds; +110 means they are not favored

# Calculate the probability of a win
probability <-e^l / (1 +e^l)
probability 
blockProbs

# Prb to Odds
probability /(1-probability)
exp(l)

# Let's fake some team stats for blocks & review the probability curve
fakeBlock <- data.frame(blocks_avg = seq(from =-200 , to=200, by=.1))
fakePreds <- predict(blockFit, fakeBlock, type = "response")
plot(sort(fakePreds))

# End