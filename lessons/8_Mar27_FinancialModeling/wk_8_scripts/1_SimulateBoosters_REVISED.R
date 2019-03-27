#' Author: Ted Kwartler
#' Date:3-27-2019
#' Purpose: Modeling Risk in MTG

# Libs
library('RCurl')
library('XML')
library('triangle')
library('dplyr')

# Load custom functions
source('/cloud/project/lessons/8_Mar27_FinancialModeling/wk_8_customFunctions/crackPack.R')
source('/cloud/project/lessons/8_Mar27_FinancialModeling/wk_8_customFunctions/cardValues.R')
source('/cloud/project/lessons/8_Mar27_FinancialModeling/wk_8_customFunctions/openBox.R')

# Setwd
setwd("/cloud/project/lessons/8_Mar27_FinancialModeling/wk_8_data")

# If that fails, copy/paste from th TCG site and save a CSV version.
# Data Integrity! High for  Genesis Wave was corrected in CSV
guildsOfRavnica <- read.csv("Guilds_10_16_18.csv" , fileEncoding="latin1")

# Examine
head(guildsOfRavnica)

# Single pack simulation; packsPerFoil is usually 6 depends on set
crackPack(guildsOfRavnica, packsPerFoil = 6)

# Get another pack
onePack <- crackPack(guildsOfRavnica,packsPerFoil = 6)

# Simulate market valuation at the pack level
cardValues(onePack, worthlessCommons = T, verbose = F)

# Simulate market valuation at the card level
cardVals <- cardValues(onePack, worthlessCommons = T, verbose = T)
cardVals
sum(cardVals$TCGdistPrice)

# Let's open a box of booster packs; usually 36 but not always
simBox <- openBox(guildsOfRavnica, 
                  numPacks       = 36, 
                  packsPerMythic = 8, 
                  packsPerFoil   = 6,
                  foilsInSet     = T)

# What is the expected return for a complete booster box?
(boxReturn <- cardValues(simBox))

# What about opening 100 boxes (3600 packs)
boxSim <-list()
for (i in 1:100){
  boxes <- openBox(guildsOfRavnica, 
                   numPacks       = 36, 
                   packsPerMythic = 8, 
                   packsPerFoil   = 6, 
                   foilsInSet     = T)
  boxes <- cardValues(boxes, verbose = T)
  nam <- i
  boxes$boxNum <-i
  print(paste('opening box',i))
  boxSim[[nam]] <- boxes
}

# Organize the verbose outcome
boxSim <- do.call(rbind, boxSim)

# Load to save time.
#saveRDS(boxSim, 'boxSim.rds')
boxSim <- readRDS('boxSim.rds')

# See what verbose=T does
boxSim[1:10,]

# Sum individual box return
indBoxes <- aggregate(boxSim$TCGdistPrice, 
                      by = list(boxSim$boxNum), 
                      FUN = sum)
head(indBoxes)

# Get the average return
boxAVG <- mean(indBoxes[,2])

# Plot and Review 
boxPrice <- 93.00
hist(unlist(indBoxes[,2]), main='Guilds of Ravnica')
abline(v=boxPrice,col="red")
text(boxPrice,10,'cost', col='red', pos=1,srt=90, cex=1)
abline(v=boxAVG,col="blue")
text(boxAVG,10,'AvgReturn',col='blue', pos=1,srt=90, cex=1)

# number of boxes below cost
length(subset(indBoxes[,2],indBoxes[,2]<=boxPrice))

# End