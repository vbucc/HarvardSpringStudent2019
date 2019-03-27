#' Author: Ted Kwartler
#' Date:11-3-2018
#' Purpose: Modeling Risk in MTG
#' 

# Install the boosterbox package
#install.packages('RCurl')
#install.packages('XML')
#install.packages('triangle')
#install.packages("C:/Users/Edward/Desktop/HarvardFallAdmin2018/lessons/8_Nov5_FinancialModeling/BoosterBox_0.0.2.tar.gz", repos = NULL, type = "source")

# Setwd
setwd("C:/Users/Edward/Desktop/HarvardFallAdmin2018/lessons/8_Nov5_FinancialModeling/data")

# Library
library(BoosterBox)

# Example data formatting
#http://magic.tcgplayer.com/db/search_result.asp?Set_Name=Guilds%20of%20Ravnica
data(modernMasters13)
head(modernMasters13)

# If that fails, copy/paste from th TCG site and save a CSV version.
# Data Integrity! High for  Genesis Wave was corrected in CSV
guildsOfRavnica <- read.csv("Guilds_10_16_18.csv" )

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

# Let's open a box of booster packs; 24 for Iconic Masters but usually 36 for other sets
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

# Avg individual box return
indBoxes <- aggregate(boxSim$TCGdistPrice, 
                      by = list(boxSim$boxNum), 
                      FUN = sum)
head(indBoxes)

# Get the average return
boxAVG <- mean(indBoxes[,2])

# Plot and Review 
boxPrice <- 83.00
hist(unlist(indBoxes[,2]), main='Guilds of Ravnica')
abline(v=boxPrice,col="red")
text(boxPrice,10,'cost', col='red', pos=1,srt=90, cex=1)
abline(v=boxAVG,col="blue")
text(boxAVG,10,'AvgReturn',col='blue', pos=1,srt=90, cex=1)

# number of boxes below cost
length(subset(indBoxes[,2],indBoxes[,2]<=boxPrice))

# End

