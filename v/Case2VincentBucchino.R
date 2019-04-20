########## ########## ########## ########## setup
# libs
library(reshape2)
library(ggplot2)
library(fastDummies)
library(dplyr)
library(tidyverse)


# wd
setwd("/cloud/project/cases/National City Bank/training")

# data
# data: prospects
data_prospects <- read.csv('ProspectiveCustomers.csv')
# data: training
data_marketingresults <- read.csv('CurrentCustomerMktgResults.csv')
data_axiom <- read.csv('householdAxiomData.csv')
data_vehicle <- read.csv('householdVehicleData.csv')
data_credit <- read.csv('householdCreditData.csv')

# merge training data
data_merged <- data_marketingresults
data_merged <- merge(data_merged,data_axiom,by="HHuniqueID")
data_merged <- merge(data_merged,data_credit,by="HHuniqueID")
#data_training <- merge(data_training,data_vehicle,by="HHuniqueID")

# training data
data_training <- data_merged

data_training <- dummy_cols(data_training, select_columns = c("Outcome","Communication","Education","AffluencePurchases","Marital"))

# data cleaning
data_training <- data_training[, !(colnames(data_training) %in% c(
                                      "HHuniqueID", "dataID","LastContactMonth","LastContactDay",
                                      "CallStart","CallEnd","DefaultOnRecord.y","CarLoan.y","RecentBalance.y","HHInsurance.y",
                                      "Outcome","Communication","Education","AffluencePurchases","Job","Marital","PetsPurchases","headOfhouseholdGender","annualDonations","EstRace"
                                      )
                                   )]
names(data_training)
head(data_training, n=5)
str(data_training)

# sample data
set.seed(123)
data_sample <- data_training[sample(1:nrow(data_training), 4000, replace=FALSE),]

# preview sample
names(data_sample)
summary (data_sample$Y_AccetpedOffer)

















########## ########## ########## cormat
# change class of all columns to numeric
#data_cormat <- sapply( data_sample, as.numeric )
cormat <- round(cor(data_sample),2)

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


upper_tri <- get_upper_tri(cormat)
upper_tri


melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()



reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))












########## ########## ########## ########## EDA
########## ########## ########## Comparing: Past outcomes
########## ########## Frequency: Privious outcomes
ggplot(data = data_sample) +
  geom_bar(mapping = aes(x = Outcome))
########## ########## Offer performance
########## 60% overall conversion rate
ftable(data_training$Y_AccetpedOffer)
ggplot(data = data_sample) +
  geom_bar(mapping = aes(x = Y_AccetpedOffer), binwidth = .5)
########## ########## Previous outcomes compared to outcome
########## Past succes is a good indicator whether they will accept a future offer.
########## Past failure shows higher likelihood to fail than succeed, but appears to be more likely to succeed than NA
########## Other appears to be 50/50
ggplot(data = data_sample, mapping = aes(x = Y_AccetpedOffer, colour = Outcome)) +
  geom_freqpoly(binwidth = .5
  )
########## ########## ########## Comparing: Communication method
########## ########## Frequency: Communication
########## Cellular is most common
ggplot(data = data_sample) +
  geom_bar(mapping = aes(x = Communication))
########## Cellular & Telephone perform similar. NA performs significantly worse.
ggplot(data = data_sample, mapping = aes(x = Y_AccetpedOffer, colour = Communication)) +
  geom_freqpoly(binwidth = .5
  )



data_country <- data.frame(country = c("China", "Germany", "UK", "US"), 
                           conversion_rate = c(0.001331558,0.062428188, 0.052612025, 0.037800687))
ggplot(data_country, aes(x=country,y = conversion_rate)) +geom_bar(stat = "identity")


















########## ########## ########## ########## workspace
########## ########## ########## junk
# histogram
ggplot(data_merged, aes(x=RecentBalance)) + geom_histogram()

# plot
g <- ggplot(data_merged, aes(Y_AccetpedOffer,RecentBalance))
g <- ggplot(data_merged, aes(Outcome,RecentBalance))

# Scatterplot
g + geom_point() + 
  geom_smooth(method="lm", se=F) + geom_quantile() +
  theme_minimal()
#labs(subtitle="mpg: city vs highway mileage", 
#y="hwy", 
#x="cty", 
#title="Scatterplot with overlapping points", 
#caption="Source: midwest")



#end
