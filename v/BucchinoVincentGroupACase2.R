# setup libs, wd, dfs ----------------------------------------------------
  setup <- function(){
    environment(setup) <- environment()
    
    #libs 
    library(reshape2)
    library(ggplot2)
    library(fastDummies)
    library(dplyr)
    library(tidyverse)
     
    setwd("/cloud/project/cases/National City Bank")
    prospects_source_df <<- read.csv('ProspectiveCustomers.csv') # prospects
    
    setwd("/cloud/project/cases/National City Bank/training") 
    marketingresults_df <<- read.csv('CurrentCustomerMktgResults.csv') # marketing results
    
    axiom_df <<- read.csv('householdAxiomData.csv') # axiom
    credit_df <<- read.csv('householdCreditData.csv') # credit
    vehicle_df <<- read.csv('householdVehicleData.csv') # vehicle
    training_df <<- Reduce(merge, list(marketingresults_df,credit_df, axiom_df, vehicle_df))  # merge marketingresults
    prospects_df <<- Reduce(merge, list(prospects_source_df,credit_df, axiom_df, vehicle_df)) # merge prospects
    remove(list=c("prospects_source_df", "marketingresults_df", "credit_df", "axiom_df", "vehicle_df"), envir = .GlobalEnv)
    }
  setup()

# EDA df overview ---------------------------------------------------------

names(training_df)
names(prospects_df)

head(training_df, n=3)
head(prospects_df, n=3)

str(training_df)
str(prospects_df)

summary(training_df) 
summary(prospects_df)

# Frequency of previous outcomes
ggplot(data = training_df) +
  geom_bar(mapping = aes(x = Outcome))

# Offer performance: 60% overall conversion rate
ftable(training_df$Y_AccetpedOffer)
ggplot(data = training_df) +
  geom_histogram(mapping = aes(x = Y_AccetpedOffer), binwidth = .5)

## Previous outcomes compared to outcome
# Past succes is a good indicator whether they will accept a future offer.
# Past failure shows higher likelihood to fail than succeed, but appears to be more likely to succeed than NA
# Other appears to be 50/50
ggplot(data = training_df, mapping = aes(x = Y_AccetpedOffer, colour = Outcome)) +
  geom_freqpoly(binwidth = .5
  )

##Comparing: Communication method
# Cellular is most common
ggplot(data = training_df) +
  geom_bar(mapping = aes(x = Communication))
# Cellular & Telephone perform similarly. NA performs significantly worse.
ggplot(data = training_df, mapping = aes(x = Y_AccetpedOffer, colour = Communication)) +
  geom_freqpoly(binwidth = .5
  )
training_df$Y_AccetpedOffer





# EDA cormat --------------------------------------------------------------

  plot_cormat <- function(){
      environment(plot_cormat) <- environment()
      
      # create dummy cols
      training_cormat_df <- training_df
    
      training_cormat_df <- dummy_cols(training_df, select_columns = c("Outcome","Communication","Education","AffluencePurchases","Marital"))
    
      # drop cols
      training_cormat_df <- training_cormat_df[, !(colnames(training_cormat_df) %in% c(
        "HHuniqueID", "dataID","LastContactMonth","LastContactDay",
        "CallStart","CallEnd","carMake","carModel","DefaultOnRecord.y","CarLoan.y","RecentBalance.y","HHInsurance.y",
        "Outcome","Communication","Education","AffluencePurchases","Job","Marital","PetsPurchases","headOfhouseholdGender","annualDonations","EstRace"
        )
      )]
    
    # change class of all columns to numeric
    training_cormat_df[] <- lapply( training_cormat_df, as.numeric )
    training_cormat_df <- na.omit(training_cormat_df)
    
    
    
    # the rest is mostly copy pasted code from internet
    cormat <- round(cor(training_cormat_df),2)
    
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
    }
  plot_cormat()




# modeling ----------------------------------------------------------------













# WORKSPACE --------------------------------------------------------------------------

# sample
#set.seed(123)
#training_df <- training_df[sample(1:nrow(training_df), 4000, replace=FALSE),]



########## junk
# histogram
ggplot(merged_df, aes(x=RecentBalance)) + geom_histogram()

# plot
g <- ggplot(merged_df, aes(Y_AccetpedOffer,RecentBalance))
g <- ggplot(merged_df, aes(Outcome,RecentBalance))

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


