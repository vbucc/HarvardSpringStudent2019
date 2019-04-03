#' Author: Ted Kwartler
#' Date:4-3-2019
#' Purpose: Getting data from the web pt2

# libraries
library(rvest)

# Stock URL
stockURL <- 'https://finance.yahoo.com/quote/AAPL/financials?p=AAPL'
stockURL <- read_html(stockURL)
stockURL

# Stock Financial Info
annualDF <- stockURL %>% 
  html_table() 

# Column headers
annualDF <- as.data.frame(annualDF)
names(annualDF) <- c('FinancialIndicator',annualDF[1,2],annualDF[1,3],annualDF[1,4],annualDF[1,5])

# End