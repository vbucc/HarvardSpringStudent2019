#' Author: Ted Kwartler
#' Data: 11-11-18
#' Purpose: Working with new file types

# Libraries
library(readxl)

# wd
setwd("/cloud/project/lessons/9_Apr3_APIs_ReportingAutomation/wk9_data")

# File Path
filePath <- 'exampleExcel.xlsx'

# Identify sheets
numSheets <- excel_sheets(filePath)
numSheets

# Get individual sheets
sheetOne   <- read_excel(filePath, 1)
sheetTwo   <- read_excel(filePath, 2)
sheetThree <- read_excel(filePath, 3)


# End