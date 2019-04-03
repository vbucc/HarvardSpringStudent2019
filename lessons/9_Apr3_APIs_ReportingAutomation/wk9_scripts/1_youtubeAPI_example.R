#' Author: Ted Kwartler
#' Date: 4-3-2019
#' Purpose: Demonstrate getting XML API information

# Libraries
library(xml2)
library(stringr)
library(rvest)

# WD
setwd("/cloud/project/lessons/9_Apr3_APIs_ReportingAutomation/wk9_data")

# Youtube URL
# https://www.youtube.com/watch?v=NYL-wPVzL64
youtubeCaption <- 'https://www.youtube.com/api/timedtext?asr_langs=it%2Ces%2Cnl%2Cpt%2Cru%2Cfr%2Cko%2Cja%2Cde%2Cen&key=yttt1&v=NYL-wPVzL64&sparams=asr_langs%2Ccaps%2Cv%2Cexpire&caps=asr&hl=en&signature=1E9AD339D8889E9CC551E51104688AD05CDE3C12.E92438D35D48CBA349DEF778DC56AE102F6D4EA7&expire=1554328006&lang=en&name=en&fmt=srv3'

# Go get the data
dat <- read_xml(youtubeCaption)

# Extract text, remove carriage returns, remove special characters
text<-xml_text(dat)
text<-str_replace_all(text, "[\r\n]" , "")
text<-iconv(text, "latin1", "ASCII", sub="")

# Save as a text file
writeLines(text,'someNews.txt')

# Or to organize all of the information, XML "nodes" to list 
xmlData <- as_list(dat)

# Extract the text
text   <- do.call(rbind, xmlData$timedtext$body)
text   <- unlist(text)

# Extract and organize the timing info
timing <- lapply(xmlData$timedtext$body, attributes)
timing <- lapply(timing, unlist)
timing <- do.call(rbind, timing)
timing <- apply(timing, 2,as.numeric)

# Organize into DF
df <- data.frame(timing, text)

# Improve names
names(df)[1:2] <- c('startTimeOnScreen','Duration')

# Examine ot make sure format is ok
head(df, 10)

write.csv(df, 'timedText.csv', row.names = F)

# End