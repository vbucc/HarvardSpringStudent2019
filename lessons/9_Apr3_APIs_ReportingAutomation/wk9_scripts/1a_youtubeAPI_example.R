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
#https://www.youtube.com/watch?v=Q-wRhzWaCac
youtubeCaption <- 'https://www.youtube.com/api/timedtext?sparams=asr_langs%2Ccaps%2Cv%2Cxoaf%2Cxorp%2Cexpire&v=Q-wRhzWaCac&hl=en&caps=asr&expire=1554337358&xoaf=1&signature=6864DD0FD06204A8416497D4A48DD33B8872DCBE.B2CB07574B93F7E20A32308009152EBFCE4262E4&key=yttt1&xorp=True&asr_langs=ko%2Cru%2Cpt%2Ces%2Cja%2Cnl%2Cen%2Cfr%2Cde%2Cit&kind=asr&lang=en&fmt=srv3'

# Go get the data
dat <- read_xml(youtubeCaption)

# Extract text, remove carriage returns, remove special characters
text<-xml_text(dat)
text<-str_replace_all(text, "[\r\n]" , "")
text<-iconv(text, "latin1", "ASCII", sub="")

# Save as a text file
writeLines(text,'someNews.txt')

# Or to organize all of the information, XML "nodes" to list 
pNodes    <- xml_find_all(dat, ".//p")
startTime <- xml_attr(pNodes, 't')
duration  <- xml_attr(pNodes, 'd')
text      <- xml_text(pNodes)

# Organize
textDF <- data.frame(startTime, duration, text)

# Drop line breaks 
textDF<-  textDF[!grepl('\n', textDF$text),]

# Examine to make sure format is ok
head(textDF, 10)

write.csv(textDF, 'timedText2.csv', row.names = F)

# End