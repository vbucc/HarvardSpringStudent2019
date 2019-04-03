#' Author: Ted Kwartler
#' Date: 11-11-18
#' Purpose: Examining the officer library

# libraries
library(officer)

# Save Path
setwd("/cloud/project/lessons/9_Apr3_APIs_ReportingAutomation/wk9_data")

# Create an empty powerpoint
pptx <- read_pptx()

# What type of slides can we make?
layout_summary(pptx)

# Save the theme
theme <- 'Office Theme'

# Slide types
layout_summary(pptx)

# What is the layout?
layout_properties ( x = pptx, 
                    layout = "Two Content", 
                    master = theme) 


# Add another slide
pptx <- add_slide(pptx, 
                  layout = "Comparison", 
                  master = theme)
# Add the title
pptx <- ph_with_text(pptx, 
                     type = "title", 
                     str = 'Ted Slide Title' )

# Right side title
pptx <- ph_with_text(pptx, 
                     type  = "body", 
                     str   = 'section_v1 this is a section',
                     index = 1)

# Right side content
pptx <- ph_with_text(pptx, 
                     type  = "body", 
                     str   = 'section_v2 another section',
                     index = 2)

# Left side content
pptx <- ph_with_text(pptx, 
                     type  = "body", 
                     str   = 'section_v3',
                     index = 3)
# Left side title
pptx <- ph_with_text(pptx, 
                     type  = "body", 
                     str   = 'section_v4',
                     index = 4)


fileName <- paste0(Sys.Date(),'_simpleExample.pptx')
print(pptx, target = fileName)
