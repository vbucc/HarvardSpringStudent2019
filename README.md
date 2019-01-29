# HarvardSpringStudent2019
For students of Harvard CSCI E-96 Spring Session 2019

## Please install the following packages with this R code.

- This is the 1st semester we will be using [https://rstudio.cloud](https://rstudio.cloud).  We may encounter some challenges along the way but this will likely mitigate technical problems related to individual systems.

- Please follow the setup instructions in the lecture 1 powerpoint located in [https://github.com/kwartler/HarvardSpringStudent2019](https://github.com/kwartler/HarvardSpringStudent2019) to set up a cloud space for programming.

- You *can* install R, R-Studio & Git locally on your laptop but any system issues will be yours to resolve.  However, your teaching staff will attempt to help.

- If you encounter any errors during set up don't worry!  PLease request technical help from the TFs or Prof K.  The `qdap` library is usually the trickiest because it requires Java and `rJava`.  So if you get any errors, try removing that from the code below and rerunning.  This will take **a long time** if you don't already have the packages, so please run prior to class, and at a time you don't need your computer ie *at night*.

```
# Easiest Method to run in your console
install.packages('pacman')
pacman::p_load(ggplot2, ggthemes, rbokeh, maps,
               ggmap, leaflet, radiant.data, DataExplorer,
               vtreat, dplyr, ModelMetrics, pROC,
               MLmetrics, caret, e1071, plyr,
               rpart.plot, randomForest, dygraphs,
               lubridate, jsonlite, tseries, ggseas,
               arules,fst, recommenderlab,reshape2,
               TTR,quantmod, htmltools,
               PerformanceAnalytics,rpart, data.table,
               pbapply, rbokeh, stringi, tm, qdap,
               dendextend, wordcloud, RColorBrewer,
               tidytext, radarchart, openNLP, xml2, stringr,
               devtools, flexdashboard, rmarkdown, httr)

# Additionally we will need this package from a different repo
install.packages('openNLPmodels.en', repo= 'http://datacube.wu.ac.at/')

# The cloud version had a problem installing forecast with the method above so start a new session
q()

# Answer N for No and click enter.
# Once you have a new session
install.packages('forecast')


# Individually you can use
# install.packages('packageName') such as below:
install.packages('ggplot2')
```
