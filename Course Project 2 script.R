#===================Reproducible Research Course Project 2======================

# This script contains the exploratory analysis for the course project
#   The final product is contained in the Rmd file and published to RPubs

#====================Assignment=================================================
#The basic goal of this assignment is to explore the NOAA Storm Database and 
#   answer some basic questions about severe weather events. You must use the 
#   database to answer the questions below and show the code for your entire 
#   analysis. Your analysis can consist of tables, figures, or other summaries. 
#   You may use any R package you want to support your analysis.

#=============================Questions=========================================
#Your data analysis must address the following questions:
    
# 1. Across the United States, which types of events (as indicated in the EVTYPE
#    variable) are most harmful with respect to population health?

# 2. Across the United States, which types of events have the greatest economic 
#    consequences?

#Consider writing your report as if it were to be read by a government or 
#    municipal manager who might be responsible for preparing for severe weather
#    events and will need to prioritize resources for different types of events.
#    However, there is no need to make any specific recommendations in your report.


#============================Read in the Data===================================

# Download the data

    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.csv.bz2")

# Read the data into R

    stormdata<-read.csv("StormData.csv.bz2")

#=========================Initial Exploratory Analysis==========================

    summary(stormdata)
    head(stormdata)
    tail(stormdata)

    hist(stormdata$FATALITIES)
    hist(stormdata$FATALITIES[stormdata$FATALITIES!=0])
    summary(stormdata$FATALITIES[stormdata$FATALITIES!=0])
    table(stormdata$FATALITIES)
    table(stormdata$INJURIES)
    table(stormdata$PROPDMG)
    hist(stormdata$PROPDMG)
    hist(stormdata$PROPDMG[stormdata$PROPDMG>0])
