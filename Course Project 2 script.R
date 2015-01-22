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

# Download the data - first check to see if it's already there

    if("StormData.csv.bz2" %in% dir()){} else {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.csv.bz2")
    }
# Read the data into R

    stormdata<-read.csv("StormData.csv.bz2")
    
# Decrease the memory requirements by eliminating all but the variables we will use

    stormdata<-stormdata[c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG")]
#=========================Initial Exploratory Analysis==========================

    dim(stormdata)
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

    summary(stormdata$EVTYPE)
    str(stormdata$EVTYPE)
    table(stormdata$EVTYPE)
    barplot(table(stormdata$EVTYPE))

#==================================Question 1 ==================================

# 1. Across the United States, which types of events (as indicated in the EVTYPE
#    variable) are most harmful with respect to population health?
    
# Find the total, mean, and meadian number of fatalities for each type of event
# Split the fatalities variable by event type
    attach(stormdata)
    by.event.type<-split(FATALITIES, EVTYPE)
# Apply sum, mean, and median
    total.fatalities.by.type<-sapply(by.event.type, sum)
    mean.fatalities.by.type<-sapply(by.event.type, mean)
    median.fatalities.by.type<-sapply(by.event.type, median)

    rm(by.event.type)   #To conserve memory

#Repeat for injuries
    by.event.type<-split(INJURIES, EVTYPE)
    # Apply sum and median
    total.injuries.by.type<-sapply(by.event.type, sum)
    median.injuries.by.type<-sapply(by.event.type, median)
    
    rm(by.event.type)   
    detach(stormdata)


# Look at the results in histograms
# This was not useful, comment it out
#    par(mfrow=c(3,1))
#    hist(log(total.fatalities.by.type+1))
#    hist(log(mean.fatalities.by.type+1))
#    hist(log(median.fatalities.by.type+1))
#    par(mfrow=c(1,1))
summary(mean.fatalities.by.type)
# Look at the results in tables
    table(total.fatalities.by.type)
    table(mean.fatalities.by.type)
    table(median.fatalities.by.type)

    hist(mean.fatalities.by.type[mean.fatalities.by.type>0])
# Line graph of fatalities by type
    par(mfrow=c(2,1), mar=c(4.3,4.3,2,2))
    plot(total.fatalities.by.type, type="l", xlab="", ylab="Total Fatalities")
#    plot(mean.fatalities.by.type, type="l", col="red")
    plot(median.fatalities.by.type, type="l", xlab="Event Type", ylab="Median Fatalities")
    par(mfrow=c(1,1), mar=(c(5,4,4,2)+.1)

    # Line graph of injuries by type
    par(mfrow=c(2,1), mar=c(4.3,4.3,2,2))
    plot(total.injuries.by.type, type="l", xlab="", ylab="Total Injuries")
    plot(median.injuries.by.type, type="p", xlab="Event Type", ylab="Median Injuries")
    par(mfrow=c(1,1), mar=(c(5,4,4,2)+.1)
        
        
        
        
    # Split the property damage variable by event type
    attach(stormdata)
    by.event.type<-split(PROPDMG, EVTYPE)
    # Apply sum and median
    total.propdamage.by.type<-sapply(by.event.type, sum)
    median.propdamage.by.type<-sapply(by.event.type, median)
    
    rm(by.event.type)   
    
    detach(stormdata)
    
    
    # Line graph of property damage by type
    par(mfrow=c(2,1), mar=c(4.3,4.3,2,2))
    plot(total.propdamage.by.type, type="l", xlab="", ylab="Total Property Damage")
    #    plot(mean.fatalities.by.type, type="l", col="red")
    plot(median.propdamage.by.type, type="l", xlab="Event Type", ylab="Median Property Damage")
    par(mfrow=c(1,1), mar=(c(5,4,4,2)+.1)
        
# Extract the event types with the highest total number of fatalities, 
#    highest mean fatalities and highest median fatalities

    names(total.fatalities.by.type)[total.fatalities.by.type==max(total.fatalities.by.type)]
    names(mean.fatalities.by.type)[mean.fatalities.by.type==max(mean.fatalities.by.type)]
    names(median.fatalities.by.type)[median.fatalities.by.type==max(median.fatalities.by.type)]

# Look at the top five event types for fatalities in each summation category

    head(names(total.fatalities.by.type[order(total.fatalities.by.type, decreasing=T)]), n=5)
    head(names(mean.fatalities.by.type[order(mean.fatalities.by.type, decreasing=T)]), n=5)
    head(names(median.fatalities.by.type[order(median.fatalities.by.type, decreasing=T)]), n=5)


    head(names(total.fatalities.by.type[order(total.fatalities.by.type, decreasing=T)]), n=1)
    head(names(median.fatalities.by.type[order(median.fatalities.by.type, decreasing=T)]), n=3)
    head(names(total.injuries.by.type[order(total.injuries.by.type, decreasing=T)]), n=1)
    head(names(median.injuries.by.type[order(median.injuries.by.type, decreasing=T)]), n=1)

    mean.fatalities.by.type["TORNADO"]

# How many of various events that appear in the top five lists are included in the data?

    event.type.table<-table(stormdata$EVTYPE)
    event.type.table["TORNADO"];event.type.table["EXCESSIVE HEAT"]; event.type.table["FLASH FLOOD"]; event.type.table["HEAT"];event.type.table["LIGHTNING"]

    event.type.table["TORNADOES, TSTM WIND, HAIL"];event.type.table["COLD AND SNOW"];event.type.table["TROPICAL STORM GORDON"];event.type.table["RECORD/EXCESSIVE HEAT"];event.type.table["EXTREME HEAT"]

    event.type.table["HEAT WAVE DROUGHT"];event.type.table["HIGH WIND/SEAS"]

# How many types of events are there that have the word "heat" in the name?

    names(event.type.table)[grep("HEAT", names(event.type.table), ignore.case=T)]
    event.type.table[grep("HEAT", names(event.type.table), ignore.case=T)]
    sum(event.type.table[grep("HEAT", names(event.type.table), ignore.case=T)])

# How many events are there with the word "tornado" in the name?

    names(event.type.table)[grep("TORNADO", names(event.type.table), ignore.case=T)]
    event.type.table[grep("TORNADO", names(event.type.table), ignore.case=T)]
    sum(event.type.table[grep("TORNADO", names(event.type.table), ignore.case=T)])

# Look for other patterns to look for
    names(event.type.table)

# How many events are there with the word "wind" in the name?

    names(event.type.table)[grep("WIND", names(event.type.table), ignore.case=T)]
    event.type.table[grep("WIND", names(event.type.table), ignore.case=T)]
    sum(event.type.table[grep("WIND", names(event.type.table), ignore.case=T)])

# How many events are there with the word "snow" in the name?

    names(event.type.table)[grep("snow", names(event.type.table), ignore.case=T)]
    event.type.table[grep("snow", names(event.type.table), ignore.case=T)]
    sum(event.type.table[grep("snow", names(event.type.table), ignore.case=T)])

# How many events are there with the word "cold" in the name?

    names(event.type.table)[grep("cold", names(event.type.table), ignore.case=T)]
    event.type.table[grep("cold", names(event.type.table), ignore.case=T)]
    sum(event.type.table[grep("cold", names(event.type.table), ignore.case=T)])

# How many events are there with the word "flood" in the name?

    names(event.type.table)[grep("flood", names(event.type.table), ignore.case=T)]
    event.type.table[grep("flood", names(event.type.table), ignore.case=T)]
    sum(event.type.table[grep("flood", names(event.type.table), ignore.case=T)])

# How many events are there with the word "slide" in the name?

    names(event.type.table)[grep("slide", names(event.type.table), ignore.case=T)]
    event.type.table[grep("slide", names(event.type.table), ignore.case=T)]
    sum(event.type.table[grep("slide", names(event.type.table), ignore.case=T)])


# Find what I haven't classified yet
    event.class<-character(length(event.type.table))

    event.class[grep("WIND", names(event.type.table), ignore.case=T)]<-"wind"
        event.class[grep("wnd", names(event.type.table), ignore.case=T)]<-"wind"
        event.class[grep("w ind", names(event.type.table), ignore.case=T)]<-"wind"
    event.class[grep("snow", names(event.type.table), ignore.case=T)]<-"snow"
        event.class[grep("blizzard", names(event.type.table), ignore.case=T)]<-"snow"
        event.class[grep("winter", names(event.type.table), ignore.case=T)]<-"snow"
        event.class[grep("wintry", names(event.type.table), ignore.case=T)]<-"snow"
    event.class[grep("heat", names(event.type.table), ignore.case=T)]<-"heat"
        event.class[grep("hot", names(event.type.table), ignore.case=T)]<-"heat"
    event.class[grep("cold", names(event.type.table), ignore.case=T)]<-"cold"
    event.class[grep("tornado", names(event.type.table), ignore.case=T)]<-"tornado"
        event.class[grep("torndao", names(event.type.table), ignore.case=T)]<-"tornado"
    event.class[grep("flood", names(event.type.table), ignore.case=T)]<-"flood"
    event.class[grep("slide", names(event.type.table), ignore.case=T)]<-"slide"
    event.class[grep("freez", names(event.type.table), ignore.case=T)]<-"ice"
        event.class[grep("frost", names(event.type.table), ignore.case=T)]<-"ice"
        event.class[grep("ice", names(event.type.table), ignore.case=T)]<-"ice"
    event.class[grep("fire", names(event.type.table), ignore.case=T)]<-"fire"
    event.class[grep("volcan", names(event.type.table), ignore.case=T)]<-"volcano"
    event.class[grep("summary", names(event.type.table), ignore.case=T)]<-"summary"
    event.class[grep("lightning", names(event.type.table), ignore.case=T)]<-"lightning"
    event.class[grep("dry", names(event.type.table), ignore.case=T)]<-"dry"
        event.class[grep("drought", names(event.type.table), ignore.case=T)]<-"dry"
    event.class[grep("hail", names(event.type.table), ignore.case=T)]<-"hail"
    event.class[grep("rain", names(event.type.table), ignore.case=T)]<-"rain"
    event.class[grep("precipitation", names(event.type.table), ignore.case=T)]<-"rain"
    event.class[grep("storm", names(event.type.table), ignore.case=T)]<-"storm"
        event.class[grep("tropical", names(event.type.table), ignore.case=T)]<-"storm"


    length(which(event.class==""))
    names(event.type.table[which(event.class=="")])
#STOP!!!  Come up with a better plan


