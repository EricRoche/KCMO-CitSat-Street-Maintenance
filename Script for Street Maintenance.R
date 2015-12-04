library(ggplot2)
library(dplyr)
library(RSocrata)
library(lubridate)
library(scales)

data = Cit.Sat.All.Years


#Looking at street maintenance by age

p1 <- ggplot(data, aes((Q9a.Maintenance.city.streets), Q29.Age))
p1 + geom_boxplot()

xtab <- xtabs(~Q9a.Maintenance.city.streets + Q29.Age, data=data)
ftable(xtab)
summary(xtab)

prop.table(ftable(xtab), margin = 2)


#Looking at street maintenance by age (within Neighborhood)

xtab <- xtabs(~Q9b.Maint.streets.your.neighborhd + Q29.Age, data=data)
ftable(xtab)
summary(xtab)

prop.table(ftable(xtab), margin = 2)





#Looking at Street Maintenance by Zip Code and comparing to service requests#

#Step 1 - Generate mean satisfaction levels by zip code for neighborhood and cw street satisfaction
#Works great. But need to filter out zip codes with <10 respondents on surveys too. I build it into the code below.

Zips <- data %>%
  group_by(Fiscal.Year, Q32.Home.zip.code) %>%
  summarise(Mean.Satisfaction.Neighborhood.Streets =  mean(Q9b.Maint.streets.your.neighborhd, na.rm=TRUE),
            Mean.Satisfaction.City.Streets = mean(Q9a.Maintenance.city.streets, na.rm=TRUE),
            n = n()) %>%
  filter(Q32.Home.zip.code != 34160 & Q32.Home.zip.code != 99999) %>%
  filter(n > 10)
  


#Step 2 - Generate Annual totals for service requests for each zipcode
Data.311 <- read.socrata("https://data.kcmo.org/311/KCMOPS311-Data/7at3-sxhp")
Data.311$CREATION.DATE <- as.POSIXct(Data.311$CREATION.DATE)    
Data.311$CLOSED.DATE <- as.POSIXct(Data.311$CLOSED.DATE)

#Step 3 - Add fiscal year to 311 data
#generates fiscal year cutoffs
fy.tmp <- seq(as.POSIXct("2000-05-01"), length=25, by="year")
Data.311$FiscalYear <- (2001:2025)[ findInterval(Data.311$CREATION.DATE, fy.tmp)]



  
