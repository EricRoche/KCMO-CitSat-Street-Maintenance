#Potholes

#Step 1 - Generate mean satisfaction levels by zip code for neighborhood and cw street satisfaction
#Works great. But need to filter out zip codes with <10 respondents on surveys too. I build it into the code below.

#Roll the data up by zip and FY and then filter out bad zips and remove anything not containg "Mark" as in marking.
Zips.311 <- Data.311 %>%
  group_by(FiscalYear, ZIP.CODE, REQUEST.TYPE) %>%
  summarise(Number.of.Requests = (n_distinct(CASE.ID))) %>%
  filter(ZIP.CODE != "na") %>%
  filter(grepl("Pot",REQUEST.TYPE))

#The previous step groups them but correctly but creates a row for each different CTD type. This leads to multiple points per year.
#This code chunk rolls up the prefiltered requests by year and zip correctly.

Zips.311 <- Zips.311 %>% 
  group_by(FiscalYear, ZIP.CODE) %>% 
  summarise(Number.of.Requests = sum(Number.of.Requests))

#Merge the df
#Create unique ID for each row
Zips$UniqueID <- paste(Zips$Q32.Home.zip.code, Zips$Fiscal.Year, sep = "-")
Zips.311$UniqueID <- paste(Zips.311$ZIP.CODE, Zips.311$FiscalYear, sep = "-")

#Merge them
Zip.Data <- left_join(Zips, Zips.311, by="UniqueID")
#mark NA's as zero
Zip.Data$Number.of.Requests[is.na(Zip.Data$Number.of.Requests)] <- 0
#drop redundant columns
Zip.Data <- select(Zip.Data, - ZIP.CODE)
Zip.Data <- select(Zip.Data, - FiscalYear)



#Plot it out

p <- ggplot(Zip.Data, aes(Number.of.Requests, Mean.Satisfaction.Neighborhood.Streets)) + geom_smooth(level=0.90)  
p+ geom_point()  +xlab("Number of Requests for Potholes") +ylab("Mean Satisfaction with Street Maintenance in YOUR Neighborhood")+
  ggtitle("CitSat By # of Pothole 311 Requests with a Loess Model")

p <- ggplot(Zip.Data, aes(Number.of.Requests, Mean.Satisfaction.Neighborhood.Streets)) + geom_smooth(method = "lm", level=0.90)  
p+ geom_point()  +xlab("Number of Requests for Potholes") +ylab("Mean Satisfaction with Street Maintenance in YOUR Neighborhood")+
  ggtitle("CitSat By # of Pothole 311 Requests with a Linear Model")

#scale_x_continuous(limits = c(0,8), breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))
