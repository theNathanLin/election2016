
primary2016 <- read.csv("Data/2016dataprimary/2016primary_results.csv")
str(primary2016)

#Explore NAs
sum(is.na(primary2016))

#Column names 
colnames(primary2016)[colSums(is.na(primary2016)) > 0]
#100 FIPS are missing

rownames(primary2016)[rowSums(is.na(primary2016)) > 0]
View(primary2016)
#New Hampshire doesn't have FIPS codes

#Setting the FIPS codes
primary2016[primary2016$county == "Belknap",]
primary2016[primary2016$county == "Belknap",]$fips <- 33001

primary2016[(primary2016$county == "Carroll"&primary2016$state_abbreviation == "NH"),]$fips <- 33003
primary2016[(primary2016$county == "Cheshire"&primary2016$state_abbreviation == "NH"),]$fips <- 33005
primary2016[(primary2016$county == "Coos"&primary2016$state_abbreviation == "NH"),]$fips <- 33007
primary2016[(primary2016$county == "Grafton"&primary2016$state_abbreviation == "NH"),]$fips <- 33009
primary2016[(primary2016$county == "Hillsborough"&primary2016$state_abbreviation == "NH"),]$fips <- 330011
primary2016[(primary2016$county == "Merrimack"&primary2016$state_abbreviation == "NH"),]$fips <- 330013
primary2016[(primary2016$county == "Rockingham"&primary2016$state_abbreviation == "NH"),]$fips <- 330015
primary2016[(primary2016$county == "Strafford"&primary2016$state_abbreviation == "NH"),]$fips <- 330017
primary2016[(primary2016$county == "Sullivan"&primary2016$state_abbreviation == "NH"),]$fips <- 330019

sum(is.na(primary2016))
#no more NAs!

