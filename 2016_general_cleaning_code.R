#2016 General Election Cleaning Code
#Nathan Lin

#Libraries needed
library(reshape2)
library(tigris)
library(ggplot2)
library(leaflet)
library(maps)
library(scales)

setwd("~/Second Year/DS 4559 - Data Science/Final Project/election2016/Data")

#Read the 2016 general election data + FIPS code database
gen16 <- read.csv("pres16results.csv", header = TRUE, stringsAsFactors = FALSE)
fips.labels <- read.csv("fips_database.csv", header = FALSE, stringsAsFactors = FALSE, colClasses = "character")

#Cleaning general election data
gen16 <- gen16[which(gen16$cand == "Donald Trump" | gen16$cand == "Hillary Clinton"),]
gen16 <- gen16[-c(1,2),]

sapply(gen16,class)

#This county wasn't labeled in the data, so we manually assigned it the county name
for (i in seq(nrow(gen16))){
  if (gen16$fips[i] == "46102") gen16$county[i] <- "Oglala Lakota County"
}

#Creating a separate table with just the state-wide results
gen16.states <- gen16[is.na(gen16$county),]
gen16.states <- gen16.states[-c(103,104),] #removing state FIPS for Alaska
gen16.states$county <- NULL
gen16.states$fips <- NULL

gen16 <- gen16[-is.na(gen16$county)]

#Removing the statewide results from the county data
for (i in seq(nrow(gen16))) {
  if (nchar(gen16$fips[i])<=2) gen16$fips[i] <- NA
}

gen16 <- gen16[-which(is.na(gen16$fips)),]
gen16$pct_report <- NULL
gen16.2 <- gen16

gen16.2$votes <- NULL
gen16.2$total_votes <- NULL
gen16.2$lead <- NULL

#Used the reshape2 package to convert the dataframe from long to wide format
long16 <- dcast(gen16.2, fips + st ~ cand, value.var = "pct")
colnames(long16) <- c("fips", "st", "DonaldTrump", "HillaryClinton")
long16$diff <- long16$DonaldTrump - long16$HillaryClinton

#Added an extra leading 0 for FIPS codes that were 4 digits long (enables the merge later)
for (i in seq(nrow(long16))) {
  if (nchar(long16$fips[i])==4) long16$fips[i] <- paste("0",long16$fips[i], sep="")
}

#Cleaning FIPS database
fips.labels$fips <- paste(fips.labels$V2,fips.labels$V3, sep = "")
fips.labels$V1 <- NULL
fips.labels$V2 <- NULL
fips.labels$V3 <- NULL
fips.labels$V5 <- NULL
colnames(fips.labels) <- c("County", "FIPS")

#Merge FIPS database with election results
long16 <- merge(long16, fips.labels, by.x = "fips", by.y = "FIPS", all.x = TRUE)

####Map Generation####
#Virginia
#Pull county information
counties <- counties("VA")

#Merge map file with election results based on FIPS code
df_merged <- geo_join(counties, long16, "GEOID", "fips")

#Determine color gradient for the map
pal <- colorNumeric(
  palette = c("blue", "red"),
  domain = df_merged$percent
)

#Determine the content of the pop-up on mouse click
popup <- paste0("<b>", paste(df_merged$County, df_merged$st, sep = ", "), "</b> <br>",
                "<b>FIPS Code: </b>", df_merged$GEOID, "<br>", 
                "<b>Trump Differential: </b>", percent(round(df_merged$diff,2)),
                "<br>", "<b>Trump Vote: </b>", percent(round(df_merged$DonaldTrump,2)), 
                "<br>", "<b>Clinton Vote: </b>", percent(round(df_merged$HillaryClinton,2)))

#Generate the interactive map with legend
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = df_merged, 
              fillColor = ~pal(diff), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.8, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = df_merged$diff, 
            position = "bottomright", 
            title = "Donald Trump's Advantage",
            labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x))

#Pennsylvania
counties2 <- counties("PA")
df_merged2 <- geo_join(counties2, long16, "GEOID", "fips")

pal <- colorNumeric(
  palette = c("blue", "red"),
  domain = df_merged$percent
)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = df_merged2, 
              fillColor = ~pal(diff), 
              color = "#b2aeae",
              fillOpacity = 0.8, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = df_merged$diff, 
            position = "bottomright", 
            title = "Donald Trump's Advantage",
            labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x))
