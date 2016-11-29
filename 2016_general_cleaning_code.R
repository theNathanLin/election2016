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
gen16 <- read.csv("pres16results.csv", header = TRUE, stringsAsFactors = FALSE)
fips.labels <- read.csv("fips_database.csv", header = FALSE, stringsAsFactors = FALSE, colClasses = "character")

#Cleaning general election data
gen16 <- gen16[which(gen16$cand == "Donald Trump" | gen16$cand == "Hillary Clinton"),]
gen16 <- gen16[-c(1,2),]

sapply(gen16,class)

for (i in seq(nrow(gen16))){
  if (gen16$fips[i] == "46102") gen16$county[i] <- "Oglala Lakota County"
}

gen16.states <- gen16[is.na(gen16$county),]
gen16.states <- gen16.states[-c(103,104),] #removing state FIPS for Alaska
gen16.states$county <- NULL
gen16.states$fips <- NULL

gen16 <- gen16[-is.na(gen16$county)]

for (i in seq(nrow(gen16))) {
  if (nchar(gen16$fips[i])<=2) gen16$fips[i] <- NA
}

gen16 <- gen16[-which(is.na(gen16$fips)),]
gen16$pct_report <- NULL
gen16.2 <- gen16

gen16.2$votes <- NULL
gen16.2$total_votes <- NULL
gen16.2$lead <- NULL

long16 <- dcast(gen16.2, fips + st ~ cand, value.var = "pct")
colnames(long16) <- c("fips", "st", "DonaldTrump", "HillaryClinton")
long16$diff <- long16$DonaldTrump - long16$HillaryClinton

for (i in seq(nrow(long16))) {
  if (nchar(long16$fips[i])==4) long16$fips[i] <- paste("0",long16$fips[i], sep="")
}

#Cleaning FIPS database
fips.labels$fips <- paste(fips.labels$V2,fips.labels$V3, sep = "")
fips.labels$V2 <- NULL
fips.labels$V3 <- NULL
fips.labels$V5 <- NULL
colnames(fips.labels) <- c("State", "County", "FIPS")

#Map Generation
counties <- counties("VA")

df_merged <- geo_join(counties, long16, "GEOID", "fips")

pal <- colorNumeric(
  palette = c("blue", "red"),
  domain = df_merged$percent
)

popup <- paste0("FIPS Code: ", df_merged$GEOID, "<br>", "Trump Differential: ", percent(round(df_merged$diff,2)),
                "<br>", "Trump Vote: ", percent(round(df_merged$DonaldTrump,2)), 
                "<br>", "Clinton Vote: ", percent(round(df_merged$HillaryClinton,2)))

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
