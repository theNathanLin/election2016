#2016 General Election Cleaning Code
#Nathan Lin

#Libraries needed
library(reshape2)
library(tigris)
library(ggplot2)
library(leaflet)
library(maps)

setwd("~/Second Year/DS 4559 - Data Science/Final Project/election2016/Data")
gen16 <- read.csv("pres16results.csv", header = TRUE, stringsAsFactors = FALSE)

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

#Map Generation
mapCounties = map("county", fill = TRUE, plot = FALSE)
leaflet(data = mapCounties) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

