all_counties_2012 <- read.csv("~/Documents/OneDrive/UVA_2ndyr/DS4559/election2016/Data/2012data/all_counties_2012.csv")
all_counties_2012 <- subset(all_counties_2012, fips!="fips")
#case insensitive search for rows containing obama or romney
all_counties_2012_romney <- all_counties_2012[grepl('romney',all_counties_2012$candidate,ignore.case=TRUE), ] 
all_counties_2012_obama <- all_counties_2012[grepl('obama',all_counties_2012$candidate,ignore.case=TRUE), ] 
all_counties_2012_obama <- aggregate (. ~ fips, data=all_counties_2012_obama, FUN=sum)
all_counties_2012_romney <- aggregate (. ~ fips, data=all_counties_2012_romney, FUN=sum)

all_counties_2012 <- rbind(all_counties_2012_romney,all_counties_2012_obama)
all_counties_2012 <- subset(all_counties_2012, fips !="")
summary(all_counties_2012)
summary(all_counties_2012$fips)

#include only first letter
all_counties_2012[,3] <- substring(all_counties_2012[,3], 1, 1) 

all_counties_2012$candidate <- replace(all_counties_2012$candidate, all_counties_2012$candidate=="o", "obama")
all_counties_2012$candidate <- replace(all_counties_2012$candidate, all_counties_2012$candidate=="O", "obama")
all_counties_2012$candidate <- replace(all_counties_2012$candidate, all_counties_2012$candidate=="B", "obama")
all_counties_2012$candidate <- replace(all_counties_2012$candidate, all_counties_2012$candidate=="b", "obama")
all_counties_2012$candidate <- replace(all_counties_2012$candidate, all_counties_2012$candidate=="m", "romney")
all_counties_2012$candidate <- replace(all_counties_2012$candidate, all_counties_2012$candidate=="M", "romney")
all_counties_2012$candidate <- replace(all_counties_2012$candidate, all_counties_2012$candidate=="r", "romney")
all_counties_2012$candidate <- replace(all_counties_2012$candidate, all_counties_2012$candidate=="R", "romney")
summary(all_counties_2012$candidate)

all_counties_2012 <- data.frame(lapply(all_counties_2012, as.character), stringsAsFactors=TRUE)
summary(all_counties_2012)
summary(all_counties_2012$fips)
aggregate(. ~ cbind(fips, candidate), data=df, FUN=sum)

