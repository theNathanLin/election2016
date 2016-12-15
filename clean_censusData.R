census <- read.csv("Data/2016dataprimary/county_facts.csv", header = TRUE, stringsAsFactors = TRUE)
#check for unclean data
data <- subset(census,select=c("fips", "area_name", "state_abbreviation", "POP060210","PST040210","AGE775214","SEX255214","RHI225214", "RHI325214", "RHI425214", "RHI525214", "RHI725214", "RHI825214", "EDU635213", "EDU685213", "INC110213", "PVY020213"))
data <- subset(data, fips !=0) #remove USA
data <- subset(data, state_abbreviation !="") #remove states
colnames(data) <- c("fips", "area_name", "state_abbreviation", "pop_sqr_mile2010","pop_total_2010","pop_65+","p_female","p_black", "p_indian", "p_asian", "p_PI", "p_hisp", "p_white", "p_HS", "p_bachelors", "median_Income", "p_below_poverty_line")
pairs(data[,4:8])
data$fips <- as.factor(data$fips)
#set factors correctly

symnum(cor(data[,4:17], use="complete.obs"))
#originally, we had both median income & percent below poverty line. since these had a 60% correlation we chose to keep just median income
data <- data[,1:16]

#import cleaned election data
clean_2012 <- read.csv("Data/clean_2012.csv")
clean_2012 <- clean_2012[,c(2,6)]
clean_2012$fips <- as.factor(clean_2012$fips)

clean_2016 <- read.csv("Data/clean_2016.csv")
clean_2016 <- clean_2016[,c(2,8)]
#Fix Ogala Lakota County FIPS code
clean_2016 [5359,]$fips <- 46113
clean_2016 [5360,]$fips <- 46113
clean_2016$fips <- as.factor(clean_2016$fips)

data_2012 <- merge(data, clean_2012,by="fips")

data_2016 <- merge(data, clean_2016,by="fips")
data_2016 <- data_2016[seq(1, 6223, 2),]

#write the files for later use
write.csv(data_2016, file = "Data/2016+census+results.csv")
write.csv(data_2012, file = "Data/2012+census+results.csv")

#Randomly order the data
set.seed(5)
data_2012 <- data_2012[order(runif(3112)), ]
data_2012$ObamaWin <- as.factor(data_2012$ObamaWin)

data_2016 <- data_2016[order(runif(3112)), ]

#Split the data ~80% training and 20% testing
train_2012 <- data_2012[1:2489,]
test_2012<- data_2012[2489:3112,]

train_2016 <- data_2016[1:2489,]
test_2016<- data_2016[2489:3112,]

#One way to look at attribute importance

library(caret)
library(corrplot)
library(DMwR)
library(ggplot2)
library(reshape2)
library(plyr)
library(sqldf)
library(mlbench)
library(randomForest)
library(gmodels)
library(party)
library(C50)
library(RWeka)

model <- train(ObamaWin ~., data=train_2012[,4:17], method="lvq", preProcess="scale")#, trControl=control)
#Estimating variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance, ylab = 'Attributes', main = 'Attribute Importance')

#2016
## FIX NA PROBLEM!!!!!!!!!!!!
model <- train(lead ~., data=train_2016[,4:17], method="lvq", preProcess="scale")#, trControl=control)
#Estimating variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance, ylab = 'Attributes', main = 'Attribute Importance')


####################################################### REANALYZE!!!!!
# our testing data has WAY more trump counties? fix later?
tree_model = ctree(ObamaWin ~ ., train_2012[,4:17]) 
plot(tree_model)
ctree_pred <- predict(tree_model,test_2012[,4:17])
CrossTable(test_2012$ObamaWin, ctree_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))


ctree_pred2 <- predict(tree_model,test_2016[,4:17])
CrossTable(test_2016$lead, ctree_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))

#not working?
rand_forest = randomForest(ObamaWin ~ ., train_2012[,4:17])

#choose machine learning methods to explore
POP060210	Population per square mile, 2010
PST040210	Population, 2010 (April 1) estimates base
AGE775214	Persons 65 years and over, percent, 2014
SEX255214	Female persons, percent, 2014
RHI225214	Black or African American alone, percent, 2014
RHI325214	American Indian and Alaska Native alone, percent, 2014
RHI425214	Asian alone, percent, 2014
RHI525214	Native Hawaiian and Other Pacific Islander alone, percent, 2014
RHI725214	Hispanic or Latino, percent, 2014
RHI825214	White alone, not Hispanic or Latino, percent, 2014
EDU635213	High school graduate or higher, percent of persons age 25+, 2009-2013
EDU685213	Bachelor's degree or higher, percent of persons age 25+, 2009-2013
INC110213	Median household income, 2009-2013
PVY020213	Persons below poverty level, percent, 2009-2013


[including hispanics?] RHI125214	White alone, percent, 2014
PST045214	Population, 2014 estimate
PST120214	Population, percent change - April 1, 2010 to July 1, 2014
POP010210	Population, 2010
AGE135214	Persons under 5 years, percent, 2014
AGE295214	Persons under 18 years, percent, 2014
RHI625214	Two or More Races, percent, 2014
POP715213	Living in same house 1 year & over, percent, 2009-2013
POP645213	Foreign born persons, percent, 2009-2013
POP815213	Language other than English spoken at home, pct age 5+, 2009-2013
INC910213	Per capita money income in past 12 months (2013 dollars), 2009-2013
VET605213	Veterans, 2009-2013
LFE305213	Mean travel time to work (minutes), workers age 16+, 2009-2013
HSG010214	Housing units, 2014
HSG445213	Homeownership rate, 2009-2013
HSG096213	Housing units in multi-unit structures, percent, 2009-2013
HSG495213	Median value of owner-occupied housing units, 2009-2013
HSD410213	Households, 2009-2013
HSD310213	Persons per household, 2009-2013
BZA010213	Private nonfarm establishments, 2013
BZA110213	Private nonfarm employment, 2013
BZA115213	Private nonfarm employment, percent change, 2012-2013
NES010213	Nonemployer establishments, 2013
SBO001207	Total number of firms, 2007
SBO315207	Black-owned firms, percent, 2007
SBO115207	American Indian- and Alaska Native-owned firms, percent, 2007
SBO215207	Asian-owned firms, percent, 2007
SBO515207	Native Hawaiian- and Other Pacific Islander-owned firms, percent, 2007
SBO415207	Hispanic-owned firms, percent, 2007
SBO015207	Women-owned firms, percent, 2007
MAN450207	Manufacturers shipments, 2007 ($1,000)
WTN220207	Merchant wholesaler sales, 2007 ($1,000)
RTN130207	Retail sales, 2007 ($1,000)
RTN131207	Retail sales per capita, 2007
AFN120207	Accommodation and food services sales, 2007 ($1,000)
BPS030214	Building permits, 2014
LND110210	Land area in square miles, 2010
