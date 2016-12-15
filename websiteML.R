census <- read.csv("Data/county_facts.csv", header = TRUE, stringsAsFactors = TRUE)
#check for unclean data
data <- subset(census,select=c("fips", "area_name", "state_abbreviation", "POP060210","PST040210","AGE775214","SEX255214","RHI225214", "RHI325214", "RHI425214", "RHI525214", "RHI725214", "RHI825214", "EDU635213", "EDU685213", "INC110213", "PVY020213"))
data <- subset(data, fips !=0) #remove USA
data <- subset(data, state_abbreviation !="") #remove states
colnames(data) <- c("fips", "area_name", "state_abbreviation", "pop_sqr_mile2010","pop_total_2010","pop_65+","p_female","p_black", "p_indian", "p_asian", "p_PI", "p_hisp", "p_white", "p_HS", "p_bachelors", "median_Income", "p_below_poverty_line")
pairs(data[,4:8])
data$fips <- as.factor(data$fips)
#set factors correctly

#symnum(cor(data[,4:17], use="complete.obs"))
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
#print(importance)
plot(importance, ylab = 'Attributes', main = 'Attribute Importance')

#2016
model <- train(lead ~., data=train_2016[,4:17], method="lvq", preProcess="scale")#, trControl=control)
#Estimating variable importance
importance <- varImp(model, scale=FALSE)
#print(importance)
plot(importance, ylab = 'Attributes', main = 'Attribute Importance')


####################################################### REANALYZE!!!!!
# our testing data has WAY more trump counties? fix later?
#86.4% accuracy
tree_model = ctree(ObamaWin ~ ., train_2012[,4:17]) 
#plot(tree_model)
ctree_pred <- predict(tree_model,test_2012[,4:17])
CrossTable(test_2012$ObamaWin, ctree_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))

#Using 2012 model for 2016 accuracy: 71.65
ctree_pred2 <- predict(tree_model,test_2016[,4:17])
CrossTable(test_2016$lead, ctree_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))

#Using jRip
jrip_model <- JRip(ObamaWin ~ ., train_2012[,4:17])
jrip_model

jrip_pred <- predict(jrip_model,test_2012[,4:17])
jrip_pred2 <- predict(jrip_model,test_2016[,4:17])

#2012 accuracy: 85.3
CrossTable(test_2012$ObamaWin, jrip_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))

#Using 2012 model for 2016 election accuracy: 93.5%
CrossTable(test_2016$lead, jrip_pred2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))

jrip_model2 <- JRip(lead ~ ., train_2016[,4:17])
jrip_model2
#2016 92% accuracy
jrip_pred3 <- predict(jrip_model2,test_2016[,4:17])
CrossTable(test_2016$lead, jrip_pred3,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Type', 'Predicted Type'))
