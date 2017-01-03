#General Strategy: create 4 popular vote models for Romney, Obama, Trump, Clinton

#Create the nescessary data frames for easy model creation
data <- read.csv("Data/ML_Census_subset")
#multiply by 100 to get "percent" figures
normalize <- function(x) {
  return((x-min(x))/(max(x) - min(x)))
}

romney_results <- read.csv("Data/2012+Romney+results.csv")
romney_data <- merge(data, romney_results, by = "fips")
#normalize population and income, leaving percents intact
romney_data[,c(5:9,11,13,16,17,21)] <- as.data.frame(lapply(romney_data[,c(5:9,11,13,16,17,21)], normalize))


obama_results <- read.csv("Data/2012+Obama+results.csv")
obama_data <- merge(data, obama_results, by = "fips")
obama_data[,c(5,6,17)] <- as.data.frame(lapply(obama_data[,c(5,6,17)], normalize))


trump_results <- read.csv("Data/2016+Trump+results.csv")
trump_data <- merge(data, trump_results, by = "fips")
trump_data <- merge(trump_data, romney_results[,c(2,5)], by.x = 'fips', by.y = 'fips', all.x = TRUE)

trump_data[,c(5:9,11,13,16,17,21,25)] <- as.data.frame(lapply(trump_data[,c(5:9,11,13,16,17,21,25)], normalize))

clinton_results <- read.csv("Data/2016+Clinton+results.csv")
clinton_data <- merge(data, clinton_results, by = "fips")
clinton_data[,c(5,6,17)] <- as.data.frame(lapply(clinton_data[,c(5,6,17)], normalize))

set.seed(1)
library(neuralnet)
## Create the models choosing top attributes
romney_model <- neuralnet(votes ~ median_Income+pop_sqr_mile2010+pop_total_2010+p_bachelors+p_asian+p_black+p_female+p_hisp+pop_65., data = romney_data,hidden = c(4,3))
obama_model <- neuralnet(votes ~ median_Income+pop_sqr_mile2010+pop_total_2010+p_bachelors+p_asian+p_black+p_female+p_hisp+pop_65., data = obama_data,hidden = c(4,3))

trump_model <- neuralnet(votes.x ~ votes.y+median_Income+pop_sqr_mile2010+pop_total_2010+p_bachelors+p_asian+p_black+p_female+p_hisp+pop_65., data = trump_data,hidden = c(10,2))
clinton_model <- neuralnet(total_votes ~ median_Income+pop_sqr_mile2010+pop_total_2010+p_bachelors+p_asian+p_black+p_female+p_hisp+pop_65., data = clinton_data,hidden = c(4,3))

#Testing the simple model
romney_model_simple <- neuralnet(votes ~ pop_total_2010, data = romney_data,hidden = c(1))
romney_results_simple <- compute(romney_model_simple, romney_data[17])
simple_prediction <- romney_results_simple$net.result
summary(simple_prediction)
plot(romney_data[,17],simple_prediction)

#Testing Romney's full model
plot(romney_model)
romney_results <- compute(romney_model, romney_data[,c(5:9,11,13,16,17)])
predicted_votes <- romney_results$net.result
plot(romney_data$votes,predicted_votes)
cor(romney_data$votes,predicted_votes)

#Testing Trump's full model
plot(trump_model)
trump_results <- compute(trump_model, trump_data[,c(5:9,11,13,16,17,25)])
predicted_votes <- trump_results$net.result
plot(trump_data$votes.x,predicted_votes)
cor(trump_data$votes.x,predicted_votes)
