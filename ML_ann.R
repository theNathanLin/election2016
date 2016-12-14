library(neuralnet)
library(clusterSim) #this package causes R to freeze..

data12_knn <- data_2012[,4:17]
data16_knn <- data_2012[,4:17]

#normalize dataset
concrete2 <- data.Normalization(data12_knn[-14],type="n4") # PROBLEM LINE

#randomize
set.seed(1234)
concrete2 <- concrete2[order(runif(3112)),]

#75% training, 25% testing
concrete_train <- concrete2[1:2334,]
concrete_test <- concrete2[2335:3112,]

#build neural net
m <- neuralnet(ObamaWin ~ ., data=concrete_train, hidden=1)

#visualize topology
plot(m)

#prediction for 
p <- compute(m,test)
strength_predictions <- p$net.result



###STOP HERE ###







normalize <- function(x) {
  return((x-min(x))/(max(x) - min(x)))
}

## Apply the normalization function:

concrete_norm <- as.data.frame(lapply(data12_knn[-14], normalize))


## Make test and train data sets:

concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
library(neuralnet)

## Create the model
concrete_model <- neuralnet(strength ~ cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data = concrete_train)
## Plot the topology
plot(concrete_model)

## Compute the results of the model on the test data.  The compute function does not work like our 
## typical "predict" function.  It has two fields: "neurons" and "net.result." We want the latter.
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result

## We can't use a confusion matrix since we are looking at a numeric prediction problem rather than a 
## classification problem.  Instead we'll look at the correlation of our predicted results and the 
## "truth."
cor(predicted_strength[1:256], concrete_test$strength[1:256])