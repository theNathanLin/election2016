## Here, we examine and practice use of the kNN machine learning algorithm.
## load the data and the working directory (optional)

#assumes you run the clean_census.csv and generate these two datasets.
data12_knn <- data_2012[,4:17]
data16_knn <- data_2016[,4:17]


## We must ALWAYS normalize data before using the kNN algorithm.  Why?
## Here, we write a function to normalize any vector of variables, x.
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

## Now, we divide the data into testin and training data (just the attributes)
prc_train <- as.data.frame(lapply(data12_knn[1:2489,1:13], normalize))
prc_test <- as.data.frame(lapply(data12_knn[2490:3112,1:13], normalize))

## We make separate vectors for the classes for training and testing that correspond to the 
## matrices above:


prc_train_labels <- data12_knn[1:2334,14]
prc_test_labels <- data12_knn[2335:3112,14]

## "class" is the package that allows us to perform kNN analysis
library(class)

## Here we perform kNN analysis.  k= 15
prc_test_pred <- knn(train=prc_train,test=prc_test,cl=prc_train_labels,k=15)

## Evaluate

library(gmodels)
CrossTable(x=prc_test_labels, y=prc_test_pred,prop.chisq = FALSE)
#2012 KNN Accuracy 0.8997429306



####2016 Election

prc_train <- as.data.frame(lapply(data16_knn[1:2334,1:13], normalize))
prc_test <- as.data.frame(lapply(data16_knn[2335:3112,1:13], normalize))
prc_train_labels <- data16_knn[1:2334,14]
prc_test_labels <- data16_knn[2335:3112,14]

## "class" is the package that allows us to perform kNN analysis
library(class)

## Here we perform kNN analysis.  k= 15
prc_test_pred <- knn(train=prc_train,test=prc_test,cl=prc_train_labels,k=15)

## Evaluate

library(gmodels)
CrossTable(x=prc_test_labels, y=prc_test_pred,prop.chisq = FALSE)
#2016 accuracy 93.7%700

