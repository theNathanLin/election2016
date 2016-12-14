## Here, we examine and practice use of the kNN machine learning algorithm.
## load the data and the working directory (optional)
setwd("~/Documents/OneDrive/UVA_2ndyr/DS4559/inclass/day12")
prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors=FALSE)

## We must ALWAYS normalize data before using the kNN algorithm.  Why?
## Here, we write a function to normalize any vector of variables, x.
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

## Next, we randomize the data to make sure we have a good mix of benign and malignant
## classes in both the training and the testing set:
set.seed(1234)
prc_r <- prc[order(runif(100)),]

## We create a new dataframe in which we have normalized all of the attribute columns of prc_r
prc_n <- as.data.frame(lapply(prc_r[2:8], normalize))

## A quick check that we succeeded at normalization shows a min of 0 and a max of 1, so we
## are confident that we can proceed.
summary(prc_n$radius)
#need to RANDOMIZE ---- start_here
data12_knn <- data_2012[,4:17]
data16_knn <- data_2012[,4:17]

## Now, we divide the data into testin and training data (just the attributes)
prc_train <- as.data.frame(lapply(data12_knn[1:2334,1:13], normalize))
prc_test <- as.data.frame(lapply(data12_knn[2335:3112,1:13], normalize))

## We make separate vectors for the classes for training and testing that correspond to the 
## matrices above:


prc_train_labels <- data12_knn[1:2334,14]
prc_test_labels <- data12_knn[2335:3112,14]

## "class" is the package that allows us to perform kNN analysis
install.packages("class")
library(class)

## Here we perform kNN analysis.  k= 15
prc_test_pred <- knn(train=prc_train,test=prc_test,cl=prc_train_labels,k=15)

## Evaluate

library(gmodels)
CrossTable(x=prc_test_labels, y=prc_test_pred,prop.chisq = FALSE)


