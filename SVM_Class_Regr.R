install.packages("mlbench")
library (e1071)
library (rpart)

##Classification
data(Glass,package="mlbench")
## split data into a train and test set
index <- 1:nrow(Glass)
#returns array of 71 observations
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

svm.model <- svm(Type ~ ., data = trainset, cost = 500, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])
summary(svm.model)
## compute svm confusion matrix
table(pred = svm.pred, true = testset[,10])

#Regression
data(Ozone, package="mlbench")
## split data into a train and test set
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,-3])
trainset <- na.omit(Ozone[-testindex,-3])
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex)
summary(svm.model)