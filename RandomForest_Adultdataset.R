library(randomForest)
library(ROCR)

adult_trainset <- read.csv("adult_trainset.csv", h = T)
adult_trainset[,"income"] <- as.factor(adult_trainset[,"income"])

adult_valset <- read.csv("adult_valset.csv", h = T)
adult_valset[,"income"] <- as.factor(adult_valset[,"income"])

bestmtry <- tuneRF(adult_trainset[-13], adult_trainset$income, ntreeTry=20, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)

adult.rf <-randomForest(income ~., data=adult_trainset, mtry=2, ntree=10, keep.forest=TRUE, importance=TRUE,test=adult_valset)

adult.rf.pr = predict(adult.rf,type="prob",newdata=adult_valset)[,2]
adult.rf.pred = prediction(adult.rf.pr, adult_valset$income)
adult.rf.perf = performance(adult.rf.pred,"tpr","fpr")
plot(adult.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

importance(adult.rf)
varImpPlot(adult.rf)
