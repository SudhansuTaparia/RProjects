# Load library
library(randomForest)
# Help on ramdonForest package and function
library(help=randomForest)
help(randomForest)

termCrosssell<-read.csv(file="bank_data.csv",header = T)
names(termCrosssell)

table(termCrosssell$y)/nrow(termCrosssell)

sample.ind <- sample(2, 
                     nrow(termCrosssell),
                     replace = T,
                     prob = c(0.6,0.4))
cross.sell.dev <- termCrosssell[sample.ind==1,]
cross.sell.val <- termCrosssell[sample.ind==2,]

table(cross.sell.dev$y)/nrow(cross.sell.dev)


table(cross.sell.val$y)/nrow(cross.sell.val)

class(cross.sell.dev$y)

varNames <- names(cross.sell.dev)
varNames <- varNames[!varNames %in% c("y")]
varNames1 <- paste(varNames, collapse = "+")
rf.form <- as.formula(paste("y", varNames1, sep = " ~ "))


cross.sell.rf <- randomForest(rf.form,
                              cross.sell.dev,
                              ntree=500,
                              importance=T)

plot(cross.sell.rf)


# Variable Importance Plot
varImpPlot(cross.sell.rf,
           sort = T,
           main="Variable Importance",
           n.var=5)


var.imp <- data.frame(importance(cross.sell.rf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]


# Predicting response variable
cross.sell.dev$predicted.response <- predict(cross.sell.rf ,cross.sell.dev)

# Load Library or packages
library(e1071)
library(caret)

# Create Confusion Matrix
table(predict=cross.sell.dev$predicted.response, truth=cross.sell.dev$y)



cross.sell.val$predicted.response <- predict(cross.sell.rf ,cross.sell.val)
table(predict=cross.sell.val$predicted.response, truth=cross.sell.val$y)
