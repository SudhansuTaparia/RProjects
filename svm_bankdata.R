#SVM on the same dataset

termCrosssell <-read.csv(file="bank_data.csv",header = T)

## Explore data frame
names(termCrosssell)

## Dependent Variable

table(termCrosssell$y)

table(termCrosssell$y)/length(termCrosssell$y)

summary(termCrosssell$age)
summary(termCrosssell$job)

## Split into development and validation sample

svm.develop <- termCrosssell[sample(nrow(termCrosssell),
                                    size=20000,
                                    replace=TRUE),]

table(svm.develop$y)/length(svm.develop$y)

svm.validate <- termCrosssell[sample(nrow(termCrosssell),
                                     size=20000,
                                     replace=TRUE),]
table(svm.validate$y)/length(svm.validate$y)


require(e1071)

svm.model <- svm(y~age+job,
                 data=svm.develop)

summary(svm.model)

svm.validate$Predicted_y <- predict(svm.model, 
                                    svm.validate[,-3])

# Compare Observed and Predicted
table.svm <- table(pred = svm.validate$Predicted_y,
                   true = svm.validate$y)/length(svm.validate$y)

table.svm