install.packages("e1071")
library(e1071)

DiseaseZ = read.table("DiseaseZ.txt", header = T, sep="\t")
Sick = subset(DiseaseZ, DiseaseZ=="YES")
NotSick = subset(DiseaseZ, DiseaseZ=="NO")
dim(Sick)[1]
dim(NotSick)[1]

prob.Sick = colSums(Sick[,1:6]== "YES")/6
prob.Sick
prob.NotSick = colSums(NotSick[,1:6]== "NO")/4
prob.NotSick

Classify = naiveBayes(DiseaseZ[1:10,1:6],DiseaseZ[1:10,7])
Classify

predict(Classify, DiseaseZ[11,1:6])


#Examples
#Titanic
Titanic.df = read.csv("titanic.csv")

Titanic.df$Filter= sample(c("TRAIN","TEST"),nrow(Titanic.df), replace = T)
TRAIN = subset (Titanic.df, Filter == "TRAIN")
TEST = subset (Titanic.df, Filter == "TEST")

Classify = naiveBayes(TRAIN[2:4],TRAIN[[5]])
Classify

TEST$Classified = predict(Classify,TEST[1:3])
table(TEST$Survived,TEST$Classified)


#Predicting stock prices
install.packages("quantmod")
library(quantmod)

install.packages("lubridate")
install.packages("stringi")
library("lubridate")

startDate = as.Date("2012-01-01")
# The beginning of the date range we want to look at
endDate = as.Date("2016-01-01") 
# The end of the date range we want to look at
getSymbols("AAPL", src = "yahoo", from = startDate, to = endDate) 


DayofWeek<-wday(AAPL, label=TRUE)
#Find the day of the week

PriceChange<- Cl(AAPL) - Op(AAPL)
#Find the difference between the close price and open price

Class<-ifelse(PriceChange>0,"UP","DOWN")
#Convert to a binary classification. (In our data set, there are no bars with an exactly 0 price change so, for simplicity sake, we will not address bars that had the same open and close price.)
DataSet<-data.frame(DayofWeek,Class)
#Create our data set

MyModel<-naiveBayes(DataSet[,1],DataSet[,2])
MyModel

TrainingSet<-DataSet[1:328,]
#We will use of the data to train the model
TestSet<-DataSet[329:492,] 
#And to test it on unseen data

CrossModel<-naiveBayes(TrainingSet[,1],TrainingSet[,2])

table(predict(CrossModel,TestSet),TestSet[,2],dnn=list('predicted','actual'))


#Case Study 2 Spam filter
#Now, let us create a spam filter

smsdata <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

str(smsdata)
smsdata$type <- factor(smsdata$type)
str(smsdata$type)

summary(smsdata)

table(smsdata$type)

library(tm)

sms_corpus <- Corpus(VectorSource(smsdata$text))
sms_corpus
print(sms_corpus)
inspect(sms_corpus[1:10])

#What to do about different types of words?
clean_corpus <- tm_map(sms_corpus, content_transformer(tolower))
clean_corpus <- tm_map(clean_corpus, removeNumbers)
clean_corpus <- tm_map(clean_corpus, removePunctuation)

#Are we good or should we do something else as well?

stopwords()[1:100]

clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

inspect(clean_corpus[1:3])

sms_dtm <- DocumentTermMatrix(clean_corpus)
inspect(sms_dtm[1:4, 11:20])

spam_indices <- which(smsdata$type == "spam")
spam_indices[1:3]

ham_indices <- which(smsdata$type == "ham")
ham_indices[1:3]

library(wordcloud)
wordcloud(clean_corpus[ham_indices], min.freq=50)

wordcloud(clean_corpus[spam_indices], min.freq=50)


smsdata_train <- smsdata[1:4100,]
smsdata_test <- smsdata[4101:5574,]

sms_dtm_train <- sms_dtm[1:4100,]
sms_dtm_test <- sms_dtm[4101:5574,]
sms_corpus_train <- clean_corpus[1:4100]
sms_corpus_test <- clean_corpus[4101:5574]

prop.table(table(smsdata_train$type))
prop.table(table(smsdata_test$type))


spam <- subset(smsdata_train, type == "spam")
ham <- subset(smsdata_train, type == "ham")

five_times_words <- findFreqTerms(sms_dtm_train, 5)
length(five_times_words)
five_times_words
five_times_words[1:10]

sms_train <- DocumentTermMatrix(sms_corpus_train, control=list(dictionary = five_times_words))

sms_test <- DocumentTermMatrix(sms_corpus_test, control=list(dictionary = five_times_words))

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

sms_train <- apply(sms_train, 2, convert_count)
sms_test <- apply(sms_test, 2, convert_count)

library(e1071)

sms_classifier <- naiveBayes(sms_train, factor(smsdata_train$type))
class(sms_classifier)

summary(sms_classifier)

sms_test_pred <- predict(sms_classifier, newdata=sms_test)
summary(sms_test_pred)

table(sms_test_pred, smsdata_test$type)



#Spam email filter
install.packages('ElemStatLearn')
install.packages('klaR')
install.packages('caret')
library('ElemStatLearn')
library("klaR")
library("caret")

sub = sample(nrow(spam), floor(nrow(spam) * 0.9))
train = spam[sub,]
test = spam[-sub,]

xTrain = train[,-58]
yTrain = train$spam

xTest = test[,-58]
yTest = test$spam

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))

prop.table(table(predict(model$finalModel,xTest)$class,yTest))


