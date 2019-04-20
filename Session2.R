#Session 2

#In this session, we will delve deeper into data analytics use cases using R, and see how R allows us to build models 
# such as predictive analytics, social media mining, among others. 
# Some of the models that we discuss in this class will be introduced in much more detail in subsequent courses. 
# Instead, in this session we will look at how do we employ such models using R and the infrastructure that exists in 
# R ecosystem.


#Putting it all together. Let us start with a simple case study with Titanic dataset
titanicdata = read.csv("train.csv")

#Structure of data
str(titanicdata)

#A very simplistic case of the issue of NA values
gooddata <- complete.cases(titanicdata)
data2 <- titanicdata[gooddata,][1:12,]

#Let us look at how overall data feels like
summary(titanicdata)
#accessing a specific column of dataset
titanicdata$Age
#Let us see what was the average fare paid 
mean(titanicdata$Fare)
#How do median and mode look in this case
median(titanicdata$Fare)

ticketfare <- table(titanicdata$Fare)
names(ticketfare)[ticketfare == max(ticketfare)]

#Range and standard deviation
range(titanicdata$Fare)
sd(titanicdata$Fare)

#Let us visualize it graphically
hist(titanicdata$Fare, main = "Fare Chart", xlab = "Fare", col = "red", breaks = 50, 
     xlim = c(0,300))

#Let us see how many survived
table(titanicdata$Survived)

prop.table(table(titanicdata$Sex, titanicdata$Survived))

#Let us do some hypothesis testing now
install.packages(PASWR)
library(PASWR)

#What does the following set of commands do
sample2 <- subset(titanicdata, titanicdata$Pclass == 1)
sample3 <- subset(titanicdata, titanicdata$Pclass != 1)
z.test(x=sample2$Survived,y=sample3$Survived, alternative="two.sided",mu=0,sigma.x=0.5, sigma.y=0.5, conf.level=0.95)
z.test(x=sample2$Survived,y=sample3$Survived, alternative="greater",mu=0,sigma.x=0.5, sigma.y=0.5, conf.level=0.95)
z.test(x=sample2$Survived,y=sample3$Survived, alternative="less",mu=0,sigma.x=0.5, sigma.y=0.5, conf.level=0.95)

#Leet us plot correlation 
plot(titanicdata$Fare, titanicdata$Age, xlab = 'Fare', ylab = 'Age')

cor.test(titanicdata$Fare, titanicdata$Age)

names(titanicdata)

#look at structure of dataset
str(titanicdata)
#By default, all text strings are imported as factors. If you dont want that:
data1 <- read.csv("train.csv",stringsAsFactors=FALSE)
str(data1)


#Analyse Titanic Data
trainData <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

head(trainData)
plot(density(trainData$Age, na.rm = TRUE))
plot(density(trainData$Fare, na.rm = TRUE))

counts <- table(trainData$Survived, trainData$Sex)
counts
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")
counts[2] / (counts[1] + counts[2])
counts[4] / (counts[3] + counts[4])

Pclass_survival <- table(trainData$Survived, trainData$Pclass)
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People",
        main = "survived and deceased between male and female")
Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2])
Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4])
Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6])

#Keep only specific columns
trainData = trainData[-c(1,9:12)]

#Convert gender to dummy variables
trainData$Sex = gsub("female", 1, trainData$Sex)
trainData$Sex = gsub("^male", 0, trainData$Sex)

#Let us try to deal with the issue of missing age numbers, and do a bit more
# advanced stuff
master = grep("Master.",trainData$Name, fixed=TRUE)
miss = grep("Miss.", trainData$Name, fixed=TRUE)
mrs = grep("Mrs.", trainData$Name, fixed=TRUE)
mr = grep("Mr.", trainData$Name, fixed=TRUE)
dr = grep("Dr.", trainData$Name, fixed=TRUE)


#What do we do here.
for(i in master) {
  trainData$Name[i] = "Master"
}
for(i in miss) {
  trainData$Name[i] = "Miss"
}
for(i in mrs) {
  trainData$Name[i] = "Mrs"
}
for(i in mr) {
  trainData$Name[i] = "Mr"
}
for(i in dr) {
  trainData$Name[i] = "Dr"
}


master_age = round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age = round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)

master_age
miss_age
mrs_age
mr_age
dr_age

for (i in 1:nrow(trainData)) {
  if (is.na(trainData[i,5])) {
    if (trainData$Name[i] == "Master") {
      trainData$Age[i] = master_age
    } else if (trainData$Name[i] == "Miss") {
      trainData$Age[i] = miss_age
    } else if (trainData$Name[i] == "Mrs") {
      trainData$Age[i] = mrs_age
    } else if (trainData$Name[i] == "Mr") {
      trainData$Age[i] = mr_age
    } else if (trainData$Name[i] == "Dr") {
      trainData$Age[i] = dr_age
    } else {
      print("Uncaught Title")
    }
  }
}


#Create some additional variables
#Why do we do it here
trainData["Child"]
for (i in 1:nrow(trainData)) {
  if (trainData$Age[i] <= 12) {
    trainData$Child[i] = 1
  } else {
    trainData$Child[i] = 2
  }
}


trainData["Family"] = NA

for(i in 1:nrow(trainData)) {
  x = trainData$SibSp[i]
  y = trainData$Parch[i]
  trainData$Family[i] = x + y + 1
}

trainData["Mother"] = NA
for(i in 1:nrow(trainData)) {
  if(trainData$Name[i] == "Mrs" & trainData$Parch[i] > 0) {
    trainData$Mother[i] = 1
  } else {
    trainData$Mother[i] = 2
  }
}


#Same code for test data
PassengerId = testData[1]
testData = testData[-c(1, 8:11)]

testData$Sex = gsub("female", 1, testData$Sex)
testData$Sex = gsub("^male", 0, testData$Sex)

test_master = grep("Master.",testData$Name)
test_miss = grep("Miss.", testData$Name)
test_mrs = grep("Mrs.", testData$Name)
test_mr = grep("Mr.", testData$Name)
test_dr = grep("Dr.", testData$Name)

for(i in test_master) {
  testData[i, 2] = "Master"
}
for(i in test_miss) {
  testData[i, 2] = "Miss"
}
for(i in test_mrs) {
  testData[i, 2] = "Mrs"
}
for(i in test_mr) {
  testData[i, 2] = "Mr"
}
for(i in test_dr) {
  testData[i, 2] = "Dr"
}

test_master_age = round(mean(testData$Age[testData$Name == "Master"], na.rm = TRUE), digits = 2)
test_miss_age = round(mean(testData$Age[testData$Name == "Miss"], na.rm = TRUE), digits =2)
test_mrs_age = round(mean(testData$Age[testData$Name == "Mrs"], na.rm = TRUE), digits = 2)
test_mr_age = round(mean(testData$Age[testData$Name == "Mr"], na.rm = TRUE), digits = 2)
test_dr_age = round(mean(testData$Age[testData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(testData)) {
  if (is.na(testData[i,4])) {
    if (testData[i, 2] == "Master") {
      testData[i, 4] = test_master_age
    } else if (testData[i, 2] == "Miss") {
      testData[i, 4] = test_miss_age
    } else if (testData[i, 2] == "Mrs") {
      testData[i, 4] = test_mrs_age
    } else if (testData[i, 2] == "Mr") {
      testData[i, 4] = test_mr_age
    } else if (testData[i, 2] == "Dr") {
      testData[i, 4] = test_dr_age
    } else {
      print(paste("Uncaught title at: ", i, sep=""))
      print(paste("The title unrecognized was: ", testData[i,2], sep=""))
    }
  }
}
testData[89, 4] = test_miss_age

testData["Child"] = NA

for (i in 1:nrow(testData)) {
  if (testData[i, 4] <= 12) {
    testData[i, 7] = 1
  } else {
    testData[i, 7] = 1
  }
}

testData["Family"] = NA

for(i in 1:nrow(testData)) {
  testData[i, 8] = testData[i, 5] + testData[i, 6] + 1
}

testData["Mother"] = NA

for(i in 1:nrow(testData)) {
  if(testData[i, 2] == "Mrs" & testData[i, 6] > 0) {
    testData[i, 9] = 1
  } else {
    testData[i, 9] = 2
  }
}


train.glm <- glm(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother, family = binomial, data = trainData)
summary(train.glm)

p.hats <- predict.glm(train.glm, newdata = testData, type = "response")
summary(p.hats)
survival <- vector()
for(i in 1:length(p.hats)) {
  if(p.hats[i] > .5) {
    survival[i] <- 1
  } else {
    survival[i] <- 0
  }
}

install.packages('randomForest')
library(randomForest)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age , data=trainData, importance=TRUE, ntree=2000)
Prediction <- predict(fit, trainData)
summary(Prediction)

train.lm <- lm(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother, data = trainData)
summary(train.lm)

p.lmhats <- predict.lm(train.lm, newdata = testData, type = "response")
summary(p.lmhats)


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

stopwords()[1:10]

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


#Social Media Analytics
#Big Data - Daily 2.5 million TB of data is generated. 
#1 Billion tweets are generated every 2 days.

#More and more people distrust mainstream news media. Surveys regularly estimate numbers to be from 40-60%
#Peer reviews matter more than advertising. Search engines matter more for research. 
# Social media analytics is important that ever, but how do we handle large amounts of information, and try to 
# get objective information from subjective opinions

install.packages("twitteR")
library(twitteR)
library(ROAuth)
library(RCurl)

#We setup a Twitter authentication process
download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="D:/CBA/CBA/cacert.pem")

key <- "sqQv5m6ymEft206ZO6zeg"
secret <- "6pv8jvwGULnWsZQSxxMs4txF8Gi7stX704xTYH0vok"

accessToken = "2218358293-nI1efnehi8pvci74OwAlhKjiNo3xIW3uKBEqQAA"
accessSecret = "zNnmLIaTfZ5E2X1ppVuozJHhUUqTEGvOkjpuz39jAEehG"

setup_twitter_oauth(key,
                    secret,
                    accessToken,
                    accessSecret)

#searchTwitter function searches for tweets for a specific keyword, and number of tweets
bigdata <- searchTwitter("#syria", n=1500)
class(bigdata)

head(bigdata)

#To access a specific object in a list
bigdata[[2]]

# conversion from list to data frame
bigdata.df <- do.call(rbind, lapply(bigdata, as.data.frame))

#Output to CSV
write.csv(bigdata.df, "D:/CBA/CBA/bigdata.csv")

#Now, let us do some analysis. Convert it to a term-document matrix. What is a term-document matrix?

install.packages("tm", dependencies=TRUE)
library("tm")

#What does the follwing line do?
# The first line uses a common family of functions (apply, lapply, and sapply). These
# functions, in general, deploy a function across a range of structured data. Sapply
# traverses a matrix, performs a function to retrieve text from the tweets, and turns the
# text into a list object, bigdata_list.
bigdata_list <- sapply(bigdata, function(x) x$getText())

clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

clean_text = clean.text(bigdata_list)
#bigdata_list <- iconv(bigdata_list,to="utf-8-mac")
# The next line of code uses the Corpus function
# to turn the list of tweets into a corpus. A corpus is an abstraction in R to represent
# a collection of documents.
bigdata_corpus <- Corpus(VectorSource(clean_text))

bigdata_corpus <- tm_map(bigdata_corpus, content_transformer(tolower))

tdm1 = TermDocumentMatrix(
  bigdata_corpus,
  control = list(
    removePunctuation = TRUE,
    stopwords = c(stopwords("english")),
    removeNumbers = TRUE, tolower = TRUE)
)


m1 = as.matrix(tdm1)

# get word counts in decreasing order
word_freqs = sort(rowSums(m1), decreasing = TRUE) 
word_freqs

dm = data.frame(word = names(word_freqs), freq = word_freqs)

wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

library(sentiment)

# classify emotion
emo = classify_emotion(clean_text, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(clean_text, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

df = data.frame(text=clean_text, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
df = within(df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

library(plyr)
library(ggplot2)
library(RColorBrewer)

ggplot(df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets")
  

# separating text by emotion
emos = levels(factor(df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = clean_text[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)



user <- getUser("sachin_rt")

userFriends <- user$getFriends()
userFollowers <- user$getFollowers()
userNeighbors <- union(userFollowers, userFriends)

userNeighbors.df = twListToDF(userNeighbors)

#another function of twitteR package
rdmTweets <- userTimeline("rdatamining", n=100)
