#Wine dataset
library("MASS")
library(car)
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep=",")


wine[2:6]

scatterplotMatrix(wine[2:6])


wine.lda <- lda(wine$V1 ~ wine$V2 + wine$V3 + wine$V4 + wine$V5 + wine$V6 + wine$V7 +
                  wine$V8 + wine$V9 + wine$V10 + wine$V11 + wine$V12 + wine$V13 +
                  wine$V14)

wine.lda

(wine.lda$svd)^2

variation_proportion = (wine.lda$svd)^2/sum((wine.lda$svd)^2)
variation_proportion

wine.lda.values <- predict(wine.lda, wine[2:14])
head(wine.lda.values$class)
head(wine.lda.values$posterior)
head(wine.lda.values$x)

table(wine.lda.values$class,wine$V1)

projecteddata <- as.matrix(wine[,2:14])%*%wine.lda$scaling
plot(projecteddata,col=wine[,1])

ldahist(data = wine.lda.values$x[,1], g=wine$V1)
ldahist(data = wine.lda.values$x[,2], g=wine$V1)


#Stock return prediction
library(ISLR)
library(MASS)
data(package="ISLR")
data(Weekly)

summary(Weekly)

weekly.lda <- lda(Weekly$Direction ~ Weekly$Lag1+Weekly$Lag2+Weekly$Lag3+Weekly$Lag4+Weekly$Lag5+Weekly$Volume)

weekly.lda

variation_proportion = (weekly.lda$svd)^2/sum((weekly.lda$svd)^2)
variation_proportion

weekly.lda.values <- predict(weekly.lda, Weekly[2:7])
head(weekly.lda.values$class)
head(weekly.lda.values$posterior)

table(weekly.lda.values$class,Weekly$Direction)

projecteddata <- as.matrix(Weekly[,2:7])%*%weekly.lda$scaling
plot(projecteddata,col=Weekly[,9])





