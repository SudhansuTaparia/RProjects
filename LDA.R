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


qda.fit = qda(Weekly$Direction ~ Weekly$Lag1+Weekly$Lag2+Weekly$Lag3+Weekly$Lag4+Weekly$Lag5+Weekly$Volume, data=Weekly)
qda.fit
qda.class=predict(qda.fit,Weekly.test)$class
table(qda.class,Weekly.test$Direction)
mean(qda.class==Weekly.test$Direction)


vec <- c(weekly.lda$scaling[1,1], weekly.lda$scaling[6,1])
v   <- vec / sqrt(sum(vec^2))  # make it a unit vector
v
lda1.points <- as.matrix(Weekly[,c(1,6)]) %*% v %*% t(v) # to project point X into unit vector v just calculate X.v.v^T

plot(Weekly[,"Lag1"], Weekly[,"Volume"], 
     col=c("blue","green","red")[Weekly$Direction], pch=19,
     xlab="Lag1", ylab="Volume", ,  main="1st discriminant functions")
segments(-vec[1],-vec[2],vec[1],vec[2])
points(lda1.points , col=c("blue","green")[Weekly$Direction], pch=18) # draw projection point
for(i in 1:nrow(Weekly)) {
  segments(Weekly[i,1], Weekly[i,6], lda1.points[i,1], lda1.points[i,2], 
           lty=2, col=c("blue","green","red")[Weekly[i,]$Direction])
}



