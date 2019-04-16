iris

iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE)

names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

library(ggvis)

head(iris)

table(iris$Species) 

summary(iris)
     
     library(class)
     
     set.seed(1234)
     
     ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
     
     iris.training <- iris[ind==1, 1:4]
     iris.test <- iris[ind==2, 1:4]
     iris.trainLabels <- iris[ind==1, 5]
     iris.testLabels <- iris[ind==2, 5]
     
     iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
     
     iris_pred
     
     library(gmodels)
     
     CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
     
     