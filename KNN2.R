# comma delimited data and no header for each variable
RawData <- read.table("diabetes.data", sep = ",", header=FALSE)

#response variable is last column, all others are predictors
responseY <- as.matrix(RawData[,dim(RawData)[2]])
predictorX <- as.matrix(RawData[,1:(dim(RawData)[2]-1)])

# principal components analysis using correlation matrix
pca <- princomp(predictorX, cor=T) 
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1]
pc.comp2 <- -1*pc.comp[,2]
dim.red.data <- cbind(pc.comp1, pc.comp2)

library(class)

set.seed(1234)
ind <- sample(2, dim(RawData), replace=TRUE, prob=c(0.67, 0.33))

train.predictorX <- dim.red.data[ind==1, ]
test.predictorX <- dim.red.data[ind==2, ]
train.responseY <- responseY[ind==1, ]
test.responseY <- responseY[ind==2, ]


# re-run below with K = 10, 5, 1
# model.knn <- knn(train = train.predictorX, test = train.predictorX, cl = train.responseY, k = 1, prob = T)
model.knn <- knn(train = train.predictorX, test = test.predictorX, cl = train.responseY, k = 1, prob = T)
model.knn
summary(model.knn)


model.knn <- knn(train = train.predictorX, test = test.predictorX, cl = train.responseY, k = 7, prob = T)
table(model.knn, test.responseY)

model.knn <- knn(train = train.predictorX, test = train.predictorX, cl = train.responseY, k = 1, prob = T)
table(model.knn, train.responseY)

# table(model.knn, train.responseY)

# library(gmodels)
# CrossTable(x = model.knn, y = test.responseY, prop.chisq=FALSE)


