
### generate data ###

set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,]+1

plot(x, col=(3-y),pch=19)

### Cost and its impact on margin width and no. of SVs ###

dat=data.frame(x=x, y=as.factor(y))
library(e1071)

svmfit = svm(y~ ., data=dat, kernel="linear", cost=10, scale=FALSE)

plot(svmfit , dat)
summary(svmfit)
svmfit$index

### Refit model with lower cost ###

svmfit=svm(y~ ., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit , dat)
summary(svmfit)
svmfit$index

### find best parameter values ###

set.seed(1)
tune.out=tune(svm, y~ ., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.09, 0.1, 0.2,5,10,100)))

summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

ypred=predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)

### decrease cost and predict ###
svmfit=svm(y~ ., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)




