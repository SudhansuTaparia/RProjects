setwd("~/Dropbox/CBA/Boston_Housing")
Boston<-read.csv("BostonHousing.csv", header=T)


# Select rows randomly
row.number<- sample(1:nrow(Boston), size=0.2*nrow(Boston))

# Split the data
Boston_test<-  Boston[row.number,]
dim(Boston_test) ## Size of the testing set
Boston_train<- Boston[-row.number,]
dim(Boston_train)  ## Size of the training set

attach(Boston_train)

par(mfrow=c(2, 2))
boxplot(CRIM, main="CRIM")
boxplot(ZN, main="ZN")
boxplot(INDUS, main="INDUS")
boxplot(NOX, main="NOX")
par(mfrow=c(2, 2))
boxplot(RM, main="RM")
boxplot(AGE, main="AGE")
boxplot(DIS, main="DIS")
boxplot(RAD, main="RAD")
par(mfrow=c(2, 2))
boxplot(TAX, main="TAX")
boxplot(PTRATIO, main="PTRATIO")
boxplot(B, main="B")
boxplot(LSTAT, main="LSTAT")

plot(density(CRIM))
boxplot(log(CRIM))

hist(ZN)
plot(density(log(ZN)))
boxplot(log(ZN+1))
plot(log(MEDV),CRIM)
plot(log(MEDV),log(CRIM))
plot(MEDV,ZN)
plot(MEDV,sqrt(ZN))
cor(MEDV,ZN)
newb<-(log(max(B)+1-B))

fit<-lm(MEDV~CRIM+ZN+RAD+TAX+B+LSTAT+PTRATIO+DIS+AGE+RM+factor(CHAS)+INDUS+NOX-1)

pairs(Boston)


library(ggplot2)

qplot(x=MEDV,y=log(MEDV),data=Boston_train, geom="density",alpha=I(.5), 
      main="MEDV vs Log(MEDV)", xlab="MEDV", 
      ylab="Density")




dat <- data.frame(cond = factor(rep(c("MEDV","LMEDV"), each=405)), 
                  MEDV = c(MEDV, log(MEDV)))



ggplot(dat, aes(x, fill=cond)) + geom_density(alpha=.3)

dat <- data.frame(cond = factor(rep(c("MEDV","Normal"), each=405)), 
                  x = c(Boston_train$MEDV,norm))



ggplot(dat, aes(x, fill=cond)) + geom_density(alpha=.3)

dat <- data.frame(cond = factor(rep(c("LMEDV","Normal"), each=405)), 
                  x = c(log(Boston_train$MEDV),lnorm))



ggplot(dat, aes(x, fill=cond)) + geom_density(alpha=.3)



dat <- data.frame(x = Boston_train$MEDV)



ggplot(dat, aes(x=MEDV)) + geom_density(fill="blueviolet")


logdata<-cbind(Boston_train,LMEDV=log(Boston_train$MEDV))


















initial.model<-lm(log(MEDV)~CRIM+ZN+INDUS+factor(CHAS)+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT, data=Boston_train)
summary(initial.model)


initial.model_age<-lm((MEDV)~CRIM+ZN+AGE+INDUS+factor(CHAS)+NOX+RM+RAD+TAX+PTRATIO+B+LSTAT, data=Boston_train)



# Select rows randomly
row.number<- sample(1:nrow(Boston), size=0.2*nrow(Boston))

# Split the data
Boston_test<-  Boston[row.number,]
write.csv(Boston_test,"Boston_test.csv")
dim(Boston_test) ## Size of the testing set
Boston_train<- Boston[-row.number,]
write.csv(Boston_train,"Boston_train.csv")
dim(Boston_train)  ## Size of the training set

9. Checking for heteroskedasticity:
  
  ```{R}
library(sandwich)
library(lmtest)
vcov<-vcovHC(model_3, omega = NULL, type = "HC4")
# The vcovHC() function creates a variance " covariance matrix for you.
```

The diagonal elements show the variance of each variable with itself. The diagonal values should have been constant, but since they vary, we can detect the presence of heteroskedasticity.
```{R}
#To remove this heteroskedasticity, we use the coeftest() function in R.
coeftest(model_3, df = Inf, vcov)
```


10. Testing our Model
```{R}
RM_Sq1<-(Boston_test$RM)^2
LRAD1<-log(Boston_test$RAD)
LB1<-(log(max(Boston_test$B)+1-Boston_test$B))
model_final<-lm(log(MEDV)~CRIM+ZN+INDUS+factor(CHAS)+NOX+RM+RM_Sq1+AGE+DIS+LRAD1+PTRATIO+LB1+LSTAT, data=Boston_test)
summary(model_final)
library(car)
residualPlots(model_final)
```


We see that our model is good enough. The few non-conformities are due to outliers in the validation set. You can check that yourselves.


So let us, see how our model looks using the complete data.

```{R}
RM_Sq2<-(Boston$RM)^2
LRAD2<-log(Boston$RAD)
LB2<-(log(max(Boston$B)+1-Boston$B))
model<-lm(log(MEDV)~CRIM+ZN+INDUS+factor(CHAS)+NOX+RM+RM_Sq2+AGE+DIS+LRAD2+PTRATIO+LB2+LSTAT, data=Boston)
summary(model)
library(car)
residualPlots(model)
```



