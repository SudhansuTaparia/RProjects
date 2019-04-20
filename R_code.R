#load data

# Select rows randomly
row.number=sample(1:nrow(Airfares), size=0.2*nrow(Airfares))

# Split the data
test=Airfares[row.number,]
dim(test) ## Size of the testing set
train=Airfares[-row.number,]
dim(train)  ## Size of the training set
attach(train)

attributes(train) #airfare is a data frame
summary(train)
train=train[,-19]     #Removing last column
summary(train)
#finding class of each variable
sapply(train,class)

#FARE=as.numeric(FARE) #conerts fare into numeric data
E_INCOME=as.numeric(E_INCOME) 
S_INCOME=as.numeric(S_INCOME)
HI=as.numeric(HI)
boxplot(FARE,main="FARE")
boxplot(log(FARE))

par(mfrow=c(3, 3))  #dividing screen in 3 rows and 3 columns
boxplot(COUPON, main="COUPON")
boxplot(NEW, main="NEW")
boxplot(HI, main="HI")
boxplot(S_INCOME, main="S_INCOME")
boxplot(E_INCOME, main="E_INCOME")
boxplot(S_POP, main="S_POP")
boxplot(E_POP, main="E_POP")
boxplot(DISTANCE, main="DISTANCE")
boxplot(PAX, main="PAX")


#visualising fators individually
par(mfrow=c(2, 3))
class(S_CODE)
S_CODE1=unclass(S_CODE)
nlevels(S_CODE1)
S_CODE1
boxplot(S_CODE1,main="S_CODE1")

class(E_CODE)
E_CODE1=unclass(E_CODE)
nlevels(ECODE1)
E_CODE
boxplot(E_CODE1,main="E_CODE1")

class(E_CITY)
E_CITY1=unclass(E_CITY)
nlevels(E_CITY1)
E_CITY
boxplot(E_CITY1,main="E_CITY1")

class(S_CITY)
S_CITY1=unclass(S_CITY)
nlevels(S_CITY)
S_CITY1
boxplot(S_CITY1,main="S_CITY1")

class(SLOT)
SLOT1=unclass(SLOT)
nlevels(SLOT)
SLOT
boxplot(SLOT1,main="SLOT1")


#visualising categorical variables wrt FARE
plot(E_CODE,FARE,main="E_CODE_vS_FARE")
plot(E_CITY,FARE,main="E_CITY_vS_FARE")
plot(S_CODE,FARE,main="S_CODE_vS_FARE")
plot(E_CODE,FARE,main="E_CODE_vS_FARE")

#checking for correlation
cor(E_INCOME,FARE)
cor(S_INCOME,FARE)
cor(S_CITY1,FARE)
cor(E_CITY1,FARE)
cor(PAX,FARE)
cor(E_POP,FARE)
cor(S_POP,FARE)


#simple linear model
model1=lm(FARE~S_CODE+S_CITY+E_CODE+E_CITY+COUPON+NEW+VACATION+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX,data=train)
summary(model1)
plot(model1)


#dropping PAX as low corelation and taking log of FARE
model2=lm(log(FARE)~S_CODE+S_CITY+E_CODE+E_CITY+COUPON+NEW+VACATION+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE)
par(mfrow=c(1, 1))
plot(model2)
 
#model when all conerted into numeric levels by using unclass command
model3=lm(FARE~S_CODE1+S_CITY1+E_CODE1+E_CITY1+COUPON+NEW+VACATION+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX)
summary(model3)
plot(model3)

#model when removing S_CITY, E_CITY, PAX
model4=lm(FARE~S_CODE+E_CODE+COUPON+NEW+VACATION+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE,data=train)
summary(model4)
plot(model4)


#testing on 20% of data (model1)
summary(test)
test=test[,-19]
modeltest=lm(FARE~S_CODE+S_CITY+E_CODE+E_CITY+COUPON+NEW+VACATION+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX,data=train)
summary(modeltest)
plot(modeltest)

#testing on entire dataset (model1)
model_fulldata=lm(FARE~S_CODE+S_CITY+E_CODE+E_CITY+COUPON+NEW+VACATION+S_INCOME+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+PAX,data=Airfares)
summary(model_fulldata)
plot(model_fulldata)

