setwd("D:/LKD_NPCI_CRASHED DATA 29 NOV 2016/D Drive/My Desktop/ISB 30 Nov Apt Form/STUDY ISB FOR ALL RESIDENCIES1/SA 3/Data Sets")
salespop=read.table("salespop.txt", header = TRUE)
salespoplm=lm(sales~.,data = salespop)
summary(salespoplm)
dim(salespop)
str(salespop)
class(salespop)

#covariance matrix
cov(salespop)
cov(salespop,use="complete.obs")

#Mean substitution or hot deck imputation
library(HotDeckImputation)
require(HotDeckImputation)
salespopk=as.matrix(salespop)
impute.mean(DATA = salespopk)
impute.mean(data=NULL,data=salespopk)

#Data file Fitness
setwd("D:/LKD_NPCI_CRASHED DATA 29 NOV 2016/D Drive/My Desktop/ISB 30 Nov Apt Form/STUDY ISB FOR ALL RESIDENCIES1/SA 3/Data Sets")
fitness=read.table("fitness.txt",header = TRUE)
require(HotDeckImputation)
fitness1=as.matrix(fitness)
index = which(fitness1[,2] == ".")
index1 = which(fitness1[,3] == ".")
fitness1[index,2] = NA
fitness1[index1,3] = NA

impute.NN_HD(DATA=as.data.frame(fitness1),distance = "man")
#something is wrong above

#Regression based imputation
x2onx1=lm(RunTime~Oxygen,data=fitness)
new=data.frame(Oxygen=c(59.571,50.541,47.273))
predict(x2onx1,new)
x3onx1=lm(RunPulse~Oxygen,data=fitness)

#something is wrong above

library(yaImpute)
setwd("D:/LKD_NPCI_CRASHED DATA 29 NOV 2016/D Drive/My Desktop/ISB 30 Nov Apt Form/STUDY ISB FOR ALL RESIDENCIES1/SA 3/Data Sets")
fitness=read.table("fitness.txt",header = TRUE)
set.seed(1)
refs=sample(rownames(fitness),c(1,2,3,6,7,9,10,12,13,15,16,17,20:24))
x=as.matrix(fitness[,1])
y=fitness[,2:3]
raw=yai(x=x,y=y,method="euclidean")
plot(raw)
tail(impute(raw))
