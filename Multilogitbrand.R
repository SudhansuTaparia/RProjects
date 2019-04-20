
setwd("D:/LKD_NPCI_CRASHED DATA 29 NOV 2016/D Drive/My Desktop/ISB 30 Nov Apt Form/STUDY ISB FOR ALL RESIDENCIES1/SA 3/Data Sets")
brandchoice=read.table("multilogitbrand.txt", header = TRUE)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
brandmulti=multinom(brand~gender+age,data=brandchoice)
summary(brandmulti)
multinom(formula=brand~gender+age,data=brandchoice)
exp(coef(brandmulti))
z=summary(brandmulti)$coefficients/summary(brandmulti)$standard.errors
z
p=(1-pnorm(abs(z),0,1))*2
p
pp=fitted(brandmulti)
pp[c(1:3,101:103,701:703),]
k=pp[c(1:733),]
write.csv(k,"brand.csv")
brandmulti2=multinom(brand~gender+age+gender*age,data=brandchoice)
summary(brandmulti2)
zzzp2=fitted(brandmulti2)
p2
exp(0.5238197)
 