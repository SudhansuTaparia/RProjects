#install package MASS
install.packages("MASS")
#Library for MASS
library(MASS)
#Load the file
ship=read.csv("C:\\Users\\sutapari\\Downloads\\ships.csv")
#Check the first few rows
head(ship)
#Remove the first columns as its just an  identifier
ship.use = ship[,-c(1,1)]
#check the head of the data without identifier
head(ship.use)
#summary of data
summary(ship.use)
#Apply GLM to form the model
incidentsglm=glm(incidents~.,family="poisson",data=ship.use)
#summary of the model
summary(incidentsglm)


install.packages("ResourceSelection")


library(ResourceSelection)
hoslem.test(ship.use$incidents, fitted(incidentsglm))


