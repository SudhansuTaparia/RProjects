## Set the working directory to where the Excel file is
## Session > Set Working Directory > Choose Directory > Select the directory where the Excel file is

getwd()
dir()

## Principal Component Analysis
## Reference: http://www.statmethods.net/advstats/factor.html

input <- read.csv("University Ranking.csv",header=TRUE)
dim(input)

mydata <- input[1:25,3:8]
head(mydata)

help(princomp) ## to understand the api for princomp
help(prcomp)
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different; why?

pcaObj<-princomp(mydata, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)
loadings(pcaObj)
plot(pcaObj)
biplot(pcaObj)
pcaObj$loadings
pcaObj$scores
