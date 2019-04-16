## Set the working directory to where the Excel file is
## Session > Set Working Directory > Choose Directory > Select the directory where the Excel file is

getwd()
dir()

## Detailed reference: 
## http://www.statmethods.net/advstats/cluster.html

input <- read.csv("Universities_Clustering.csv",header=TRUE)
dim(input)

mydata <- input[1:25,3:8]
normalized_data <- scale(mydata)

## Hierarchical Clustering

d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")
plot(fit) # display dendrogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
rect.hclust(fit, k=3, border="red")
membership<-as.matrix(groups)
membership

## K-means clustering

fit <- kmeans(normalized_data, 3) # 3 cluster solution
fit
mydata<- data.frame(mydata, fit$cluster) # append cluster membership
mydata
aggregate(mydata, by=list(fit$cluster), FUN=mean) 

## Other APIs for calculating distances
?dist
?daisy # it is available in "cluster" package; insatll and load the package first
library(cluster)
?daisy
