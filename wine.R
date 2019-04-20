#Column Names 
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')
#Remove the first attribute as it is just an identifier.
wine.use = wine[,-c(1,1)]
head(wine.use)
pairs(wine.use)
#PCA for Wine Data
pc=princomp(wine.use,cor=TRUE,score=TRUE)
print(pc)
#Bar Chart
plot(pc)
#Elbow curve
plot(pc,type="l")
biplot(pc)

#Cluster Analysis using all chemical measurements
#Standardise the data
medians=apply(wine.use,2,median)

#Find the mean Average Deviation for each column
mads=apply(wine.use,2,mad)

#Now use the scale function to scale the data correctly
wine.Scaled=scale(wine.use,center=medians,scale=mads)
wine.dist=dist(wine.Scaled,method = "euclidean")
wine.hclust=hclust(wine.dist)
plot(wine.hclust)

# Since # components form the mjor proportion lets perform a k means clustering with k=3
results=kmeans(wine.Scaled,3)
results
table(wine$Type,results$cluster)

