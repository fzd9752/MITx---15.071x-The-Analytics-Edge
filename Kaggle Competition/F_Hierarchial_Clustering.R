# Kaggle Hierarchial Clustering
numtrian = as.vector(numtrain)

trdistance = dist(numtrain, method = "euclidean")
Hiercluster =hclust(trdistance, method = "ward.D")

plot(Hiercluster)

traincluster = cutree(Hiercluster, k = 3)
traincluster

cluster1 <- subset(numtrain, traincluster == 1)        
head(cluster1)        
cluster1 <- as.data.frame(cluster1)
summary(cluster1$Party)

cluster2 <- subset(numtrain, traincluster == 2)        
head(cluster2)        
cluster2 <- as.data.frame(cluster2)
summary(cluster2$Party)

cluster3 <- subset(numtrain, traincluster == 3)        
head(cluster3)        
cluster3 <- as.data.frame(cluster3)
summary(cluster3$Party)
