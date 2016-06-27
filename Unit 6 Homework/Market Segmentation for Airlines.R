# Unit 6 Homework - Market Segmentation for Airlines

setwd("/Users/ElsaW/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 6 Homework")

## 1 Normalizing the Data

airlines = read.csv("AirlinesCluster.csv")
summary(airlines)

library(lattice)
library(ggplot2)
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

## 2 Hierarchical Clustering
airlinesDist = dist(airlinesNorm, method = "euclidean")
airlinesHierCluster = hclust(airlinesDist, method = "ward.D")
plot(airlinesHierCluster)
rect.hclust(airlinesHierCluster, k = 6, border = "red")

clusterGroups = cutree(airlinesHierCluster, k = 5)
table(clusterGroups)

HierCluster = split(airlines, clusterGroups)

summary(HierCluster[[1]])

tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)

colMeans(subset(airlines, clusterGroups == 1))
colMeans(subset(airlines, clusterGroups == 2))
colMeans(subset(airlines, clusterGroups == 3))
colMeans(subset(airlines, clusterGroups == 4))
colMeans(subset(airlines, clusterGroups == 5))

lapply(split(airlines, clusterGroups), colMeans)

## 3 K-Means Clustering
k = 5
set.seed(88)
KMC = kmeans(airlinesNorm, centers = k, iter.max = 1000)
table(KMC$cluster)

KmeansCluster = split(airlines, KMC$cluster)
table(KMC$cluster, clusterGroups)
KMC$centers
colMeans(subset(airlinesNorm, clusterGroups == 2))
