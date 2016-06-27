# Unit 6 Homework - Document Clustering with Daily Kos

setwd("/Users/ElsaW/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 6 Homework")

## 1 Hierarchical Clustering
kos = read.csv("dailykos.csv")
str(kos)
kosDist = dist(kos, method = "euclidean")
kosHierClust = hclust(kosDist, method="ward.D")
plot(kosHierClust)

clustterkos = cutree(kosHierClust, k = 7)
table(clustterkos)

HierCluster = split(kos, clustterkos)
tail(sort(colMeans(HierCluster[[1]])))
tail(sort(colMeans(HierCluster[[2]])))
tail(sort(colMeans(HierCluster[[3]])))
tail(sort(colMeans(HierCluster[[4]])))
tail(sort(colMeans(HierCluster[[5]])))
tail(sort(colMeans(HierCluster[[6]])))
tail(sort(colMeans(HierCluster[[7]])))

## 2 K-Means Clustering

k = 7
set.seed(1000)
KMC = kmeans(kos, centers = k)
kosCluster = KMC$cluster
table(kosCluster)

### or
KmeansCluster = split(kos, kosCluster)

tail(sort(colMeans(KmeansCluster[[1]])))
tail(sort(colMeans(KmeansCluster[[2]])))
tail(sort(colMeans(KmeansCluster[[3]])))
tail(sort(colMeans(KmeansCluster[[4]])))
tail(sort(colMeans(KmeansCluster[[5]])))
tail(sort(colMeans(KmeansCluster[[6]])))
tail(sort(colMeans(KmeansCluster[[7]])))

table(clustterkos, kosCluster)

