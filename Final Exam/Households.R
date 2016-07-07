# The Analytics Edge - Final Exam 2

setwd("/Users/ElsaW/Documents/MOOC/MOOC_Completed/MITx - 15.071x The Analytics Edge/Final Exam")

households <- read.csv("Households.csv")
head(households)

households[households$MorningPct == 100, ]
nrow(households[households$AfternoonPct == 100, ])

min(households$AvgDiscount[households$AvgSalesValue >= 150])
min(households$AvgSalesValue[households$AvgDiscount >= 25])
table(households$NumVisits >= 300)
148/nrow(households)

summary(households)

library(caret)
preproc = preProcess(households)
HouseholdsNorm = predict(preproc, households)
head(HouseholdsNorm)

summary(HouseholdsNorm)

set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)



library(grid)
library(modeltools)
library(stats4)
library (flexclust)

set.seed(200)
K <- kmeans (HouseholdsNorm, centers = 10)
str(K)

which.max(K$centers[, "MorningPct"])
K$centers[, c("AvgDiscount", "AvgSalesValue")]
K$centers[, c("NumVisits", "AvgSalesValue")]


set.seed(5000)
K2 <- kmeans (HouseholdsNorm, centers = 5)

K2$size
K2$centers