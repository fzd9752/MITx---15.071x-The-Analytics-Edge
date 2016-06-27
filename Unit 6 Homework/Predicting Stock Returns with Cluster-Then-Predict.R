# Unit 6 Homework - Predicting Stock Returns with Cluster-Then-Predict

setwd("/Users/ElsaW/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 6 Homework")

## 1 Exploring the Dataset

stocks = read.csv("StocksCluster.csv")
str(stocks)

mean(stocks$PositiveDec)

sort(cor(stocks[,1:11]))

summary(stocks)

## 2 Initial Logistic Regression Model

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~., data = stocksTrain, family = binomial)
table(stocksTrain$PositiveDec, predict(StocksModel, type = "response") >= 0.5)
(990+3640)/nrow(stocksTrain)

table(stocksTest$PositiveDec, predict(StocksModel, newdata = stocksTest, type = "response") >= 0.5)
(417+1553)/nrow(stocksTest)

mean(stocksTest$PositiveDec)

## 3 Clustering Stocks

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTest)
summary(normTrain)

k = 3
set.seed(144)
km = kmeans(normTrain, centers = k)
table(km$cluster)


library(grid)
library(modeltools)
library(stats4)
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

## 4 Cluster-Specific Predictions

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

tapply(stocksTrain$PositiveDec, clusterTrain, mean)

StocksModel1 = glm(PositiveDec ~., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~., data = stocksTrain3, family = binomial)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")

table(stocksTest1$PositiveDec, PredictTest1 >= 0.5)
(774+30)/nrow(stocksTest1)
table(stocksTest2$PositiveDec, PredictTest2 >= 0.5)
(757+388)/nrow(stocksTest2)
table(stocksTest3$PositiveDec, PredictTest3 >= 0.5)
(13+49)/nrow(stocksTest3)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions >= 0.5)
(1544+467)/(467+1110+353+1544)
