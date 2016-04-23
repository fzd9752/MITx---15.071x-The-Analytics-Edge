# Unit 2 Homework - Detecting Flu Epidemics via Search Engine Query Data

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 2 Homework/Detecting Flu Epidemics via Search Engine Query Data")
library(ggplot2)

## P1

FluTrain <- read.csv("FluTrain.csv")
str(FluTrain)
plot(FluTrain$Week, FluTrain$ILI)
plot(FluTrain$Week, FluTrain$Queries)
FluTrain$Week[which.max(FluTrain$ILI)]
FluTrain$Week[which.max(FluTrain$Queries)]

hist(FluTrain$ILI)

plot(FluTrain$Queries, log(FluTrain$ILI))

## P2

FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

exp(0.5 * cor(FluTrain$Queries, log(FluTrain$ILI)))

cor(FluTrain$Queries, log(FluTrain$ILI))^2

## P3

FluTest <- read.csv("FluTest.csv")

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
FluTest$Week
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]  

(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

sqrt(sum((FluTest$ILI - PredTest1)^2)/nrow(FluTest))

## !!!
sqrt(mean((PredTest1-FluTest$ILI)^2))

## P4

install.packages("zoo")

library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)

hist(FluTrain$ILILag2)
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

## 5

FluTest$ILILag2 = coredata(lag(zoo(FluTest$ILI), -2, na.pad=TRUE))
summary(FluTest)

nrow(FluTrain)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata = FluTest))

sqrt(mean((PredTest2 - FluTest$ILI)^2))
