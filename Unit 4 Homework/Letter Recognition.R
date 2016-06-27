# Unit 4 Homework - Letter Recognition

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 4 Homework")

letters <- read.csv("letters_ABPR.csv")

## 1 Predicting B or Not B
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
spl <- sample.split(letters$isB, SplitRatio = 0.5)
train <- subset(letters, spl == T)
test <- subset(letters, spl == F)

table(test$isB)
1175/nrow(test)

library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method="class")

predb <- predict(CARTb, newdata = test, type = "class")
table(test$isB, predb)
(1135+313)/nrow(test)

library(ggplot2)
library(randomForest)

forestmodel <- randomForest(isB ~. -letter, data = train, type = "class")
predForest <- predict(forestmodel, newdata = test, type = "class")
table(test$isB, predForest)
(1167+361)/nrow(test)

## 2 Predicting the letters A, B, P, R
letters$letter = as.factor( letters$letter )

set.seed(2000)
spl2 <- sample.split(letters$letter, SplitRatio = 0.5)
train2 <- subset(letters, spl2 == T)
test2 <- subset(letters, spl2 == F)

table(test2$letter)
404/nrow(test2)

letterMod <- rpart(letter ~. -isB, data = train2, method = "class")
predMult <- predict(letterMod, newdata = test2, type = "class")
table(test2$letter, predMult)
(348+318+363+340)/nrow(test2)

set.seed(1000)
letterForest <- randomForest(letter ~. -isB, data = train2, method = "class")
predLetter <- predict(letterForest, newdata = test2, type = "class")
table(test2$letter, predLetter)
(390+380+393+364)/nrow(test2)
