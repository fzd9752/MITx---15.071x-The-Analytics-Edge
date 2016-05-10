# Unit 3 Homework - Predicting Parole Violators

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 3 Homework")

## 1

parole <- read.csv("parole.csv")
str(parole)

table(parole$violator)

## 2

summary(parole)

parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole)
table(parole$state)
str(parole$crime)

## 3

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

## 4

violatorLog <- glm(violator ~ ., data = train, family = binomial)
summary(violatorLog)

exp(1.61)

-4.2411574 + 0.3869904 + 0.8867192 + 50*-0.0001756 + 3*-0.1238867 + 12*0.0802954 + 0.6837143
exp(-1.700629)
1/(1+exp(1.700629))

## 5

Testpred <- predict(violatorLog, newdata = test, type = "response")
max(Testpred)

table(test$violator, Testpred >= 0.5)
12/23
167/179
(12+167)/nrow(test)

table(test$violator)
179/202





