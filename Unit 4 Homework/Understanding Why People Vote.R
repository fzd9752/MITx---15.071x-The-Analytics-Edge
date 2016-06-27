# Unit 4 Homework - Understanding Why People Vote

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 4 Homework")

gerber <- read.csv("gerber.csv")

## 1 Logistic Regression
table(gerber$voting)/nrow(gerber)

tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

gerberLog <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)
summary(gerberLog)

predLog <- predict(gerberLog, type = "response")
table(gerber$voting, predLog > 0.3)
(51966 + 134513)/nrow(gerber)

table(gerber$voting, predLog > 0.5)
235388/nrow(gerber)

library(ROCR)
predROC <- prediction(predLog, gerber$voting)
as.numeric(performance(predROC, "auc")@y.values)


## 2 Trees
library(rpart)
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)


CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

## 3 Interaction Terms
CARTModel4 <- rpart(voting ~ control, data = gerber, cp = 0.0)
prp(CARTModel4, digits = 6)
abs(0.296638 - 0.34)

CARTModel5 <- rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(CARTModel5, digits = 6)
0.334176-0.290456
0.345818-0.302795

LogSexControl <- glm(voting ~ control + sex, data = gerber, family = binomial)
summary(LogSexControl)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogSexControl, newdata=Possibilities, type="response")
abs(0.2908065-0.290456)

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558-0.290456)
