# Unit 4 Homework - State Data Revisted (OPTIONAL)

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 4 Homework")

data(state)
statedata = data.frame(state.x77)
str(statedata)

## 1 Linear Regression
LinReg <- lm(Life.Exp ~., data = statedata)
summary(LinReg)

PredReg <- predict(LinReg)
sum((PredReg - statedata$Life.Exp)^2)
### or
sum(LinReg$residuals^2)

LinReg2 <- lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(LinReg2)

sum((LinReg2$residuals)^2)

## 2 CART Model
library(rpart)
library(rpart.plot)

CARTModel <- rpart(Life.Exp ~., data = statedata)
prp(CARTModel)

PredCART <- predict(CARTModel)
sum((PredCART - statedata$Life.Exp)^2)

CARTModel2 <- rpart(Life.Exp ~., data = statedata, minbucket = 5)
prp(CARTModel2)

PredCART2 <- predict(CARTModel2)
sum((PredCART2 - statedata$Life.Exp)^2)

CARTModel3 <- rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
prp(CARTModel3)
PredCART3 <- predict(CARTModel3)
sum((PredCART3 - statedata$Life.Exp)^2)

## 3 Cross-validation
library(caret)
set.seed(111)
TC <- trainControl(method = "cv", number = 10)
G <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(Life.Exp ~., data = statedata, method = "rpart", trControl = TC, tuneGrid = G)

CARTModel4 <- rpart(Life.Exp ~. , data = statedata, cp = 0.12)
prp(CARTModel4)

PredCART4 <- predict(CARTModel4)
sum((PredCART4 - statedata$Life.Exp)^2)

set.seed(111)
cartControl <- trainControl(method = "cv", number = 10)
cartGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = cartControl, tuneGrid = cartGrid)

CARTModel5 <- rpart(Life.Exp ~ Area , data = statedata, cp = 0.02)
prp(CARTModel5)

PredCart5 <- predict(CARTModel5)
sum((PredCart5 - statedata$Life.Exp)^2)
