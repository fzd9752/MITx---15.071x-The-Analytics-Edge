# Unit 4 Homework - Predicting Earnings from Census Data

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 4 Homework")

census <- read.csv("census.csv")

## 1 Logistic Regression
library(caTools)
set.seed(2000)
spl <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, spl == T)
test <- subset(census, spl == F)

Log <- glm(over50k ~., data = train, family = binomial)
summary(Log)

predLog <- predict(Log, newdata = test, type = "response")
table(test$over50k, predLog > 0.5)
(1888+9051)/nrow(test)

table(test$over50k)
9713/12791

library(ROCR)
ROCLog <- prediction(predLog, test$over50k)
as.numeric(performance(ROCLog,"auc")@y.values)


## 2 CART
library(rpart)
library(rpart.plot)
CARTMod <- rpart(over50k ~., method = "class", data = train)
prp(CARTMod)

PredCART <- predict(CARTMod, type = "class", newdata = test)
table(test$over50k, PredCART)
(9243+1596)/nrow(test)

PredCART2 <- predict(CARTMod, newdata = test)
ROCCART <- prediction(PredCART2[,2], test$over50k)

plot (performance(ROCCART, "tpr", "fpr") , colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))

## 3 Random Forest
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

library(randomForest)
set.seed(1)
ForestMod <- randomForest(over50k ~., data = trainSmall, method = "class")
predForest <- predict(ForestMod, newdata = test, type = "class")
table(test$over50k, predForest)
(9585+1092)/nrow(test)

vu = varUsed(ForestMod, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(ForestMod$forest$xlevels[vusorted$ix]))

varImpPlot(ForestMod)

## 4 Cross-Validation
library (caret)
library (e1071)
set.seed(2)
tr.control = trainControl(method = "cv", number = 10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
tr = train(over50k ~., data = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)

CartNew <- rpart(over50k ~., data = train, method = "class", cp = 0.002)
predictionnew <- predict(CartNew, newdata = test, type = "class")
table(test$over50k, predictionnew)
(1838+9178)/nrow(test)

prp(CartNew)

