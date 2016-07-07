# The Analytics Edge - Final Exam 1

setwd("/Users/ElsaW/Documents/MOOC/MOOC_Completed/MITx - 15.071x The Analytics Edge/Final Exam")

fedFunds <- read.csv("federalFundsRate.csv", stringsAsFactors=FALSE)

str(fedFunds)
mean(fedFunds$RaisedFedFunds)

table(fedFunds$Chairman, fedFunds$RaisedFedFunds)

fedFunds$Chairman <- as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres <- as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds <- as.factor(fedFunds$RaisedFedFunds)



library(caTools)
set.seed(201)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

training <- subset(fedFunds, spl == T)
testing <- subset(fedFunds, spl == F)

mod1 <- glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, family="binomial")
summary(mod1)

a <- 9.121012 + (-0.003427 * 1.7) + (-3*0.157658) + (-0.047449*5.1) + (-0.136451 * 65.3)  + (-0.006931 * 18)
1/(1+exp(-a))

odds <- exp(0.347829)
P1 = odds * P0

pred <- predict(mod1, newdata = testing, type="response")
head(pred)

table(training$RaisedFedFunds)
table(testing$RaisedFedFunds, pred >= 0.5)

library(gplots)
library(ROCR)

plot(performance(prediction(pred, testing$RaisedFedFunds),"tpr", "fpr"), colorize = T)
as.numeric(performance(prediction(pred, testing$RaisedFedFunds), "auc")@y.values)

library(lattice)
library(ggplot2)
library(caret)
library(e1071)

set.seed(201)
trC <- trainControl(method = "cv", number = 10)
Gr <- expand.grid(.cp = seq(0.001, 0.05, 0.001))
train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "rpart", trControl = trC, tuneGrid = Gr)
    
library(rpart)
library(rpart.plot)

cart <- rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "class", cp = 0.016)
prp(cart)

predcart <- predict(cart, newdata = testing, type = "class")
table(predcart, testing$RaisedFedFunds)
(48+64)/nrow(testing)
