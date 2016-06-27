# Kaggle Competition

## Load the data
train <- read.csv("train2016.csv", na.strings=c("","NA"))

## Subset the data for response rate over 0.9
tr90 <- subset(train, rowSums(!is.na(train[, 8:108]))/101 >= 0.9)

## Impute with MICE
library(Rcpp)
library(mice)
set.seed(888)

tr90_imputed <- complete(mice(tr90))

## Select Important vairables with Boruta
library(ranger)
library(Boruta)
set.seed(888)

boruta.train <- Boruta(Party ~. -USER_ID, data = tr90_imputed, doTrace = 1)

print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
        boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

### Get the Important Variables
getSelectedAttributes(final.boruta, withTentative = F)

## Build the Log model for Imp Vars 0.6627623
LogReg <- glm(Party ~ Q109244 + Q113181 + Q115611 + Q98869
                      + Q98197 + Q119851, 
                        data = tr90_imputed, family = binomial)
summary(LogReg)
table(tr90_imputed$Party, predict(LogReg, type = "response") >= 0.5)


## Build the CART model 0.6622399
library(rpart)
library(rpart.plot)

sim.tr90 = tr90_imputed[,c("USER_ID", "YOB", "Gender", "Income", "HouseholdStatus", 
                     "EducationLevel", "Party", "Q119851", "Q115611", 
                     "Q113181", "Q109244", "Q98197")]


ImpVars.Cart <- rpart(Party ~. -USER_ID, data = sim.tr90, cp = 0.008)
prp(ImpVars.Cart)

library(lattice)
library(ggplot2)
library(caret)
library(e1071)

train(Party ~. -USER_ID, data = sim.tr90, method = "rpart", trControl = trainControl(method = "cv", number = 10), tuneGrid = expand.grid(.cp = seq(0.001, 0.1, 0.001)))

ImpVars.Cart <- rpart(Party ~. -USER_ID, data = tr90_imputed, method = "class", cp = 0.008)
prp(ImpVars.Cart)

predict(ImpVars.Cart)
table(sim.tr90$Party, predict(ImpVars.Cart) >= 0.5)

## Subset the data with Imp Var
tr0 <- subset(train, is.na(train$Q119851))
tr0 <- subset(train, is.na(train$Q115611))
tr0 <- subset(train, is.na(train$Q113181))
tr0 <- subset(train, is.na(train$Q109244))
tr0 <- subset(train, is.na(train$Q98197))
tr0 <- subset(train, is.na(train$Q98869))






