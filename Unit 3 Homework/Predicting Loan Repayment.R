# Unit 3 Homework - Predicting Loan Repayment

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 3 Homework")

## 1

loans <- read.csv("loans.csv")
summary(loans)
table(loans$not.fully.paid)
1533/9578

missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
table(missing$not.fully.paid)

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
## or
loans <- read.csv("loans_imputed.csv")


# 2

set.seed(144)
split <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train <- subset(loans, split == T)
test <- subset(loans, split == F)
LoansLog <- glm(not.fully.paid ~ ., data = train, family = binomial)
summary(LoansLog)

-10*-9.406e-03
exp(-9.406e-03 * 700)/exp(-9.406e-03 * 710)

test$predicted.risk <- predict(LoansLog, newdata = test, type="response")
table(test$not.fully.paid, test$predicted.risk >= 0.5)
2403/2873
table(test$not.fully.paid)
2413/2873

library(ROCR)
ROCRpred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

## 3

rateLog <- glm(not.fully.paid ~ int.rate, data = train, family = binomial)
summary(rateLog)

predictbi <- predict(rateLog, newdata = test, type = "response")
max(predictbi)

table(test$not.fully.paid, predictbi >= 0.5)

ROCRpred = prediction(predictbi, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

## 4

10*exp(3*.06)

## 5

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit) * 10

## 6

highInterest <- subset(test, int.rate >= 0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)
110/437

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
