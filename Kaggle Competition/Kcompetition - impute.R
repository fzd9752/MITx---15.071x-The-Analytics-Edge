#KCompetition - imputing data

setwd("/Users/ElsaW/Documents/MOOC/1.MITx - 15.071x The Analytics Edge/Kaggle Competition")

## Load the files
train = read.csv("train2016.csv", na.strings=c("","NA"))
test = read.csv("test2016.csv", na.strings=c("","NA"))

## Impute by mice
library(Rcpp)
library(mice)
set.seed(888)
train_imputed = complete(mice(train))
test_imputed = complete(mice(test))

## Split for training and testing
library(caTools)
set.seed(888)
spl = sample.split(train_imputed, 0.7)

tr = subset(train_imputed, spl == T)
te = subset(train_imputed, spl == F)


