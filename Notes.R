#Change the location

Sys.setlocale("LC_ALL", "C")

getwd()

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge")

# Unit 1 

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 1")

mean(WHO$Over60)

which.min(WHO$Over60)

WHO$Country[183]

which.max(WHO$LiteracyRate)

WHO$Country[44]

str(WHO)

tapply(WHO$ChildMortality, WHO$Region, mean)

which.min(tapply(WHO$ChildMortality, WHO$Region, mean))

## which.max(na.rm)?

max(WHO$LiteracyRate, na.rm = TRUE)

which(WHO$LiteracyRate == max(WHO$LiteracyRate, na.rm = TRUE))

## Clear the Console
Control + L

# Unit 2
setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 2")

## Predicting the Quality of Wine

wine <- read.csv("wine.csv")
str(wine)

modelQQ4 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(modelQQ4)

cor(wine$HarvestRain, wine$WinterRain)

wineTest <- read.csv("wine_test.csv")

## Moneyball

baseball <- read.csv("baseball.csv")

moneyball <- subset(baseball, Year < 2002)
moneyball$RD <- moneyball$RS - moneyball$RA

plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

80.881375 + 99*0.105766

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

-804.63 + 2737.77*0.311 + 1584.91*0.405
-837.38 + 2913.60*0.297 + 1514.29*0.370

-804.63 + 2737.77*0.338 + 1584.91*0.540
-804.63 + 2737.77*0.391 + 1584.91*0.450
-804.63 + 2737.77*0.369 + 1584.91*0.374
-804.63 + 2737.77*0.313 + 1584.91*0.447
-804.63 + 2737.77*0.361 + 1584.91*0.500

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank, wins2012)
cor(teamRank, wins2013)

# Unit 3 

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 3 Logistic Regression")

exp(-1)
1/(1+exp(1))

#Change the location

Sys.setlocale("LC_ALL", "C")

getwd()

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge")

# Unit 1 

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 1")

mean(WHO$Over60)

which.min(WHO$Over60)

WHO$Country[183]

which.max(WHO$LiteracyRate)

WHO$Country[44]

str(WHO)

tapply(WHO$ChildMortality, WHO$Region, mean)

which.min(tapply(WHO$ChildMortality, WHO$Region, mean))

## which.max(na.rm)?

max(WHO$LiteracyRate, na.rm = TRUE)

which(WHO$LiteracyRate == max(WHO$LiteracyRate, na.rm = TRUE))

## Clear the Console
Control + L

# Unit 2
setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 2")

## Predicting the Quality of Wine

wine <- read.csv("wine.csv")
str(wine)

modelQQ4 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(modelQQ4)

cor(wine$HarvestRain, wine$WinterRain)

wineTest <- read.csv("wine_test.csv")

## Moneyball

baseball <- read.csv("baseball.csv")

moneyball <- subset(baseball, Year < 2002)
moneyball$RD <- moneyball$RS - moneyball$RA

plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

80.881375 + 99*0.105766

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

-804.63 + 2737.77*0.311 + 1584.91*0.405
-837.38 + 2913.60*0.297 + 1514.29*0.370

-804.63 + 2737.77*0.338 + 1584.91*0.540
-804.63 + 2737.77*0.391 + 1584.91*0.450
-804.63 + 2737.77*0.369 + 1584.91*0.374
-804.63 + 2737.77*0.313 + 1584.91*0.447
-804.63 + 2737.77*0.361 + 1584.91*0.500

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank, wins2012)
cor(teamRank, wins2013)

# Unit 3 

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 3 Logistic Regression")

## Expert
exp(-1)
1/(1+exp(1))

quality = read.csv("quality.csv")

install.packages("caTools")

library(caTools)

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

spl = sample(1:nrow(data), size=0.7 * nrow(data))
train = data[spl,]
test = data[-spl,]

QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)

summary(QualityLog2)

20/25
15/25

10/25

install.packages("ROCR")
library(gplots)
library(ROCR)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)


## Heart Study

framingham <- read.csv("framingham.csv")
str(framingham)
set.seed(2000)

split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train <- subset(framingham, split == T)
test <- subset(framingham, split == F)

framinghamLog <- glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

11/(198)
1069/1075

library(Rcpp)
library(mice)

