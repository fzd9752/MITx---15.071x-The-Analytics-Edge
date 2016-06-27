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

# Unit 4 Tree

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 4 Trees")

## The Supreme Court

library(caTools)
library(rpart)
library(rpart.plot)

stevens = read.csv("stevens.csv")
str(stevens)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)
prp(StevensTree)

PredictCART = predict(StevensTree, newdata = Test, type = "class")

library(gplots)
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)


StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)
prp(StevensTree2)

StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)
prp(StevensTree3)

install.packages("randomForest")
library(randomForest)

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
set.seed(100)
StevensForest1 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
predictSteven1 <- predict(StevensForest1, newdata = Test)
table(Test$Reverse, predictSteven1)
nrow(Test)
(43+74)/170

set.seed(200)
StevensForest2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
predictSteven2 <- predict(StevensForest1, newdata = Test)
table(Test$Reverse, predictSteven2)

library(lattice)
library(ggplot2)
library(caret)
library(e1071)

StevensCART <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", cp = 0.18)
prp(StevensCART)


## D2Hawkeye

### Video 6
Claims <- read.csv("ClaimsData.csv")
str(Claims)
summary(Claims$reimbursement2008)
table(Claims$bucket2009)/nrow(Claims)
library(caTools)
set.seed(88)
spl <- sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain <- subset(Claims, spl == T)
ClaimsTest <- subset(Claims, spl == F)

mean(ClaimsTrain$age)
table(ClaimsTrain$diabetes)/nrow(ClaimsTrain)

### Video 7
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
(110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest)

PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)

table(ClaimsTest$bucket2009)/nrow(ClaimsTest)

nrow(ClaimsTest)
sum(table(ClaimsTest$bucket2009)*PenaltyMatrix[,1])/nrow(ClaimsTest)

### Video 8
library(rpart)
library(rpart.plot)
ClaimsTree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", cp = 0.00005, parms = list(loss = PenaltyMatrix))
prp(ClaimsTree)

PredictTest <- predict(ClaimsTree, newdata = ClaimsTest, type = "class")
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
