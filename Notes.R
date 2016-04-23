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

