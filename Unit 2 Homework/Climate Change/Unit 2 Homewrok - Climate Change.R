# Unit 2 Homework - CLIMATE CHANGE

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 2 Homework/Climate Change")

## Problem 1

climate <- read.csv("climate_change.csv")
str(climate)
train <- subset(climate, Year <= 2006)
test <- subset(climate, Year > 2006)

model1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
summary(model1)

## P2

cor(train)["N2O",]
cor(train)["CFC.11",]

## P3

model2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data = train)
summary(model2)

## P4 step()

simplemodel <- step(model1)
summary(simplemodel)

## P5

predictTemp <- predict(simplemodel, newdata = test)
SSE <- sum((predictTemp - test$Temp)^2)
SST <- sum((mean(train$Temp) - test$Temp)^2)
R2 <- 1 - SSE/SST
R2
