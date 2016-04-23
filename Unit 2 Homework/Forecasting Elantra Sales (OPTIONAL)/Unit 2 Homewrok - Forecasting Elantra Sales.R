# Unit 2 Homework - Forecasting Elantra Sales (OPTIONAL)

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 2 Homework/Forecasting Elantra Sales (OPTIONAL)")

## P1

elantra <- read.csv("elantra.csv")

elantraTrain <- subset(elantra, Year <= 2012)
elantraTest<- subset(elantra, Year > 2012)
str(elantraTrain)

## P2

ElantraLM <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(ElantraLM)

## P3

ElantraLM2 <- lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(ElantraLM2)

110.69 * 2
110.69 * 4

## P4

elantraTrain$MonthFactor = as.factor(elantraTrain$Month)
elantraTest$MonthFactor = as.factor(elantraTest$Month)
ElantraLM3 <- lm(ElantraSales ~ MonthFactor + Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(ElantraLM3)

## P5

cor(elantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

## P6

ElantraLM4 <- lm(ElantraSales ~ MonthFactor + Unemployment + CPI_all + CPI_energy, data = elantraTrain)
summary(ElantraLM4)

ElantraPre <- predict(ElantraLM4, newdata = elantraTest)
sum((ElantraPre - elantraTest$ElantraSales)^2)

mean(elantraTrain$ElantraSales)

SSE = sum((ElantraPre - elantraTest$ElantraSales)^2)
SST = sum((elantraTest$ElantraSales - mean(elantraTrain$ElantraSales))^2)
1-SSE/SST

sort(abs(ElantraPre - elantraTest$ElantraSales))
which.max(abs(ElantraPre - elantraTest$ElantraSales))

elantraTest[5, ]
