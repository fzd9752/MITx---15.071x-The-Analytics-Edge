# Unit 2 Homework - Reading Test Scores

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 2 Homework/Reading Test Scores")

## P1

pisaTrain <- read.csv("pisa2009train.csv")
pisaTest<- read.csv("pisa2009test.csv")

str(pisaTrain)

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

summary(pisaTrain)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
nrow(pisaTest)

## P2

factor(pisaTrain$grade)

## P3

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore <- lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

SST <- sum((pisaTrain$readingScore - mean(pisaTrain$readingScore))^2)
SSE <- (1 - 0.3251) * SST

SSE = sum(lmScore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))
RMSE

sqrt(mean(lmScore$residuals^2))

29.542707*2


## P4

predTest <- predict(lmScore, newdata = pisaTest)
summary(predTest)
summary(predTest)[1] - summary(predTest)[6]

sum((pisaTest$readingScore - predTest)^2)
sqrt(SSE/nrow(pisaTest))
sqrt(mean((pisaTest$readingScore - predTest)^2))

mean(pisaTrain$readingScore)
sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)

1 - sum((pisaTest$readingScore - predTest)^2)/sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
