#Change the location

Sys.setlocale("LC_ALL", "C")

getwd()

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge")

# Unit 1 
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


