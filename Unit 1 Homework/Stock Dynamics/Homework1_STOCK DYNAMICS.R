# Homework 1 - STOCK DYNAMICS

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 1 Homework/Stock Dynamics")


IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
Boeing <- read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

# Problem 1

summary(Boeing)

summary(IBM)

summary (GE)

summary(CocaCola)

median(Boeing$StockPrice)

sd(ProcterGamble$StockPrice)

# Problem 2

plot(CocaCola$Date, CocaCola$StockPrice, type = 'l', col = 'green')
CocaCola[which.max(CocaCola$StockPrice), ]
CocaCola[which.min(CocaCola$StockPrice), ]

## adding a line
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = 'red', lty = 2)
abline(v=as.Date(c("2000-03-01")), lwd=2)

abline(v=as.Date(c("1983-01-01")), lwd=1, col = 'blue')
abline(v=as.Date(c("1983-12-01")), lwd=1, col = 'blue')

# Problem 3

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="blue")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="orange")

## line type: lty=2 will make the line dashed, lty=3 will make the line dotted, 
##lty=4 will make the line alternate between dashes and dots, and lty=5 will make 
##the line long-dashed

abline(v=as.Date(c("2000-03-01")) , lty = 2)

abline(v=as.Date(c("1997-09-01")) , lty = 3)
abline(v=as.Date(c("1997-11-01")) , lty = 4)

# Problem 4

tapply(IBM$StockPrice, months(IBM$Date), mean) > mean(IBM$StockPrice)

which.max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))
which.max(tapply(GE$StockPrice, months(GE$Date), mean))

tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)