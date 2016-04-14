# Homework 1 - AN ANALYTICAL DETECTIVE

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 1 Homework/Analytic Detective")



#Problem 1

mvt <- read.csv("mvtWeek1.csv")

nrow(mvt)
str(mvt)
max(mvt$ID)
min(mvt$Beat)
sum(mvt$Arrest)

summary(mvt$LocationDescription)
summary(mvt$LocationDescription)["ALLEY"]

# Problem 2

head(mvt$Date)

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

summary(DateConvert)

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert

which.min(table(mvt$Month))

which.max(table(mvt$Weekday))

table(mvt$Month, mvt$Arrest)[,2]

which.max(table(mvt$Month, mvt$Arrest)[,2])

# Problem 3

jpeg('unit1-hist.jpg')
hist(mvt$Date, breaks=100)
dev.off()

boxplot(mvt$Date ~ mvt$Arrest)
boxplot(subset(mvt$Date, mvt$Arrest == T))

table(mvt$Arrest, mvt$Year)
subset(mvt$Arrest, mvt$Year == 2001)

mean(subset(mvt$Arrest, mvt$Year == 2001))
mean(subset(mvt$Arrest, mvt$Year == 2007))
mean(subset(mvt$Arrest, mvt$Year == 2012))

# Problem 4

head(sort(table(mvt$LocationDescription), decreasing = T))

Top5 <- subset(mvt, mvt$LocationDescription == 'STREET'
               | mvt$LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)'
               | mvt$LocationDescription == 'ALLEY' 
               | mvt$LocationDescription == 'GAS STATION' 
               | mvt$LocationDescription == 'DRIVEWAY - RESIDENTIAL')

loc.st <- subset(mvt, mvt$LocationDescription == 'STREET')
loc.pk <- subset(mvt, mvt$LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)')
loc.al <- subset(mvt, mvt$LocationDescription == 'ALLEY')
loc.gs <- subset(mvt, mvt$LocationDescription == 'GAS STATION')
loc.dr <- subset(mvt, mvt$LocationDescription == 'DRIVEWAY - RESIDENTIAL')
Top5 <- rbind(loc.st, loc.pk, loc.al, loc.gs, loc.dr)

Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)
table(Top5$LocationDescription, Top5$Weekday)
