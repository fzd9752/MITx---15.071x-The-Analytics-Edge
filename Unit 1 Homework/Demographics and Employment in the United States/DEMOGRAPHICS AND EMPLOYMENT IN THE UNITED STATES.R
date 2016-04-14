# Homework 1 - DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 1 Homework/Demographics and Employment in the United States")

CPS <- read.csv("CPSData.csv")

# Problem 1

str(CPS)

summary(CPS)

sort(table(CPS$State))

mean(CPS$Citizenship)

table(CPS$Citizenship)
1-(7590/131302)

table(CPS$Race, CPS$Hispanic)
tapply(CPS$Hispanic, CPS$Race, sum) > 250

# Problem 2

is.na(CPS$Married)
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))

table(CPS$Region, is.na(CPS$MetroAreaCode))

tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean),decreasing = T)

# Problem 3

MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")

str(MetroAreaMap)
str(CountryMap)

## The first two arguments determine the data frames to be merged (they are called "x" and "y", 
## respectively, in the subsequent parameters to the merge function). by.x="MetroAreaCode" means 
## we're matching on the MetroAreaCode variable from the "x" data frame (CPS), while by.y="Code" 
## means we're matching on the Code variable from the "y" data frame (MetroAreaMap). Finally, 
## all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), even if some of the 
## rows' MetroAreaCode doesn't match any codes in MetroAreaMap (for those familiar with database 
## terminology, this parameter makes the operation a left outer join instead of an inner join).

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

summary(CPS)
sum(is.na(CPS$MetroArea))

sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = T), decreasing = T)

# Problem 4

CPS <- merge(CPS, CountryMap, by.x = "CountryOfBirthCode", by.y = "Code", all.x = T)

str(CPS)

sum(is.na(CPS$Country))

sort(table(CPS$Country), decreasing = T)
sort(table(CPS$Country), decreasing = T)[3]

subset(CPS$Country, CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", na.rm = T) != "United States"
mean(subset(CPS$Country, CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", na.rm = T) != "United States",na.rm = T)

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

summary(subset(CPS$MetroArea, CPS$Country == "India"), na.rm = T)
summary(subset(CPS$MetroArea, CPS$Country == "Brazil"), na.rm = T)
summary(subset(CPS$MetroArea, CPS$Country == "Somalia"), na.rm = T)

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))