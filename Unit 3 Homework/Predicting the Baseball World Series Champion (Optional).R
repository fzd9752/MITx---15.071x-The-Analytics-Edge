# Unit 3 Homework - Predicting the Baseball World Series Champion (OPTIONAL)

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 3 Homework")

## 1

baseball <- read.csv("baseball.csv")
nrow(baseball)
#or
str(baseball)

table(baseball$Year)
#or
length(table(baseball$Year))

baseball <- subset(baseball, Playoffs == 1)
nrow(baseball)

table(baseball$Year)
#or
table(table(baseball$Year))

## 2

PlayoffTable = table(baseball$Year)
names(PlayoffTable)
str(names(PlayoffTable))

baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)] 

table(baseball$NumCompetitors)

## 3

baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)

mod1 <- glm(WorldSeries ~ Year, data = baseball, family = binomial)
summary(mod1)
mod2 <- glm(WorldSeries ~ RS, data = baseball, family = binomial)
summary(mod2)
mod3 <- glm(WorldSeries ~ RA, data = baseball, family = binomial)
summary(mod3)
mod4 <- glm(WorldSeries ~ W, data = baseball, family = binomial)
summary(mod4)
mod5 <- glm(WorldSeries ~ OBP, data = baseball, family = binomial)
summary(mod5)
mod6 <- glm(WorldSeries ~ SLG, data = baseball, family = binomial)
summary(mod6)
mod7 <- glm(WorldSeries ~ BA, data = baseball, family = binomial)
summary(mod7)
mod8 <- glm(WorldSeries ~ RankSeason, data = baseball, family = binomial)
summary(mod8)
mod9 <- glm(WorldSeries ~ OSLG, data = baseball, family = binomial)
summary(mod9)
mod10 <- glm(WorldSeries ~ NumCompetitors, data = baseball, family = binomial)
summary(mod10)
mod11 <- glm(WorldSeries ~ League, data = baseball, family = binomial)
summary(mod11)

# 4

multimod1 <-  glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(multimod1)

cor(baseball[c("Year","RA", "RankSeason", "NumCompetitors")])


Logmod1 <-  glm(WorldSeries ~ Year + RA, data = baseball, family = binomial)
summary(Logmod1)
Logmod2 <-  glm(WorldSeries ~ Year  + RankSeason, data = baseball, family = binomial)
summary(Logmod2)
Logmod3 <-  glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = binomial)
summary(Logmod3)
Logmod4 <-  glm(WorldSeries ~ RA + RankSeason, data = baseball, family = binomial)
summary(Logmod4)
Logmod5 <-  glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = binomial)
summary(Logmod5)
Logmod6 <-  glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(Logmod6)
