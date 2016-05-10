# Unit 3 Homework - Popularity of Music Records

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 3 Homework")

## 1

songs <- read.csv("songs.csv")
table(songs$year)

table(songs$artistname)["Michael Jackson"] ### or
MichaelJackson = subset(songs, artistname == "Michael Jackson")
str(MichaelJackson) ### or 
nrow(MichaelJackson)

MichaelJackson[c("songtitle", "Top10")]

table(songs$timesignature)

songs$songtitle[which.max(songs$tempo)]

## P2

SongsTrain <- subset(songs, year != 2010) ### or
SongsTrain = subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)
nrow(SongsTrain)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain <- SongsTrain[,!(names(SongsTrain)%in% nonvars)]
SongsTest <- SongsTest[, !(names(SongsTest) %in% nonvars)]

SongsLog1 <- glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(SongsLog1)

## P3

cor(SongsTrain$loudness, SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

## P4

predTest <- predict(SongsLog3, newdata = SongsTest , type="response")
table(SongsTest$Top10, predTest >= 0.45)
(309 + 19)/(309+5+59)

table(SongsTest$Top10)
314/373

19/59
309/314

