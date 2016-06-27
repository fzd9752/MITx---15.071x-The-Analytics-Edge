# Unit 5 Homework -Automating Reviews in Medicine

### Setting the basic environment:
Sys.setlocale("LC_ALL", "C")
setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 5 Homework")


## 1 Loading the Data
trials <- read.csv("clinical_trial.csv", stringsAsFactors = F)
str(trials)
max(nchar(trials$abstract))

sum(nchar(trials$abstract) == 0)

trials$title[which.min(nchar(trials$title))]

## 2 Preparing the Corpus
library(NLP)
library(tm)

corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

corpusTitle <- tm_map(corpusTitle, tolower)
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)

corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("en"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("en"))

corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

str(dtmTitle)
ncol(dtmAbstract)

sort(colSums(dtmAbstract))
which.max(colSums(dtmAbstract))

## 3 Building a model
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial

ncol(dtm)

library(caTools)
set.seed(144)
split <- sample.split(dtm$trial, 0.7)
train <- subset(dtm, split == T)
test <- subset(dtm, split == F)

table(train$trial)
730/nrow(train)

library(rpart)
library(rpart.plot)

trialCART <- rpart(trial ~., data = train, method = "class")
prp(trialCART)

trialPred <- predict(trialCART)
max(trialPred[,2])

predTrain = predict(trialCART)[,2]
summary(predTrain)

table(train$trial, predTrain >= 0.5)
(631+441)/nrow(train)
441/(441+131)
631/(631+99)

## 4 Evaluating the model on the testing set
predTest <- predict(trialCART, newdata = test, method = "class")[, 2]
table(test$trial, predTest >= 0.5)
(162+261)/nrow(test)

library(gplots)
library(ROCR)

predROCR <- prediction(predTest, test$trial)
perROCR <- performance(predROCR, "tpr", "fpr")
plot(perROCR, colorize = T)
performance(predROCR, "auc")@y.values

## 5 DECISION-MAKER TRADEOFFS