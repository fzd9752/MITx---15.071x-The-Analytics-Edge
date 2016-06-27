# Unit 5 Homework -Detecting Vandalism on Wikipedia

### Setting the basic environment:
Sys.setlocale("LC_ALL", "C")
setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 5 Homework")

## 1 Bags of Words
wiki = read.csv("wiki.csv", stringsAsFactors = F)
wiki$Vandal = as.factor(wiki$Vandal)

str(wiki)
table(wiki$Vandal)

library(NLP)
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("en"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
str(wordsAdded)

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("en"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl == T)
test = subset(wikiWords, spl == F)

table(test$Vandal)
618/(618+545)

library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal~., data = train, method = "class")
prp(wikiCART)
predCART = predict(wikiCART, newdata = test, type = "class")
table(test$Vandal, predCART)
(12+618)/nrow(test)

## 2 Problem-specific Knowledge
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vandal~., data = wikiTrain2, method = "class")
prp(wikiCART2)
predCART2 = predict(wikiCART2, newdata = wikiTest2)
head(predCART2)
pred.prob = predCART2[,2]
table(test$Vandal, pred.prob >= 0.5)
(57+609)/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

train3 = subset(wikiWords2, spl == T)
test3 = subset(wikiWords2, spl == F)

wikiCART3 = rpart(Vandal~., data = train3, method = "class")
prp(wikiCART3)
pred3 = predict(wikiCART3, newdata = test3, type = "class")
table(test3$Vandal, pred3)
(514+248)/nrow(test3)

## 3 Using Non-Textual Data

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

train4 = subset(wikiWords3, spl == T)
test4 = subset(wikiWords3, spl == F)

wikiCART4 = rpart(Vandal~., data = train4, method = "class")
prp(wikiCART4)
pred4 = predict(wikiCART4, newdata = test4, type = "class")
table(test4$Vandal, pred4)
(595+248)/nrow(test4)
