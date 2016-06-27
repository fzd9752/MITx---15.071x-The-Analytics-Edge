# Unit 5 Homework -  Separating Spam from Ham (Part 1)

### Setting the basic environment:
Sys.setlocale("LC_ALL", "C")
setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 5 Homework")

emails <- read.csv("emails.csv", stringsAsFactors = F)
str(emails)

table(emails$spam)
sum(emails$spam == 1)

head(emails$text)

max(nchar(emails$text))

which.min(nchar(emails$text))

## 2 Preparing the Corpus
library(NLP)
library(tm)

corpus <- Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("en"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse <- as.data.frame(as.matrix(spdtm))
str(emailsSparse)
colnames(emailsSparse) = make.names(colnames(emailsSparse))

which.max(colSums(emailsSparse))

emailsSparse$spam = emails$spam
str(emailsSparse)

subset(emailsSparse, spam == 1)

sort(colSums(subset(emailsSparse, spam == 0)))
sum(colSums(subset(emailsSparse, emailsSparse$spam == 0)) >= 5000)

sort(colSums(subset(emailsSparse, spam == 1)))

## 3 Building machine learning models
emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)

spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == T)
test = subset(emailsSparse, spl == F)

spamLog = glm(spam ~., data = train, family = "binomial")

library(rpart)
library(rpart.plot)
spamCart = rpart(spam ~., data = train, method="class")

library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~., data = train)

predLog = predict(spamLog, type = "response")
predCart = predict(spamCart)[,2]
predRF = predict(spamRF, type = "prob")[,2]

sum(predLog < 0.00001)
sum(predLog > 0.99999)
sum(predLog >= 0.00001 & predLog <= 0.99999)

summary(spamLog)

prp(spamCart)

table(train$spam, predLog >= 0.5)
(3052+952)/nrow(train)

library(ROCR)
rocrLog = prediction(predLog, train$spam)
performance(rocrLog, "auc")@y.values

table(train$spam, predCart >= 0.5)
(2884+887)/nrow(train)

rocrCart = prediction(predCart, train$spam)
performance(rocrCart, "auc")@y.values

table(train$spam, predRF >= 0.5)
(3013+914)/nrow(train)

rocrRF = prediction(predRF, train$spam)
performance(rocrRF, "auc")@y.values

## 4 Evaluating on the Test Set
testLog = predict(spamLog, newdata = test, type = "response")
testCart = predict(spamCart, newdata = test)[,2]
testRF = predict(spamRF, newdata = test, type = "prob")[,2]

table(test$spam, testLog >= 0.5)
(376+1257)/nrow(test)
performance(prediction(testLog, test$spam),"auc")@y.values

table(test$spam, testCart >= 0.5)
(386+1228)/nrow(test)
performance(prediction(testCart, test$spam),"auc")@y.values

table(test$spam, testRF >= 0.5)
(386+1290)/nrow(test)
performance(prediction(testRF, test$spam),"auc")@y.values

# Unit 5 Homework -  Separating Spam from Ham (Part 2)

## 6 Integrating Word Count Information

wordCount = rowSums(as.matrix(dtm))

hist(wordCount)

hist(log(wordCount))

emailsSparse$logWordcount = log(wordCount)
boxplot(emailsSparse$logWordcount~emailsSparse$spam)

train2 = subset(emailsSparse, spl ==T)
test2 = subset(emailsSparse, spl ==F)

spam2CART = rpart(spam ~., data = train2, method = "class")
set.seed(123)
spam2RF = rpart(spam ~., data = train2)

prp(spam2CART)
prp(spam2RF)

pred2CART = predict(spam2CART, newdata = test2)[,2]
pred2RF = predict(spam2RF, newdata = test2, type = "prob")[,2]

table(test2$spam, pred2CART >= 0.5)
(384+1214)/nrow(test2)
performance(prediction(pred2CART,test$spam),"auc")@y.values

table(test2$spam, pred2RF >= 0.5)
(384+1214)/nrow(test2)
performance(prediction(pred2RF,test$spam),"auc")@y.values
