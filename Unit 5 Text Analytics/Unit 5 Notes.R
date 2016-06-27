# Unit 5 Text Analytics

### Setting the basic environment:
Sys.setlocale("LC_ALL", "C")
setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 5 Text Analytics")


## Turning Tweets into Knowledge

###Install packages for pre-processing

install.packages("tm")
install.packages("SnowballC")
library(NLP)
library(tm)
library(SnowballC)

findFreqTerms(frequencies, lowfreq= 100)

str(trainSparse)
tweetLog <- glm(Negative ~., data = trainSparse, family = binomial)
predictions = predict(tweetLog, newdata=testSparse, type="response")

table(testSparse$Negative, predictions > 0.5)
(32+253)/nrow(testSparse)

## IBM Watson

