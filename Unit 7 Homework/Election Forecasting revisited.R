# Unit 7 Homework - Election Forecasting Revisited

setwd("/Users/ElsaW/Documents/MOOC/1.MITx - 15.071x The Analytics Edge/Unit 7 Homework")

library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")

#1
str(statesMap)
length(table(statesMap$group))

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

#2
polling = read.csv("PollingImputed.csv")
str(polling)
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(predictionDataFrame$TestPredictionBinary)
mean(predictionDataFrame$TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

nrow(predictionMap)
nrow(statesMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", name = "Prediction 2012")

#3
str(predictionMap)
predictionMap$TestPrediction[predictionMap$Test.State == "Florida"][1]

#4
?geom_polygon