# Explore the data

## Explore the quality of responding

## Rate of nonresponse
sum(is.na(train))/(101*nrow(train))

## Rate of unit nonresponse
rowSums(!is.na(train[, 8:108]))/101

resultplot = NA
for (i in seq(0, 1, 0.001)) {
        test = mean(rowSums(!is.na(train[, 8:108]))/101 >= i) 
        
        out = data.frame("rate" = i
                         , "Percent" = test)
        
        resultplot = rbind(resultplot, out)
        
}


ggplot(resultplot, aes(Percent, rate)) + geom_smooth()

## Rate of Item nonresponse
colSums(!is.na(train[, 8:108]))
sort(colSums(!is.na(train[, 8:108])))


which(rowSums(!is.na(train[, 8:108]))/101 <= 0)
which(rowSums(!is.na(train[, 8:108]))/101 <= 0.1)
train[which(rowSums(!is.na(train[, 8:108])) == 1), ]

Response0 = subset(train, rowSums(!is.na(train[, 8:108])) == 0)
Response1 = subset(train, rowSums(!is.na(train[, 8:108])) == 1)


RegMod.response0 = glm(Party ~ YOB + Gender + EducationLevel, data = Response0, family = binomial)
summary(RegMod.response0)

trainnonas = subset(train, rowSums(is.na(train[, 2:108])) == 0)

#cart
library(rpart)
library(rpart.plot)
cartforpuretrain = rpart(Party ~ . -USER_ID, data = trainnonas, method = "class")
prp(cartforpuretrain)

pretrain = predict(cartforpuretrain, newdata = train, type = "class")
head(pretrain)
table(pretrain, train$Party)

cartfortrain = rpart(Party ~ . -USER_ID, data = train, method = "class")
prp(cartfortrain)
pretrain2 = predict(cartfortrain, type = "class")
table(pretrain2, train$Party)


#RandomForest
library(randomForest)

RFmod = randomForest(Party ~ . -USER_ID, data = train, na.action = na.omit)
varImpPlot(RFmod)

# K-means clustering
library(grid)
library(lattice)
library(modeltools)
library(stats4)
library(flexclust)

kclustermod = kmeans(trainnonas, centers = 10, iter.max = 1000)

library(nFactors)
ev <- eigen(cor(train))

