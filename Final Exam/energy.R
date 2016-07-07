# The Analytics Edge - Final Exam 3

setwd("/Users/ElsaW/Documents/MOOC/MOOC_Completed/MITx - 15.071x The Analytics Edge/Final Exam")

energy <- read.csv("energy.csv")
head(energy)
str(energy)

energy[which.max(energy$GenTotalRenewable),]

mean(energy$AllSourcesCO2[energy$presidential.results == 0], na.rm = T)
mean(energy$AllSourcesCO2[energy$presidential.results == 1], na.rm = T)
mean(energy$AllSourcesNOx[energy$presidential.results == 0], na.rm = T)
mean(energy$AllSourcesNOx[energy$presidential.results == 1], na.rm = T)

cor(energy$AllSourcesCO2, energy$EsalesIndustrial, use = "complete")
cor(energy$AllSourcesSO2, energy$EsalesIndustrial, use = "complete")
cor(energy$AllSourcesNOx, energy$EsalesResidential, use = "complete")
cor(energy$AllSourcesCO2, energy$EsalesCommercial, use = "complete")

boxplot(energy$EPriceTota ~ energy$STATE)
tapply(energy$EPriceTotal, energy$STATE, mean)
sort(tapply(energy$EPriceTotal, energy$STATE, mean))
sort(tapply(energy$GenTotal, energy$STATE, mean))

## Pro 5
energy$GenSolarBinary <- as.factor(energy$GenSolarBinary)
energy$GenTotalRenewableBinary <- as.factor(energy$GenTotalRenewableBinary)
energy$presidential.results <- as.factor(energy$presidential.results)

set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]

mod <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train, family = "binomial")
summary(mod)

## Pro 6

premod <- predict(mod, newdata = test, type = "response")
table(test$GenSolarBinary, premod >= 0.5)
(18+154)/nrow(test)

testRep <- test[test$presidential.results == 0, ]
testDem <- test[test$presidential.results == 1, ]
preRep <- predict(mod, newdata = testRep, type = "response")
preDem <- predict(mod, newdata = testDem, type = "response")

table(testRep$GenSolarBinary, preRep >= 0.5)
(2+90)/nrow(testRep)
table(testDem$GenSolarBinary, preDem >= 0.5)
(16+64)/nrow(testDem)

## Pro 7
train.limited <- train[ , c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import")]
test.limited <- test[ , c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import")]
train.norm <- predict(preProcess(train.limited), train.limited)
test.norm <- predict(preProcess(test.limited),test.limited)

library(flexclust)
set.seed(144)
K <- kmeans(train.norm, centers = 2, iter.max = 1000)
km.kcca = as.kcca(K, train.norm)
cluster.train <- predict(km.kcca)

train1 <- train[K$cluster == 1, ]
train2 <- train[K$cluster == 2, ]

K$centers
mean(train1$AllSourcesCO2, na.rm = T)
mean(train2$AllSourcesCO2, na.rm = T)

## Pro 8
mod1 <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train1, family = "binomial")
summary(mod1)

## Pro 9
kt.kcca <- as.kcca(K, test.norm)
cluster.test <- predict(kt.kcca)
test1 <- test[cluster.test == 1, ]

premod1 <- predict(mod1, newdata = test1, type = "response")
table(test1$GenSolarBinary, premod1 >= 0.5)
(4+114)/nrow(test1)

test1mod <- predict(mod, newdata = test1, type = "response")
table(test1$GenSolarBinary, test1mod >= 0.5)
115/130

## Pro 10
mod2 <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train2, family = "binomial")
summary(mod2)

## Pro 11
test2 <- test[cluster.test == 2, ]
premod2 <- predict(mod2, newdata = test2, type = "response")
table(test2$GenSolarBinary, premod2 >= 0.5)
60/80

test2mod <- predict(mod, newdata = test2, type = "response")
table(test2$GenSolarBinary, test2mod >= 0.5)
(18+39)/80

## Pro 12
AllPredictions <- c(premod1,premod2)
AllOutcomes <- c(test1$GenSolarBinary, test2$GenSolarBinary)
table(AllOutcomes, AllPredictions >= 0.5)

(178)/nrow(test)
