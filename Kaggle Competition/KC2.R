# Kaggle Competition 0.61063

## Load the data
train <- read.csv("train2016.csv", stringsAsFactors = F)
any(is.na(train))
str(train)

train[train == ""] = "Skip"
str(train)

train <- lapply(train, as.factor)
train$USER_ID <- as.numeric(as.character(train$USER_ID))
train$YOB <- as.numeric(as.character(train$YOB))
class(train)
train <- as.data.frame(train)
str(train)

any(is.na(train))
nrow(train[which(is.na(train)), ])

train <- na.omit(train)
class(train)

## Select Important vairables with Boruta
library(ranger)
library(Boruta)
set.seed(888)

boruta.train <- Boruta(Party ~. -USER_ID, data = train, doTrace = 1)

print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
        boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

### Get the Important Variables
getSelectedAttributes(final.boruta, withTentative = F)

### "Gender"  "Q118233" "Q115611" "Q113181" "Q110740" "Q109244" 
### "Q108855" "Q106272" "Q101163" "Q98869"  "Q98197"

Log1 <- glm(Party ~ Gender + Q118233 + Q115611 + Q113181 + Q110740 +
            Q109244 + Q108855 + Q106272 + Q101163 + Q98869 +
            Q98197, data = train, family = binomial)
summary(Log1)
tb <- table(train$Party, predict(Log1, type = "response") >= 0.5)
(tb[1,1]+tb[2,2])/5235



