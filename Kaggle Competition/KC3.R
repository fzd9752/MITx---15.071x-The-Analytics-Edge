# Kaggle Competition 0.54023
train0 <- read.csv("train2016.csv",  stringsAsFactors = F)
train <- read.csv("train2016.csv",  stringsAsFactors = F)

## Clean data
train[train == ""] = 0
any(is.na(train))
train[train == "Democrat"] = 0
train[train == "Republican"] = 1

train[train == "Yes"] = 2
train[train == "No"] = 1
train[train == "Male"] = 2
train[train == "Female"] = 1
train[train == "Only-child"] = 1
train[train == "Check!"] = 2
train[train == "Nope"] = 1
train[train == "Optimist"] = 2
train[train == "Pessimist"] = 1
train[train == "Mom"] = 2
train[train == "Dad"] = 1
train[train == "Rent"] = 2
train[train == "Own"] = 1
train[train == "Public"] = 2
train[train == "Private"] = 1
train[train == "Science"] = 2
train[train == "Art"] = 1
train[train == "Study first"] = 2
train[train == "Try first"] = 1
train[train == "Giving"] = 2
train[train == "Receiving"] = 1
train[train == "Idealist"] = 2
train[train == "Pragmatist"] = 1
train[train == "Standard hours"] = 2
train[train == "Odd hours"] = 1
train[train == "Hot headed"] = 2
train[train == "Cool headed"] = 1
train[train == "Happy"] = 2
train[train == "Right"] = 1
train[train == "A.M."] = 2
train[train == "P.M."] = 1
train[train == "Circumstances"] = 2
train[train == "Me"] = 1
train[train == "Start"] = 2
train[train == "End"] = 1
train[train == "TMI"] = 2
train[train == "Mysterious"] = 1
train[train == "People"] = 2
train[train == "Technology"] = 1
train[train == "Tunes"] = 2
train[train == "Talk"] = 1
train[train == "Supportive"] = 2
train[train == "Demanding"] = 1
train[train == "Mac"] = 2
train[train == "PC"] = 1
train[train == "Cautious"] = 2
train[train == "Risk-friendly"] = 1
train[train == "Socialize"] = 2
train[train == "Space"] = 1
train[train == "Yes!"] = 2
train[train == "Umm..."] = 1
train[train == "Online"] = 2
train[train == "In-person"] = 1
train[train == "Yay people!"] = 2
train[train == "Grrr people"] = 1

train[train == "under $25,000"] = 1
train[train == "$25,001 - $50,000"] = 2
train[train == "$50,000 - $74,999"] = 3
train[train == "$75,000 - $100,000"] = 4
train[train == "$100,001 - $150,000"] = 5
train[train == "over $150,000"] = 6

train[train == "Current K-12"] = 1
train[train == "High School Diploma"] = 2
train[train == "Current Undergraduate"] = 3
train[train == "Associate's Degree"] = 4
train[train == "Bachelor's Degree"] = 5
train[train == "Master's Degree"] = 6
train[train == "Doctoral Degree"] = 7

train[train == "Domestic Partners (no kids)"] = 1
train[train == "Domestic Partners (w/kids)"] = 2
train[train == "Married (no kids)"] = 3
train[train == "Married (w/kids)"] = 4
train[train == "Single (no kids)"] = 5
train[train == "Single (w/kids)"] = 6

train = sapply(train, as.numeric)
train <- as.data.frame(train)
str(train)

natr <- train[rowSums(is.na(train)) > 0, ]
tr <- na.omit(train)

## Find ImpVars
library(ranger)
library(Boruta)
set.seed(888)

boruta.train <- Boruta(Party ~. -USER_ID, data = tr, doTrace = 1)

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
### "Gender"  "Q115611" "Q113181" "Q110740" "Q109244" "Q108855" "Q107491" 
### "Q106997" "Q106272" "Q106388" "Q106389" "Q106042" "Q105840" "Q105655"
### "Q101163" "Q101596" "Q99480"  "Q98869" "Q98059"  "Q98197" 

Log3 <- glm(Party ~ Gender + Q115611 + Q113181 + Q110740 + Q109244 + Q108855 + Q107491
            + Q106997 +Q106272 +Q106388 +Q106389 +Q106042 +Q105840 +Q105655 + Q101163
            + Q101596 + Q99480 + Q98869 + Q98059 + Q98197, data = tr, family = binomial)




correlation <- as.data.frame(cor(train[4:108]))
correlation[correlation >= 0.6]


## submissiont
test <- read.csv("test2016.csv",  stringsAsFactors = F)

## Clean data
test[test == ""] = 0
any(is.na(test))
test[test == "Democrat"] = 0
test[test == "Republican"] = 1

test[test == "Yes"] = 2
test[test == "No"] = 1
test[test == "Male"] = 2
test[test == "Female"] = 1
test[test == "Only-child"] = 1
test[test == "Check!"] = 2
test[test == "Nope"] = 1
test[test == "Optimist"] = 2
test[test == "Pessimist"] = 1
test[test == "Mom"] = 2
test[test == "Dad"] = 1
test[test == "Rent"] = 2
test[test == "Own"] = 1
test[test == "Public"] = 2
test[test == "Private"] = 1
test[test == "Science"] = 2
test[test == "Art"] = 1
test[test == "Study first"] = 2
test[test == "Try first"] = 1
test[test == "Giving"] = 2
test[test == "Receiving"] = 1
test[test == "Idealist"] = 2
test[test == "Pragmatist"] = 1
test[test == "Standard hours"] = 2
test[test == "Odd hours"] = 1
test[test == "Hot headed"] = 2
test[test == "Cool headed"] = 1
test[test == "Happy"] = 2
test[test == "Right"] = 1
test[test == "A.M."] = 2
test[test == "P.M."] = 1
test[test == "Circumstances"] = 2
test[test == "Me"] = 1
test[test == "Start"] = 2
test[test == "End"] = 1
test[test == "TMI"] = 2
test[test == "Mysterious"] = 1
test[test == "People"] = 2
test[test == "Technology"] = 1
test[test == "Tunes"] = 2
test[test == "Talk"] = 1
test[test == "Supportive"] = 2
test[test == "Demanding"] = 1
test[test == "Mac"] = 2
test[test == "PC"] = 1
test[test == "Cautious"] = 2
test[test == "Risk-friendly"] = 1
test[test == "Socialize"] = 2
test[test == "Space"] = 1
test[test == "Yes!"] = 2
test[test == "Umm..."] = 1
test[test == "Online"] = 2
test[test == "In-person"] = 1
test[test == "Yay people!"] = 2
test[test == "Grrr people"] = 1

test[test == "under $25,000"] = 1
test[test == "$25,001 - $50,000"] = 2
test[test == "$50,000 - $74,999"] = 3
test[test == "$75,000 - $100,000"] = 4
test[test == "$100,001 - $150,000"] = 5
test[test == "over $150,000"] = 6

test[test == "Current K-12"] = 1
test[test == "High School Diploma"] = 2
test[test == "Current Undergraduate"] = 3
test[test == "Associate's Degree"] = 4
test[test == "Bachelor's Degree"] = 5
test[test == "Master's Degree"] = 6
test[test == "Doctoral Degree"] = 7

test[test == "Domestic Partners (no kids)"] = 1
test[test == "Domestic Partners (w/kids)"] = 2
test[test == "Married (no kids)"] = 3
test[test == "Married (w/kids)"] = 4
test[test == "Single (no kids)"] = 5
test[test == "Single (w/kids)"] = 6

test = sapply(test, as.numeric)
test <- as.data.frame(test)
str(test)


PredTest = predict(Log3, newdata=test, type="response")

threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "Log3.csv", row.names=FALSE)

