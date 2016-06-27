# Covert data to numeric 
train <- read.csv("train2016.csv",  stringsAsFactors = F)

train[train == "Democrat"] = 0
train[train == "Republican"] = 1

train[train == "Yes"] = 1
train[train == "No"] = -1
train[train == ""] = 0
train[train == "Male"] = 1
train[train == "Female"] = -1
train[train == "Only-child"] = -1
train[train == "Check!"] = 1
train[train == "Nope"] = -1
train[train == "Optimist"] = 1
train[train == "Pessimist"] = -1
train[train == "Mom"] = 1
train[train == "Dad"] = -1
train[train == "Rent"] = 1
train[train == "Own"] = -1
train[train == "Public"] = 1
train[train == "Private"] = -1
train[train == "Science"] = 1
train[train == "Art"] = -1
train[train == "Study first"] = 1
train[train == "Try first"] = -1
train[train == "Giving"] = 1
train[train == "Receiving"] = -1
train[train == "Idealist"] = 1
train[train == "Pragmatist"] = -1
train[train == "Standard hours"] = 1
train[train == "Odd hours"] = -1
train[train == "Hot headed"] = 1
train[train == "Cool headed"] = -1
train[train == "Happy"] = 1
train[train == "Right"] = -1
train[train == "A.M."] = 1
train[train == "P.M."] = -1
train[train == "Circumstances"] = 1
train[train == "Me"] = -1
train[train == "Start"] = 1
train[train == "End"] = -1
train[train == "TMI"] = 1
train[train == "Mysterious"] = -1
train[train == "People"] = 1
train[train == "Technology"] = -1
train[train == "Tunes"] = 1
train[train == "Talk"] = -1
train[train == "Supportive"] = 1
train[train == "Demanding"] = -1
train[train == "Mac"] = 1
train[train == "PC"] = -1
train[train == "Cautious"] = 1
train[train == "Risk-friendly"] = -1
train[train == "Socialize"] = 1
train[train == "Space"] = -1
train[train == "Yes!"] = 1
train[train == "Umm..."] = -1
train[train == "Online"] = 1
train[train == "In-person"] = -1
train[train == "Yay people!"] = 1
train[train == "Grrr people"] = -1

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

str(train)

train$USER_ID = NULL

train = sapply(train, as.numeric)
train <- as.data.frame(train)

train <- train %>% select(-Party, everything())
train$Party <- as.numeric(train$Party)

res.pca = PCA(train[ ,7:107], scale.unit=T, ncp=5, graph=F)
plot.PCA(res.pca, axes = c(1,2), choix = "ind", habillage = 6)
dimdesc(res.pca)

res.pca$eig

str(train)
library(factoextra)
eig.val <- get_eigenvalue(res.pca)
head(eig.val,50)

barplot(eig.val[,2], names.arg = 1:101)

var <- get_pca_var(res.pca)
var

var$coord[, 1:4]

fviz_pca_var(res.pca)

cor.mat <- round(cor(train[7:107]),2)
library("corrplot")
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(train[7:107], histogram=TRUE, pch=19)

print(res.pca)
