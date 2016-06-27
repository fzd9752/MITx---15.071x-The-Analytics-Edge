# Kaggle Competition
train0 <- read.csv("train2016.csv",  stringsAsFactors = F)
train <- read.csv("train2016.csv",  stringsAsFactors = F)

## Clean data
train[train == ""] = 0

train[train == "Democrat"] = 0
train[train == "Republican"] = 1

train[train == "Yes"] = 1
train[train == "No"] = -1
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

train = sapply(train, as.numeric)
train <- as.data.frame(train)
str(train)

# Subset Economy

trEco <- train[, c("Party", "Income", "Q102089"
                   , "Q102674", "Q106388", "Q108343", "Q109367", "Q116441", "Q117193"
                   , "Q122770", "Q123464", "Q123621", "Q114152", "Q115195")]

trRule <- train[, c("Party", "Q116953", "Q98578", "Q99982", "Q102687"
                    , "Q104996", "Q111220", "Q116197", "Q116797", "Q120194", "Q99480"
                    , "Q101596", "Q111580", "Q115602", "Q98078", "Q105655")]

trFam <- train[, c("Party", "HouseholdStatus", "Q98059", "Q108754", "Q108855"
                   , "Q120650", "Q124122", "Q101163", "Q108617", "Q120978", "Q111848"
                   ,"Q99716")]

trSoc <- train[, c("Party", "Q106997", "Q108342", "Q108856", "Q113584"
                   , "Q114386", "Q124742")]

trEdu <- train[, c("Party", "EducationLevel", "Q120379", "Q120472", "Q122771"
                   , "Q119851")]

trAtt <- train[, c("Party", "Q118232", "Q119650", "Q98869", "Q114961"
                   , "Q116881")]

trCnsv <- train[, c("Party", "Q115777", "Q118117", "Q121011", "Q121700"
                    , "Q115390", "Q115899", "Q116448")]

trSlf <- train[, c("Party", "Q100689", "Q106993", "Q107869", "Q112270"
                   , "Q112512", "Q120014", "Q122120")]

trFrt <- train[, c("Party", "Q105840", "Q100680", "Q120012", "Q102906"
                   , "Q118237", "Q119334")]

trUnk <- train[, c("Party", "Q99581", "Q100010", "Q103293", "Q117186", "Q106042"
                   , "Q106389", "Q107491", "Q110740", "Q113583", "Q114517", "Q114748"
                   , "Q118892", "Q122769", "Q112478", "Q121699", "Q106272", "Q118233")]
