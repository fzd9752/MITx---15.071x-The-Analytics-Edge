# Submission 0.55460

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

df <- test

df <- df %>%
        mutate(FinaceStatus = Q102674*0.69741563 + Q106388*0.45840428 + Q108343*0.67475749
               + Q116441*0.56444927 + Q123621*0.54434040)

df <- df %>%
        mutate(EducationStatus = Q120379*0.6497771 + Q122771*-0.4915218 + Q119851*0.5922486)

df <- df %>%
        mutate(Fam = Q98059*-0.68715014 + Q120650*-0.49784237 +  Q108617*0.66863893 +  Q99716*0.63392069)

df <- df %>%
        mutate(Soc = Q106997*-0.65758066 + Q108856*-0.66264616 + Q113584*-0.51027050)

df <- df %>% mutate(Att = Q119650*-0.5884656 + Q98869*-0.6552226 + Q114961*0.5760039 + Q116881*-0.4754082)

df <- df %>% mutate(Con = Q118117*-0.58626035 + Q121700*-0.62746999 + Q116448*-0.58817316)

df <- df %>% mutate(Slf = Q106993*-0.58365703 + Q107869*-0.40823785 + Q112512*-0.45264414
                    + Q120014*-0.47422437 + Q122120*0.53987832)

df <- df %>% mutate(Frst = Q105840*-0.578957504 + Q100680*-0.527635468 + Q120012*-0.420185112
                    + Q102906*-0.562846897 + Q118237*-0.569155604)

df <- df %>% mutate(Pray = (Q98197 + Q113181)/2)


PredTest = predict(Log2, newdata=df, type="response")

threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "Log2.csv", row.names=FALSE)
