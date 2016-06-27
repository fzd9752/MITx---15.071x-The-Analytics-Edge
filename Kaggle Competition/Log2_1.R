# Add Factors
library(dplyr)
df <- train

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



# set Logistic Regression

Log2 <- glm(Party ~ FinaceStatus + EducationStatus + Fam + Soc + Att + Con + Slf + Frst + Pray + Q109244
            + Q115611 + Gender, data = df, family = binomial)
summary(Log2)
tb <- table(df$Party, predict(Log2, type = "response") >= 0.5)
(tb[1,1]+tb[2,2])/5235


Log2 <- glm(Party ~ EducationStatus + Fam + Pray + Q109244
            + Q115611 + Gender, data = df, family = binomial)
summary(Log2)
tb <- table(df$Party, predict(Log2, type = "response") >= 0.5)
(tb[1,1]+tb[2,2])/5235

