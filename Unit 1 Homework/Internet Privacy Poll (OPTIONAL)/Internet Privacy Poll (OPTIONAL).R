# Homework 1 INTERNET PRIVACY POLL (OPTIONAL)

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 1 Homework/Internet Privacy Poll (OPTIONAL)")

# Problem 1

poll <- read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)

table(poll$Smartphone)

table(poll$State, poll$Region == "South")
table(poll$State, poll$Region == "Midwest")

# Problem 2

table(poll$Smartphone, poll$Internet.Use)

sum(is.na(poll$Internet.Use))
sum(is.na(poll$Smartphone))

limited <- subset(poll, poll$Internet.Use == 1 | poll$Smartphone == 1)
str(limited)

# Problem 3

summary(limited)

sum(limited$Info.On.Internet == 0)
sum(limited$Info.On.Internet == 11)
table(limited$Info.On.Internet)

mean(limited$Worry.About.Info, na.rm = T)
mean(limited$Anonymity.Possible, na.rm = T)
mean(limited$Tried.Masking.Identity, na.rm = T)
summary(limited)

# Problem 4

hist(limited$Age)

plot(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))
## Wrong
index <- table(limited$Age, limited$Info.On.Internet) > 1
table(limited$Age, limited$Info.On.Internet)[index]
sum(table(limited$Age, limited$Info.On.Internet)[index])

jitter(c(1, 2, 3))

plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

tapply(limited$Info.On.Internet, limited$Smartphone, mean)

tapply(limited$Tried.Masking.Identity == 1, limited$Smartphone, mean, na.rm = T)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
