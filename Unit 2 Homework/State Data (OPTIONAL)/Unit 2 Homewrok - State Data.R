# Unit 2 Homework - State Data (OPTIONAL)

setwd("~/Documents/MOOC/MITx - 15.071x The Analytics Edge/Unit 2 Homework/State Data (OPTIONAL)")

data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

str(statedata)

## P1

plot(statedata$x, statedata$y)

which.max(tapply(statedata$HS.Grad, statedata$state.region, mean))

?boxplot
boxplot(Murder ~ state.region, data = statedata)

subset(statedata, statedata$state.region == "Northeast")

## P2

LinReg <- lm(Life.Exp ~ Population + Income + Illiteracy + 
                        Murder + HS.Grad + Frost + Area, data = statedata)
summary(LinReg)

plot(statedata$Income, statedata$Life.Exp)

## P3

LinReg2 <- lm(Life.Exp ~ Population + Income + Illiteracy + 
                     Murder + HS.Grad + Frost, data = statedata)
summary(LinReg2)

LinReg3 <- lm(Life.Exp ~ Population + Income + 
                     Murder + HS.Grad + Frost, data = statedata)
summary(LinReg3)

LinReg4 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(LinReg4)

sort(predict(LinReg4))

statedata$state.name[which.min(statedata$Life.Exp)]

statedata$state.name[which.max(statedata$Life.Exp)]

sort(abs(predict(LinReg4)-statedata$Life.Exp))

sort(abs(LinReg4$residuals))
