library(quantmod)
#Waste Management v S&P500
getSymbols(c("WM", "^GSPC"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(WM[,6], GSPC[,6])
names(dt1)[1:2] <- c("WM", "GSPC")
dt1$lagWM <- lag(dt1$WM)
head(dt1)
dt1$lagGSPC <- lag(dt1$GSPC)
dt1$rtrnWM <- (dt1$WM - dt1$lagWM)/dt1$lagWM
dt1$rtrnGSPC <- (dt1$GSPC - dt1$lagGSPC)/dt1$lagGSPC
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnGSPC, y=dt1$rtrnWM, main="WM ~ GSPC")
cor(dt1$rtrnGSPC, dt1$rtrnWM)
linearMod <- lm(rtrnWM ~ rtrnGSPC, data=dt1)
print(linearMod)
summary(linearMod)
#(Intercept) is estimate of Risk Free Rate
#rtrn = estimate of beta
write.csv(dt1, file = "WMGSPC2.csv")

#Waste Management v NASDAQ
getSymbols(c("WM", "^IXIC"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(WM[,6], IXIC[,6])
names(dt1)[1:2] <- c("WM", "IXIC")
dt1$lagWM <- lag(dt1$WM)
head(dt1)
dt1$lagIXIC <- lag(dt1$IXIC)
dt1$rtrnWM <- (dt1$WM - dt1$lagWM)/dt1$lagWM
dt1$rtrnIXIC <- (dt1$IXIC - dt1$lagIXIC)/dt1$lagIXIC
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnIXIC, y=dt1$rtrnWM, main="WM ~ IXIC")
cor(dt1$rtrnIXIC, dt1$rtrnWM)
linearMod <- lm(rtrnWM ~ rtrnIXIC, data=dt1)
print(linearMod)
summary(linearMod)
#(Intercept) is estimate of Risk Free Rate
#rtrn = estimate of beta

#Waste Management v Dow Jones
getSymbols(c("WM", "^DJI"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(WM[,6], DJI[,6])
names(dt1)[1:2] <- c("WM", "DJI")
dt1$lagWM <- lag(dt1$WM)
head(dt1)
dt1$lagDJI <- lag(dt1$DJI)
dt1$rtrnWM <- (dt1$WM - dt1$lagWM)/dt1$lagWM
dt1$rtrnDJI <- (dt1$DJI - dt1$lagDJI)/dt1$lagDJI
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnDJI, y=dt1$rtrnWM, main="WM ~ DJI")
cor(dt1$rtrnDJI, dt1$rtrnWM)
linearMod <- lm(rtrnWM ~ rtrnDJI, data=dt1)
print(linearMod)
summary(linearMod)
#(Intercept) is estimate of Risk Free Rate
#rtrn = estimate of beta

#Waste Management v NYSE
getSymbols(c("WM", "^NYA"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(WM[,6], NYA[,6])
names(dt1)[1:2] <- c("WM", "NYA")
dt1$lagWM <- lag(dt1$WM)
head(dt1)
dt1$lagNYA <- lag(dt1$NYA)
dt1$rtrnWM <- (dt1$WM - dt1$lagWM)/dt1$lagWM
dt1$rtrnNYA <- (dt1$NYA - dt1$lagNYA)/dt1$lagNYA
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnNYA, y=dt1$rtrnWM, main="WM ~ NYA")
cor(dt1$rtrnNYA, dt1$rtrnWM)
linearMod <- lm(rtrnWM ~ rtrnNYA, data=dt1)
print(linearMod)
summary(linearMod)
#(Intercept) is estimate of Risk Free Rate
#rtrn = estimate of beta

