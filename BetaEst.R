library(quantmod)
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
write.csv(dt1, file = "WMIXIC.csv")

#Republic Services v NYSE
getSymbols(c("RSG", "^NYA"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(RSG[,6], NYA[,6])
names(dt1)[1:2] <- c("RSG", "NYA")
dt1$lagRSG <- lag(dt1$RSG)
head(dt1)
dt1$lagNYA <- lag(dt1$NYA)
dt1$rtrnRSG <- (dt1$RSG - dt1$lagRSG)/dt1$lagRSG
dt1$rtrnNYA <- (dt1$NYA - dt1$lagNYA)/dt1$lagNYA
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnNYA, y=dt1$rtrnRSG, main="RSG ~ NYA")
cor(dt1$rtrnNYA, dt1$rtrnRSG)
linearMod <- lm(rtrnRSG ~ rtrnNYA, data=dt1)
print(linearMod)
summary(linearMod)

#Casella Waste Services v IXIC
getSymbols(c("CWST", "^IXIC"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(CWST[,6], IXIC[,6])
names(dt1)[1:2] <- c("CWST", "IXIC")
dt1$lagCWST <- lag(dt1$CWST)
head(dt1)
dt1$lagIXIC <- lag(dt1$IXIC)
dt1$rtrnCWST <- (dt1$CWST - dt1$lagCWST)/dt1$lagCWST
dt1$rtrnIXIC <- (dt1$IXIC - dt1$lagIXIC)/dt1$lagIXIC
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnIXIC, y=dt1$rtrnCWST, main="CWST ~ IXIC")
cor(dt1$rtrnIXIC, dt1$rtrnCWST)
linearMod <- lm(rtrnCWST ~ rtrnIXIC, data=dt1)
print(linearMod)
summary(linearMod)

#Stericycle Inc v IXIC
getSymbols(c("SRCL", "^IXIC"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(SRCL[,6], IXIC[,6])
names(dt1)[1:2] <- c("SRCL", "IXIC")
dt1$lagSRCL <- lag(dt1$SRCL)
head(dt1)
dt1$lagIXIC <- lag(dt1$IXIC)
dt1$rtrnSRCL <- (dt1$SRCL - dt1$lagSRCL)/dt1$lagSRCL
dt1$rtrnIXIC <- (dt1$IXIC - dt1$lagIXIC)/dt1$lagIXIC
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnIXIC, y=dt1$rtrnSRCL, main="SRCL ~ IXIC")
cor(dt1$rtrnIXIC, dt1$rtrnSRCL)
linearMod <- lm(rtrnSRCL ~ rtrnIXIC, data=dt1)
print(linearMod)
summary(linearMod)