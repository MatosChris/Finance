library(quantmod)
#Waste Management v GSPC
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
write.csv(dt1, file = "WMGSPC.csv")


#Republic Services v S&P500
getSymbols(c("RSG", "^GSPC"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(RSG[,6], GSPC[,6])
names(dt1)[1:2] <- c("RSG", "GSPC")
dt1$lagRSG <- lag(dt1$RSG)
head(dt1)
dt1$lagGSPC <- lag(dt1$GSPC)
dt1$rtrnRSG <- (dt1$RSG - dt1$lagRSG)/dt1$lagRSG
dt1$rtrnGSPC <- (dt1$GSPC - dt1$lagGSPC)/dt1$lagGSPC
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnGSPC, y=dt1$rtrnRSG, main="RSG ~ GSPC")
cor(dt1$rtrnGSPC, dt1$rtrnRSG)
linearMod <- lm(rtrnRSG ~ rtrnGSPC, data=dt1)
print(linearMod)
summary(linearMod)

write.csv(dt1, file = "RSGGSPC.csv")

#Casella Waste Services v GSPC
getSymbols(c("CWST", "^GSPC"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(CWST[,6], GSPC[,6])
names(dt1)[1:2] <- c("CWST", "GSPC")
dt1$lagCWST <- lag(dt1$CWST)
head(dt1)
dt1$lagGSPC <- lag(dt1$GSPC)
dt1$rtrnCWST <- (dt1$CWST - dt1$lagCWST)/dt1$lagCWST
dt1$rtrnGSPC <- (dt1$GSPC - dt1$lagGSPC)/dt1$lagGSPC
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnGSPC, y=dt1$rtrnCWST, main="CWST ~ GSPC")
cor(dt1$rtrnGSPC, dt1$rtrnCWST)
linearMod <- lm(rtrnCWST ~ rtrnGSPC, data=dt1)
print(linearMod)
summary(linearMod)

write.csv(dt1, file = "CWSTGSPC.csv")

#Stericycle Inc v GSPC
getSymbols(c("SRCL", "^GSPC"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2010-01-01"), to=as.Date("2021-10-01"))
dt1 <- cbind(SRCL[,6], GSPC[,6])
names(dt1)[1:2] <- c("SRCL", "GSPC")
dt1$lagSRCL <- lag(dt1$SRCL)
head(dt1)
dt1$lagGSPC <- lag(dt1$GSPC)
dt1$rtrnSRCL <- (dt1$SRCL - dt1$lagSRCL)/dt1$lagSRCL
dt1$rtrnGSPC <- (dt1$GSPC - dt1$lagGSPC)/dt1$lagGSPC
dt1 <- na.omit(dt1)
head(dt1)
nrow(dt1)
scatter.smooth(x=dt1$rtrnGSPC, y=dt1$rtrnSRCL, main="SRCL ~ GSPC")
cor(dt1$rtrnGSPC, dt1$rtrnSRCL)
linearMod <- lm(rtrnSRCL ~ rtrnGSPC, data=dt1)
print(linearMod)
summary(linearMod)

write.csv(dt1, file = "SRCLGSPC.csv")