library(quantmod)
library(plyr)
library(openxlsx)
getSymbols(c("WM", "^GSPC"), src="yahoo",
           periodicity="monthly",
           from=as.Date("2014-01-01"), to=as.Date("2021-10-02"))

WM1 <-WM[,6]
GSPC1 <-GSPC[,6]

WM2 <- data.frame(Date = index(WM1), coredata(WM1) )
GSPC2 <- data.frame(Date = index(GSPC1), coredata(GSPC1) )


WM_Price <- WM2
rm(WM2)
colnames(WM_Price) <- gsub("WM2", "WM_Price", colnames(WM_Price))

GSPC_Price <- GSPC2
rm(GSPC2)
colnames(GSPC_Price) <- gsub("GSPC2", "GSPC_Price", colnames(GSPC_Price))

names(WM_Price)[names(WM_Price) == "WM.Adjusted"] <- "WM"
names(GSPC_Price)[names(GSPC_Price) == "GSPC.Adjusted"] <- "GSPC"

write.xlsx(WM_Price, "WM_Price.xlsx")
write.xlsx(GSPC_Price, "GSPC_Price.xlsx")
