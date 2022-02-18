library(data.table)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(forecast)
library(plyr)

WM2 <- read_excel("WM_Price.xlsx")
GSPC <- read_excel("GSPC_Price.xlsx")
CPI <- read_excel("CPIAUCSL.xls")
PPI <- read_excel("PPIACO.xls")
FED <- read_excel("FEDFUNDS.xls")
UNRATE <- read_excel("UNRATE.xls")


names(CPI)[2] <-c("CPI")
names(FED)[1] <- c("Date")
names(PPI)[1] <- c("Date")
names(UNRATE)[1] <- c("Date")
names(CPI)[1] <- c("Date")


CPI$Date <-as.Date(CPI$Date, "%m/%d/%Y")
FED$Date <-as.Date(FED$Date, "%m/%d/%Y")
PPI$Date <-as.Date(PPI$Date, "%m/%d/%Y")
UNRATE$Date <-as.Date(UNRATE$Date, "%m/%d/%Y")

stockinfo <- join_all(list(WM2, GSPC, CPI, FED, PPI, UNRATE), by='Date', type='left')


data(stockinfo)
n = nrow(stockinfo)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train = stockinfo[trainIndex ,]
test = stockinfo[-trainIndex ,]

fit3 <- lm(WM ~ GSPC + CPI + FEDFUNDS + PPIACO + UNRATE, data=train)
summary(fit3) # show results

# Other useful functions
coefficients(fit3) # model coefficients
confint(fit3, level=0.95) # CIs for model parameters
fitted(fit3) # predicted values
residuals(fit3) # residuals
anova(fit3) # anova table
vcov(fit3) # covariance matrix for model parameters
influence(fit3) # regression diagnostics

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit3)

# Stepwise Regression
library(MASS)
step <- stepAIC(fit3, direction="both")
step$anova # display results

#Score the Test Data 
WMpredict <- predict(fit3, test)

#convert scores to a dataframe
actuals_preds <- data.frame(cbind(actuals=test$WM2, predicteds=WMpredict))  # make actuals_predicteds dataframe.

#determine accuracy of model.  
correlation_accuracy <- cor(actuals_preds)  

#review actual prices
head(actuals_preds)
write.csv(actuals_preds, "actuals.csv")


forecast(WMpredict, 24)
plot(forecast(WMpredict, 24))
