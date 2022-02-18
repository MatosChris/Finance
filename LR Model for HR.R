library(data.table)
players2018 <-fread("players2018.csv")
fit <- lm(HR ~ AB + SO + X2B  + RBI + SB, data=players2018) #lm = logistics model
summary(fit) #results

# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

# scatterplot for HR ~ AB correlation
scatter.smooth(x=players2018$AB, y=players2018$HR, main="HR ~ AB")
# scatterplot for HR ~ SO correlation
scatter.smooth(x=players2018$SO, y=players2018$HR, main="HR ~ SO")
# scatterplot for HR ~ X2B correlation
scatter.smooth(x=players2018$X2B, y=players2018$HR, main="HR ~ X2B")
# scatterplot for price ~ cut correlation
scatter.smooth(x=players2018$RBI, y=players2018$HR, main="HR ~ RBI")



# Stepwise Regression
library(MASS)
step <- stepAIC(fit, direction="both")
step$anova # display results, tells me what to kick out (likely high P-value)

#Score the 2019 Players
players2019 <-fread("players2019.csv")
HRPred <- predict(fit, players2019)

#convert scores to a dataframe
actuals_preds <- data.frame(cbind(actuals=players2019$HR, predicteds=HRPred))  # make actuals_predicteds dataframe.

#determine accuracy of model.  
correlation_accuracy <- cor(actuals_preds)  

#review the Number of HRs
head(actuals_preds)
write.csv(actuals_preds, "actuals.csv")