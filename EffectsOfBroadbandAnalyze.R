# Load necessary libraries
library(lmtest)
library(sandwich)
library(stargazer)

# Load in data
data<-read.csv("HOHData.csv")

# Provide summary statistics for the most important variables
summStats<-data[, c("INCTOT", "HASBBIA", "AGE", "SEX", "FAMSIZE")]
summary(summStats)
#Export Summary Statistics
tex_file <- "C:/Users/jack/Downloads/summStats.tex"
stargazer(summStats, out=tex_file)

# Scatter plot of total family income vs. total personal income of the head of household
# Contains line of best fit
fit<-lm(FTOTINC~INCTOT+SEX+AGE+as.factor(EDUC), data = data, weights = PERWT)
plot(data$INCTOT, data$FTOTINC, main="Total Family Income vs. Total Personal Income", ylab="Total Family Income", xlab="Total Personal Income of the Head of Household")
abline(fit, col = "red")
summary(fit)

# Regression Model
logReg<-lm(LOGINCTOT~HASBBIA+SEX+AGE+as.factor(EDUC)+as.factor(YEAR)+as.factor(RACE)+as.factor(STATEICP), weights = PERWT, data = data)
sumLogReg<-summary(logReg)

# Short Regression Model
shortReg<-lm(LOGINCTOT~HASBBIA+SEX+AGE, data=data, weights=PERWT)
summary(shortReg)

# Export regression
# Note that this regression omits some control variables to make the table more readable
finalReg<-"C:/Users/jack/Downloads/finalReg.tex"
stargazer(logReg, title="Regression Results", omit=c("EDUC", "RACE", "STATEICP"), out = finalReg)


# Store values for later comparison
coeffSumLogReg<-sumLogReg$coefficients[c("HASBBIA", "SEX", "AGE"), ]
# Obtain heteroskedasticity-robust standard errors
robust_se <- sqrt(diag(vcovHC(logReg, type = "HC1")))
# Display the robust standard errors
sumRobLogReg<-summary(logReg, robust = "HC1")
# Store values for later comparison
coeffSumRobLogReg<-sumRobLogReg$coefficients[c("HASBBIA", "SEX", "AGE"), ]

# Comparison of regression and heteroskedasticity robust standard errors
stargazer(coeffSumLogReg, coeffSumRobLogReg, title="Robust Linear Regression Coefficients", type="text")

# Extract the coefficients and their robust standard errors separately
coefficients_with_robust_se <- cbind(coef(logReg), robust_se)


# Generate summary statistics on observations without broadband internet
NOBBIA<-subset(data, HASBBIA==0)
nrow(NOBBIA)
summary(NOBBIA)

# Generate summary statistics on observations without broadband internet, that also make more than $4,500 a year
NOBBIAPOOR<-subset(data, HASBBIA==0 & INCTOT>4500)
nrow(NOBBIAPOOR)
summary(NOBBIAPOOR)


# Comparison of income among those that have broadband and those that do not have broadband
YESBBIA<-subset(data, HASBBIA==1)
summary(NOBBIA$INCTOT)
summary(YESBBIA$INCTOT)
# A plot of the distribution of income of those with bb and those without
plot(density(NOBBIA$INCTOT), main="Distribution of Income of Breadwinners")
lines(density(YESBBIA$INCTOT), col="red")
legend("topright", legend = c("No Broadband", "Broadband Users"), col = c("black", "red"), lty = 1)