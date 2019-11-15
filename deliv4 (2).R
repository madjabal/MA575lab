# Code for Deliverable 4
# Cannot use FFMC, DMC, or DC
# Explanatory is ISI 

Data <- read.csv("forestfires.csv",header=T)
Data <- subset(Data, select=-c(FFMC, DMC, DC))
Data <- Data[Data$ISI < 50, ]
Data <- Data[Data$month != 'nov', ]

# Sample 50% of Data to train and the other 50% to test
set.seed((101))
train <- sample(nrow(Data), dim(Data)%/%2)
test_data <- Data[-train, ]
train_data <- Data[train, ]

attach(train_data)

D <- cbind(X,Y, month, day, temp, RH, wind, rain, area, ISI)
pairs(D, gap=0.4, pch = 19, cex = 0.1)

c <- cor(D)
round(c,3)
## OLS ..................................................................................
lm.ols <- lm(ISI ~ temp + wind + RH + day + month + I(wind^2) + I(wind^3) + I(wind^4) + temp*RH)
summary(lm.ols)

StanResMLS <- rstandard((lm.ols))

preds <- predict(lm.ols, newdata=test_data)

# Plot against ISI shows pattern and reveals low quality model
plot(ISI, StanResMLS,pch=19,cex=0.5)
abline(h=2,lty=2)
abline(h=-2,lty=2)

# This plot against the predicted shows no pattern however
plot(fitted(lm.ols), StanResMLS,pch=19,cex=0.5)

# It plots the data(ISI in this case) in sorted order versus quantiles from a standard Normal Distr
q1 <- qqnorm(StanResMLS)
#plot(range(q1$x, q1$x), range(q1$y, q1$y), ylab = "Standardized Residuals", xlab = "Theoretical Quantiles" ) 
points(q1)
points(q1,col="blue", pch = 19, cex = 0.5)
qqline(StanResMLS,lty = 2)
legend(2, 0, legend=c("MLS"), col=c("blue"), lty=0, cex=1, pch=19)

# histogram of OLS residuals
par(mfrow=c(1,1))
hist(StanResMLS,100)


### Residuals for training data
ResMLS <- resid(lm.ols)
### Residuals for validation data
output<-predict(lm.ols, se.fit = TRUE, newdata=test_data)
ResMLSValidation <- test_data$ISI - output$fit



### Plot residuals
par(mfrow=c(1,1))
plot(train_data$ISI,ResMLS,xlab="ISI", ylab="Residuals",col=c("blue"), lty=0, cex=0.3, pch=19)
points(test_data$ISI,ResMLSValidation,col="red", lty=0, cex=0.3, pch=19)
legend("topleft", legend=c("Training","Validation"), col=c("blue","red"), lty=0, cex=0.5, pch=19)
# Mean Square Error for training data
mean((ResMLS)^2)
# Mean Square Error for validation data
mean((ResMLSValidation)^2)
# Relative Mean Square Error for validation data
mean((ResMLSValidation)^2) / mean((test_data$ISI)^2)

