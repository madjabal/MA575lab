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

attach(Data)

D <- cbind(X,Y, month, day, temp, RH, wind, rain, area, ISI)
pairs(D, gap=0.4, pch = 19, cex = 0.1)

c <- cor(D)
round(c,3)

# + day + month + I(wind^2) + I(wind^3) + I(wind^4)

lm.ols <- lm(ISI ~ temp + wind + RH)
summary(lm.ols)


StanResMLS <- rstandard((lm.ols))

# Plot against ISI shows pattern and reveals low quality model
plot(ISI, StanResMLS, pch = 19, cex = .5)

# This plot against the predicted shows no pattern however
plot(fitted(lm.ols), StanResMLS, xlab='Fitted Values', pch = 19, cex = 0.5)

