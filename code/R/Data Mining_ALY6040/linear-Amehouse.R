install.packages("glmnet")
install.packages("xgboost")
library(tidyverse)
library(caret)
library(MASS) #Box-cox
library(car) #Vif
library(glmnet) #LASSO, Ridge
library(xgboost)
#drop ID, sold_Date,sale-data
Amehouse4<- Amehouse3[,c(3:21,23:24)]

# Preprocessing: scale and center
params <- preProcess(Amehouse4[c(-1)], method=c("center", "scale"))
newdata <- predict(params, Amehouse4)

# Train-Test Split
set.seed(123456)
index <- sample(1:nrow(newdata), 0.8*nrow(newdata)) 
train <- newdata[index,]
validation <- newdata[-index,]

# Evaluations
RMSE <- function(x, y){
  sqrt(mean((x-y)^2))
}

R2 <- function(actual, pred){
  rss <- sum((pred-actual)^2)
  tss <- sum((actual-mean(actual))^2)
  r2 <- 1 - rss/tss
  return(r2)
}

# Linear Regression
lm.fit <- lm(Sales_Price~., data=newdata)
# summary(lm.fit)
par(mfrow=c(1,2))
# Assumption of linearity failed, transformation needed!
qqnorm(newdata$Sales_Price)
qqline(newdata$Sales_Price, col=2)

lm.res <- resid(lm.fit)
plot(lm.fit$fitted.values, lm.res, xlab="Fitted value", ylab="Residuals")
abline(h=0,col="red")

# Box-cox Trans
par(mfrow=c(1,1))
set.seed(123456)
bc <- boxcox(lm.fit, lambda=seq(-1,1,0.1))

best.lam <- bc$x[which(bc$y==max(bc$y))]
best.lam

lm.fit <- lm((Sales_Price)^best.lam~., data=newdata)

par(mfrow=c(1,2))
qqnorm(newdata$Sales_Price^best.lam)
qqline(newdata$Sales_Price^best.lam, col=2)
lm.res <- resid(lm.fit)
plot(lm.fit$fitted.values, lm.res, xlab="Fitted value", ylab="Residuals")
abline(h=0,col="red")

# Update new dataset with Box-cox trans
newtrain <- train
newval <- validation
newtrain$price <- (newtrain$Sales_Price)^best.lam
newval$price <- (newval$Sales_Price)^best.lam

# 10-fold CV
train.control <- trainControl(method="cv", number=10,
                              savePredictions="all")
#Linear Regression
par(mfrow=c(1,1))
lm.model <- train(Sales_Price~., data=newtrain, method="lm", trControl=train.control)
print(lm.model)
summary(lm.model)

lm.fit <- lm(Sales_Price~., data=newtrain)
vif(lm.fit)
