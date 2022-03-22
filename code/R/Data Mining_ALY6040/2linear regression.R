##### 2 - Simple and Multiple Linear Regression ####


##-- a. Simple linear regression--##


#Regress sqrt_price on sqft_living.
#(I am using sqrt_price rather than price as the outcome variable because price is heavily
#skewed.)
lm.fit1 <- lm(sqrt_price ~ sqft_living, data=king)
summary(lm.fit1)   
confint(lm.fit1)
# model fit:  R^2 = .52, F(1, 21611) = 2.315e4, p < .001
# coefficient of sqft_living:  b = 0.159, t = 152.1, p < .001, 95% CI = [0.157, 0.161]

#Examine the diagnostic plots.
#(The QQ plot shows that the data are still skewed, but it is better than it would be if 
#I regressed price instead.)
par(mfrow=c(2,2))
plot(lm.fit1)    

#Identify values with strong leverage
plot(hatvalues(lm.fit1))
which.max(hatvalues(lm.fit1))    
#largest leverage point for observation 12778
sort(hatvalues(lm.fit1), decreasing=TRUE)
#Observations 12778 and 7253 have high leverage relative to the other observations

#Plot the regression line along with the confidence region around the line.
library(ggplot2)

#First calculate the confidence region.
confidence <- predict(lm.fit1, interval="confidence")
df <- cbind(king$sqft_living, king$sqrt_price, confidence)
df <- as.data.frame(df)
colnames(df) <- c("sqft_living", "sqrt_price", "fit", "lwr", "upr")

#Now plot the data and regression line
ggplot(df, aes(x=sqft_living, y=sqrt_price)) +
  geom_point(size=.5, alpha=.3) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.2) +
  geom_line(aes(x=sqft_living, y=fit), color="red") +
  scale_x_continuous(breaks=c(2000, 4000, 6000, 8000, 10000, 12000)) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  annotate("text", label="R^2 = .5172", x=12000, y=400) +
  labs(title=
         "Relationship between sqft_living and sqrt_price,\n with fitted linear regression line") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))



##-- b. Multiple linear regression--##

#Regress sqrt_price on sqft_living and grade
lm.fit2 <- lm(sqrt_price ~ sqft_living + grade, data=king)
summary(lm.fit2)    
confint(lm.fit2)
# model fit:  Adj R^2 = .58, F(2, 21610) = 1.483e4, p < .001
# coefficient of sqft_living:  b = .094, t = 62.55, p < .001, 95% CI = [.091, .097]
# coefficient of grade:  b = 66.15, t = 56.08, p < .001, 95% CI = [63.839, 68.464]

#Examine the diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit2)

#Identify values with strong leverage
plot(hatvalues(lm.fit2))
which.max(hatvalues(lm.fit2))    
#largest leverage point for observation 12778
sort(hatvalues(lm.fit2), decreasing=TRUE)  
#high leverage among observations 12778 and 7253

#Determine the confidence prediction intervals for a house with average sqft_living and 
#average grade.
mean(king$sqft_living)
mean(king$grade)
predict(lm.fit2, data.frame(sqft_living=c(2079.9), grade=c(7.66)), interval="confidence")
#      fit      lwr      upr
# 706.5021 704.7442 708.2599
predict(lm.fit2, data.frame(sqft_living=c(2079.9), grade=c(7.66)), interval="prediction")
#      fit     lwr      upr
# 706.5021 448.074 964.9301



##-- c. Linear polynomial regression--##


#Regress sqrt_price onto a quadratic function of sqft_living
poly.fit2 <- lm(sqrt_price ~ poly(sqft_living, 2), data=king)
summary(poly.fit2)    #R^2 = .5203
confint(poly.fit2)
# model fit:  Adj R^2 = .52, F(2, 21610) = 1.172e4, p < .001
# coefficient of linear term:  b = 2.147e4, t = 152.65, p < .001, 
#                              95% CI = [21193.39, 21744.74]
# coefficient of quadratic term: b = 1.680e3, t = 11.94, p < .001, 
#                                95% CI = [1404.27, 1955.62]

#Regress sqrt_price onto a cubic function of sqft_living
poly.fit3 <- lm(sqrt_price ~ poly(sqft_living, 3), data=king)
summary(poly.fit3)    #R^2 = .5244
confint(poly.fit3)
# model fit:  Adj R^2 = .52, F(3, 21609) = 7944, p < .001
# coefficient of linear term:  b = 21469.07, t = 153.30, p < .001, 
#                              95% CI = [21194.57, 21743.57]
# coefficient of quadratic term: b = 1679.94, t = 12.00, p < .001, 
#                                95% CI = [1405.44, 1954.44]
# coefficient of cubic term: b = -1913.90, t = -13.67, p < .001, 
#                                95% CI = [-2188.40, -1639.40]

#Determine whether adding polynomial terms significantly improves the model fit
anova(lm.fit1, poly.fit2, poly.fit3)  
#The fit improves significantly with the addition of increasingly higher order polynomial
#terms, but the change in RSS is only tiny.

#Plot the fitted degree-2 polynomial line with confidence regions. 
confidence2 <- predict(poly.fit2, interval="confidence")
df2 <- cbind(king$sqft_living, king$sqrt_price, confidence2)
df2 <- as.data.frame(df2)
colnames(df2) <- c("sqft_living", "sqrt_price", "fit", "lwr", "upr")
head(df2)

ggplot(df2, aes(x=sqft_living, y=sqrt_price)) +
  geom_point(size=.5, alpha=.3) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.2) +
  geom_line(aes(x=sqft_living, y=fit), color="red") +
  scale_x_continuous(breaks=c(2000, 4000, 6000, 8000, 10000, 12000)) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500, 3000)) +
  annotate("text", label="R^2 = .5203", x=12000, y=400) +
  labs(title=
         "Relationship between sqft_living and sqrt_price,\n with fitted polynomial regression line") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))
theme_bw()

dev.off()

##-- d. Using the validation set approach to select a polynomial term --##


#Although a linear terms seems to describe the relationship between sqft_living and sqrt_price
#fairly well, I'll demonstrate how to perform cross-validation using the validation set approach
#to select among higher degree polynomial terms. We'll separate the data into a training and 
#test set, use a loop to fit polynomial models on the training set, then calculate how well 
#those models fit on the test set.

#First, separate the data into a training set and a test set.
set.seed(9)
samp <- sample(nrow(king), nrow(king)/2)
train <- king[samp,]
test <- king[-samp,]

#Now write a loop to fit models with polynomial terms from 1 to 5 on the training set, then 
#compute the training set mean square error (MSE) and test set MSE for each of these models,
#then add the errors to the trainMSE and testMSE vectors.
trainMSE <- rep(0, 5)
testMSE <- rep(0, 5)
for (i in 1:5){
  lm.fit <- lm(sqrt_price ~ poly(sqft_living, i), data=train)
  trainMSE[i] <- mean( (train$sqrt_price - predict(lm.fit, train))^2 )
  testMSE[i] <- mean( (test$sqrt_price - predict(lm.fit, test))^2 ) 
}

#Examine the training set and test set MSEs:
trainMSE    #[20031.51, 19731.88, 19687.58, 19671.46, 19671.46]   
testMSE     #[19794.82, 19918.86, 19644.44, 19827.91, 19837.57]

#Plot the training MSE and test MSE
degree <- 1:5
plot(degree, trainMSE, type="b", col="blue", ylim=c(19600, 20100))
lines(degree, testMSE, type="b", col="red")
legend("topright", legend=c("training set", "test set"), col=c("blue", "red"), pch=1)

#The lowest test set MSE occurs with a polynomial of degree 3 (MSE = 19644.44). 



##-- e. Using k-fold cross-validation to select a polynomial model --##


#Instead of separating the data into a single training set and single test set, we can perform
#k-fold cross-validation to select a polynomial term. Here, I'll perform k-fold CV with k=10. 

#Use the cv.glm() function from the boot library to conduct K-fold CV
library(boot)

#Write a loop to compute the k-fold CV error for polynomials up to degree five, using k=10. 
#Then add the cv error for each polynomial term to the cv.error vector.
#(Note - we have to use the glm() function rather than the lm() function because the cv.glm()
#function can't read lm() objects.)
cv.error <- rep(0, 5)
set.seed(9)
for (i in 1:5){
  glm.fit <- glm(sqrt_price ~ poly(sqft_living, i), data=king)
  cv.error[i] <- cv.glm(king, glm.fit, K=10)$delta[1]
}
cv.error    #[19921.28, 19843.60, 19709.00, 19862.91, 19975.88]

#plot the errors
plot(degree, cv.error, type="b")

#Again, the lowest CV error occurs with a polynomial of degree 3 (cv error = 19709.00). 
#Fit a degree-3 polynomial on the entire data set to obtain the coefficients of the model and 
#model fit:
lm.fit3 <- lm(sqrt_price ~ poly(sqft_living, 3), data=king)
summary(lm.fit3)

#Final Note on cross-validation - you can combine the validation set approach and k-fold cross 
#validation to select a model rather than using each of the methods separately. First, split the 
#data into a training set and a test set as you would for the validation set approach. Then run 
#k-fold cross-validation on the training set, select the best model based on cv error, then
#validate that model by fitting it on the test set and computing the test set error. 



##-- f. Plotting the linear polynomial regression model --##


#Next, I'll plot the degree-3 polynomial model (lm.fit3) that we created in the previous section. 
#I'll plot the data along with the prediction line and the 2*SE (standard error) bands. 
#First, create an evenly spaced sequence of 100 predictor values:
xmin <- min(king$sqft_living)
xmax <- max(king$sqft_living)
xrange <- seq(xmin, xmax, length.out=100)

#Next, calculate the predicted outcome value (i.e., predicted values of sqrt_price) and standard
#error for each value in xrange:
lm.pred3 <- predict(lm.fit3, newdata=list(sqft_living=xrange), se=TRUE)

#Calculate the 2*SE bands:
lower <- lm.pred3$fit+2*lm.pred3$se.fit
upper <- lm.pred3$fit-2*lm.pred3$se.fit
se.bands <-cbind(lower, upper)

#Now plot the data along with the prediction line and standard error bands:
plot(king$sqft_living, king$sqrt_price, xlab="sqft_living", ylab="sqrt_price")
lines(xrange, lm.pred3$fit, lwd=2, col="blue")
matlines(xrange, se.bands, lwd=1, col="blue", lty=2)

#From the plot, it appears that some outlier values in the upper range of sqft_living are 
#influencing the curve of the line. It would be worth removing some of these observations and 
#refitting the model to determine whether the model changes substantially.

