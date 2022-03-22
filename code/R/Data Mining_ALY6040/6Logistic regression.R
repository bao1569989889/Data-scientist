##### 6 - Logistic regression #####


#To fit logistic regression and other classification models to the data, we need to dichotomize 
#the price variable in the king data set. #In a new variable called price2, assign a value of 0 
#to observations in which price < $600,000, and assign a value of 1 to observations in which
#price >= $600,000. 
king$price2 <- ifelse(king$price < 600000, 0, 1)
table(king$price2)    
#15247 houses classified as 0
#6366 houses classified as 1
king$price2 <- as.factor(king$price2)

#(*Important note - I am dichotomizing the price variable only for the purpose of demonstrating
#classification modeling techniques. For real applications, it is not recommended that you dichotomize
#continuous factors because you lose a lot of variance in the factor. You are better off sticking to 
#methods that are suited for modeling continous outcomes.)



##-- a. Logistic regression --##


#Use the glm() function to fit a logistic regression model, predictiong price2 from sqft_living, 
#grade, and waterfront.
log.fit1 <- glm(price2 ~ sqft_living + grade + waterfront, data=king, family=binomial)
summary(log.fit1)

#Obtain a vector of the predicted probability of each observation, based on the model. 
#These values represent the probability of each observation having price2 = 1 (i.e., the probability
#that the price of the house is >= $600,000).
log.prob1 <- predict(log.fit1, type="response")

#Create vector of class predictions based on probability values. Assign to class 0 if the 
#probability is <= .5, and assign to class 1 if the probability is > .5.
log.pred1 <- rep("0", nrow(king))
log.pred1[log.prob1>.5] <- "1"
table(log.pred1)    
#16754 observations predicted to be in class 0
#4859 observations predicted to be in class 1

#Compare the predicted classifications to the observed data to determine the accuracy of the model.
table(predicted=log.pred1, observed=king$price2)
mean(log.pred1==king$price2)    #accuracy rate = 82.95%
mean(log.pred1!=king$price2)    #error rate = 17.05% 



#-- b. Logistic regression with cross-validation --##


#Use cross-validation to determine how well a logistic regression model performs on a test set. 
#First, split the data into a training set and a test set.
set.seed(9)
train <- sample(nrow(king), nrow(king)/2)
test <- king[-train,]

#Fit a logistic regression on the training set.
log.fit2 <- glm(price2 ~ sqft_living + grade + waterfront, data=king[train,], family=binomial)
summary(log.fit2)

#Based on the model fitted on the training set, obtain predicted probabilities on the test set.
log.prob2 <- predict(log.fit2, test, type="response")

#Create vector of class predictions based on probability values.
log.pred2 <- rep("0", length(log.prob2))
log.pred2[log.prob2>.5] <- "1" 
table(log.pred2)    
#8416 observations predicted to be in class 0
#2391 observations predicted to be in class 1

#Compare the predicted classifications to the observed data in order to determine the model's accuracy.
table(predicted=log.pred2, observed=test$price2)
mean(log.pred2==test$price2)    #accuracy rate = 82.85%
mean(log.pred2!=test$price2)    #error rate = 17.15% 

#Finally, you can use the predict() function to predict the classification based on  particular 
#values of sqft_living, grade, and waterfront. For example, for a house in which sqft_living=2000
#and grade=7, what is the probability of being in class 1 (i.e., the price being >= $600,00) if
#it has a waterfront view versus if it does not have a waterfront view?
predict(log.fit2, newdata=data.frame(sqft_living=c(2000,2000), grade=c(7,7), waterfront=c(0,1)),
        type="response")    
#with waterfront=0 (i.e., no view), probability=0.12
#with waterfront=1 (i.e., has a view), probability=0.76



##-- c. Logistic polynomial regression --##


#As with linear regression, we can add polynomial terms to logistic regression. We'll use polynomial
#logistic regression to predict price2 from bedrooms, starting with a degree-2 polynomial.
log.fit1 <- glm(price2 ~ poly(bedrooms, 2), data=king, family=binomial)
summary(log.fit1)

#Obtain predicted probabilities for all observations in the data set.
log.prob1 <- predict(log.fit1, type="response")

#Create a vector of class predictions based on probability values. Assign an observation to class 0 
#if the probability is <= .5, and assign an observation to class 1 if the probability is > .5.
log.pred1 <- rep("0", nrow(king))
log.pred1[log.prob1>.5] <- "1" 
table(log.pred1)    
prop.table(table(log.pred1))    
#19683 observations (or 91.07%) predicted to be in class 0
#1930 observations (or 8.93%) predicted to be in class 1

#For reference, calculate the prior probabilities based on the observed data:
prop.table(table(king$price2))    
#70.55% of observations are in class 0
#29.45% of observations are in class 1 

#Calculate the classification accuracy by comparing the predictions to the observed data.
table(predicted=log.pred1, observed=king$price2)
mean(log.pred1==king$price2)    #accuracy rate = 71.15%
mean(log.pred1!=king$price2)    #error rate = 28.85%

#Considering that the prior probability of being classified as "0" is 70.55%, this model does 
#not perform much better than a model that would classify every observation as "0".



##-- d. Logistic polynomial regression with the validation set approach --## 


#Select the degree of polynomial to use in the logistic regression model by using the validation set 
#approach. First, split the data into a training set and a test set: 
set.seed(9)
samp <- sample(nrow(king), nrow(king)/2)
train <- king[samp,]
test <- king[-samp,]

#Run a loop to calculate the classification accuracy in the test set for each degree of polynomial. 
#As in the previous section, we'll use a categorization threshold of 50%. 
accuracy <- rep(NA, 5)
for (i in 1:5){
  log.fit <- glm(price2 ~ poly(bedrooms, i), data=train, family=binomial)  
  prob <- predict(log.fit, test, type="response")
  pred <- rep("0", nrow(test))
  pred[prob>.5] <- "1"
  accuracy[i] <- mean(pred==test$price2)
}
accuracy    #[0.7133340, 0.7132414, 0.7133340, 0.7133340, 0.7125937]

#There is not much difference between the models. Again, the prior probability of being in class 0 
#is 70.55%, so the models do not perform much better than if we had simply predicted that every 
#observation is in class 0. We could try changing the classification threshold to some other value
#besides 50% and then observe how the prediction accuracy changes. However, I'm not going to do that 
#here. 



##-- e. Plotting the logistic polynomial regression model --##


#Next, I'll plot the degree-2 logistic regression model (log.fit1) that we created in section c. 
#I'll plot the data along with the prediction line and the 2*SE (standard error) bands. 

#First, create a vector of integers to represent the range of values in bedrooms. Then calculate 
#the predicted values and the 2*SE bands for each value of bedrooms. 
#(Note - Predictions are in terms of log-odds. You can select type="response" to obtain predictions in 
#terms of probabilities, but you might end up with negative probabilities for the standard error bands.)
bed.range <- 0:11
log.pred1 <- predict(log.fit1, newdata=list(bedrooms=bed.range), se=TRUE) 
lower <- log.pred1$fit+2*log.pred1$se.fit
upper <- log.pred1$fit-2*log.pred1$se.fit
se.bands <- cbind(lower, upper)

#Transform the predictions and SE bands from log-odds to probabilities.
#The equation is pr = [e^(log-odds)]/[1 + e^(log-odds)] 
probs <- exp(log.pred1$fit)/(1+exp(log.pred1$fit))
probs.se <- exp(se.bands)/(1+exp(se.bands))   

#The price2 variable is a factor which is coded by R as 1's and 2's rather than as 0's and 1's when 
#plotted:
plot(king$bedrooms, king$price2)

#Therefore, we need to transform price2 into a numeric variable that is coded as 0's and 1's:
price2.num <- as.numeric(king$price2)
price2.num <- price2.num-1

#Now plot the data using the new numeric price2.num variable, then add the prediction line
#and standard error bands.
plot(king$bedrooms, price2.num, type="n", xlab="number of bedrooms", ylab="price (>$600,000)")
points(jitter(king$bedrooms), price2.num, pch="|")   
lines(bed.range, probs, lwd=2, col="blue")
matlines(bed.range, probs.se, lwd=1, col="blue", lty=2)
