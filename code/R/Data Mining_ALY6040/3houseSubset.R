##### 3 - Subset Selection for Multiple Linear Regression #####


#Use the regsubsets() function from the leaps library to perform subset selection.
library(leaps)

#Create a data frame containing only the variables to be included in the analyses.
king2 <- king[,c("sqrt_price", "sqft_living", "sqft_lot", "sqft_living15", "sqft_lot15", 
                 "bedrooms", "bathrooms",  "floors", "waterfront", "condition", "grade", 
                 "yr_built")]

##-- a. Best subset selection--##


#Perform best subset selection
best.fit1 <- regsubsets(sqrt_price ~ ., data=king2, nvmax=24) 
best.fit1
#Observe the variables associated with the best-fitting model of each size. The best fit is 
#quantified by RSS.
best.summary1 <- summary(best.fit1)
best.summary1

#Review the fit statistics for each model size. 
#The fit statistics provided are rsq, rss, adjr2, cp, and bic:
names(best.summary1)


#Examine the adjusted R^2 fit statistics:
best.summary1$adjr2  
plot(best.summary1$adjr2, xlab="Number of variables", ylab="Adjusted R^2", type="b")
#The fit increases sharply at 3 predictors, then levels off
which.max(best.summary1$adjr2)   
#The best fit based on adjr2 occurs when all 11 predictors are included in the model.
best.summary1$adjr2[11]          
#With all 11 predictors, adjusted R^2 = 0.6842.

#Display the variables used for each model size, ranked from best to worst fit based on adj R^2.
plot(best.fit1, scale="adjr2")


#Examine the BIC fit statistics:
best.summary1$bic
plot(best.summary1$bic, xlab="Number of variables", ylab="BIC", type="b")
#The fit improves dramatically at 3 or 4 predictors, then begins to level off
which.min(best.summary1$bic)   
#The best fit based on bic occurs when 10 predictors are included in the model.
best.summary1$bic[10]          
#With 10 predictors, bic = -24811.35.

#Display the variables used for each model size, ranked from best to worst fit based on BIC
plot(best.fit1, scale="bic")


#A model with four predictors appears to have a good fit while also having few predictors.
#Identify the coefficients of the best fitting model with four predictors:
coef(best.fit1, 4)    
# sqft_living    waterfront         grade      yr_built 
#  0.08815258  303.21987532   90.43340751   -2.00430520 



##-- b. Forward stepwise selection--##


#Perform forward stepwise selection
fwd.fit1 <- regsubsets(sqrt_price ~ ., data=king2, nvmax=11, method="forward")
fwd.summary1 <- summary(fwd.fit1)
fwd.summary1


#Examine the adjusted R^2 fit statistics:
fwd.summary1$adjr2
plot(fwd.summary1$adjr2, xlab="Number of variables", ylab="Adjusted R^2", type="b")
#As with best subset selection, the fit increases sharply at 3 predictors, then levels off.
which.max(fwd.summary1$adjr2)   
#The best fit based on adjr2 occurs when all 11 predictors are included in the model.
fwd.summary1$adjr2[11]          
#With all 11 predictors, adjusted R^2 = 0.6842.


#Examine the BIC fit statistics:
fwd.summary1$bic
plot(fwd.summary1$bic, xlab="Number of variables", ylab="BIC", type="b")
#The fit improves dramatically at 3 or 4 predictors, then begins to level off
which.min(fwd.summary1$bic)   
#The best fit based on bic occurs when 10 predictors are included in the model.   
fwd.summary1$bic[10]          
#With 10 predictors, bic = -24811.35.



##-- c. Backward stepwise selection--##


#Perform backward stepwise selection
bwd.fit1 <- regsubsets(sqrt_price ~ ., data=king2, nvmax=11, method="backward")
bwd.summary1 <- summary(bwd.fit1)
bwd.summary1


#Examine the adjusted R^2 fit statistics:
bwd.summary1$adjr2
plot(bwd.summary1$adjr2, xlab="Number of variables", ylab="Adjusted R^2", type="b")
#As with best subset selection and forward stepwise selection, the fit increases 
#sharply at 3 predictors, then levels off.
which.max(bwd.summary1$adjr2)   
#The best fit based on adjr2 occurs when all 11 predictors are included in the model.
bwd.summary1$adjr2[11]          
#With all 11 predictors, adjusted R^2 = 0.6842.


#Examine the BIC fit statistics:
bwd.summary1$bic
plot(bwd.summary1$bic, xlab="Number of variables", ylab="BIC", type="b")
#The fit improves dramatically at 3 or 4 predictors, then begins to level off
which.min(bwd.summary1$bic)   
#The best fit based on bic occurs when 10 predictors are included in the model.   
bwd.summary1$bic[10]          
#With 10 predictors, bic = -24811.35.



##-- d. Choosing a model using the validation set approach --##


#From the best subset selection that was performed earlier, the best-fitting model, based on 
#adjusted R^2, included all 11 predictors, or, based on BIC, included 10 predictors, However, 
#we should perform cross-validation because we might be over-fitting the data.

#First, split the observations into a training set and a test set. 
set.seed(9)
samp <- sample(nrow(king2), nrow(king2)/2)
train <- king2[samp,]
test <- king2[-samp,]

#Perform best subset selection on the training set.
best.fit2 <- regsubsets(sqrt_price ~ ., data=train, nvmax=11)
best.summary2 <- summary(best.fit2)
best.summary2

#Compute the test set error for the best model of each model size. First create a model matrix 
#from the test set data.
test.mat <- model.matrix(sqrt_price ~ ., data=test)
head(test.mat)

#For each model size, extract the coefficients from best.fit2, multiply them into the 
#appropriate columns of the test model matrix to form the predictions, compute the test MSE, 
#and add the test MSE to a vector called val.errors.
val.errors <- rep(NA, 11)
for (i in 1:11){
  coefi <- coef(best.fit2, id=i)
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((test$sqft_living-pred)^2)
}
val.errors

#Plot the test MSE for each model size
plot(val.errors, xlab="number of variables", ylab="test MSE", type="b")

#Determine the model size for which test MSE is smallest.
which.min(val.errors)   #number of predictors = 1
val.errors[1]           #test MSE with 1 predictor = 2471200

#Based on the test set errors, the model with only one predictor fits the best, in contrast to 
#what we found in the previous analyses in which a model with many more predictors fit best.
#Now obtain the coefficients for the model with one predictor.
coef(best.fit2, 1)    #Intercept = 370.70, sqft_living = 0.16 

#Finally, to obtain the most accurate model coefficients, perform a linear regression on the 
#entire data set using the predictor obtained for the best 1-variable model.
best.lm1 <- lm(sqrt_price ~ ., data=king2)
summary(best.lm1)
best.lm <- lm(sqrt_price ~ sqft_living, data=king2)
summary(best.lm)    #Intercept = 375.6, sqft_living = 0.159 



##-- e. Choosing a model using 10-fold cross-validation--##


#Create a function to calculate test set mean square error (test MSE).
#(Credit to "Introduction to Statistical Learning, with Applications in R", written by
#James, Witten, Hastie, & Tibshirani)
predict.regsubsets <- function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}

#Create a vector that assigns each observation to one of k=10 folds.  
set.seed(9) 
folds <- sample(1:10, nrow(king2), replace=TRUE)
table(folds)
mean(table(folds))    #average of 2161.3 observations per fold

#Create a 10x11 matrix in which to store the cross-validation errors. Each row corresponds to
#one of 10 folds, and each column corresponds a model size.
cv.errors <- matrix(NA, 10, 11, dimnames=list(NULL, paste(1:11)))

#Write a loop that performs cross-validation, using the predict.regsubsets() function above.
#This loop calculates the MSE for each fold within each model size and then adds it to the 
#appropriate place in the cv.errors matrix.
for(k in 1:10){
  best.fit3 = regsubsets(sqrt_price ~ ., data=king2[folds!=k,], nvmax=11)   
  #run model on all data except those in the kth fold
  for(i in 1:11) {
    pred = predict(best.fit3, king2[folds==k,], id=i) 
    #using the model that was just run, predict outcomes for data in the (k)th fold 
    cv.errors[k,i] = mean((king2$sqrt_price[folds==k]-pred)^2)
    #calculate the MSE and add it to the appropriate place in the matrix
  }
}
cv.errors

#Average over the columns (i.e., average over the k=10 folds for each of the 12 model sizes) 
#to obtain the CV error for each of the model sizes. 
mean.cv <- apply(cv.errors, 2, mean)
mean.cv
plot(mean.cv, type="b")
which.min(mean.cv)    #the lowest CV error occurs with all 11 predictors
mean.cv[11]           #CV error for 11 predictors = 13036.95

#Perform best subset selection on the full data to obtain the most accurate coefficients for the 
#10-variable model.
best.fit4 <- regsubsets(sqrt_price ~ ., data=king2, nvmax=11)
coef(best.fit4, 11)    
