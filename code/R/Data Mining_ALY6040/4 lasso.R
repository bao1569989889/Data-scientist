##### 4 - Ridge Regression and Lasso #####


##Use the glmnet() function from the glmnet library to run ridge regression and lasso. 
library(glmnet)

#Create a data frame containing only the variables to be included in the analyses.
king2 <- king[,c("sqrt_price", "sqft_living", "sqft_lot", "sqft_living15", "sqft_lot15", 
                 "bedrooms", "bathrooms",  "floors", "waterfront", "condition", "grade", 
                 "yr_built")]



##-- a. Ridge regression--##


#The glmnet() function takes an x matrix and a y vector. First create a model matrix for x 
#and a vector for y.
x <- model.matrix(sqrt_price ~ ., data=king2)
y <- king2$sqrt_price

#Run the ridge regression by specifying alpha=0. Note that glmnet() automatically standardizes
#the variables. By default, glmnet() performs ridge regression for an automatically selected 
#range of lambda values. 
ridge.fit1 <- glmnet(x, y, alpha=0)

#Examine the range of lambda values used.
range(ridge.fit1$lambda)    #[14.60346 - 146034.55801]   

#Associated with each value of λ is a vector of ridge regression coefficients, stored in a matrix 
#that can be accessed by coef(). 
dim(coef(ridge.fit1))    #13 rows, 100 columns
coef(ridge.fit1)    
#(There should only be 12 rows, one for each of the 11 predictors, plus the intercept, 
#but there is another empty row for the intercept. I don't know why.)

#Plot the coefficients against log(lambda). Notice how the coefficients shrink with increasing
#values of lambda.
plot(ridge.fit1, xvar="lambda", label=TRUE)

#When lambda is large (and correspondingly, the L2 norm is small), the coefficient estimates 
#should become much smaller, compared to when lambda is small, because the larger lambda should
#have forced the coefficients to shrink.
ridge.fit1$lambda[1]    #lambda = 146034.6
coef(ridge.fit1)[,1]   
#notice that the coefficients are really small at a very large lambda value
ridge.fit1$lambda[100]  #lambda = 14.60346
coef(ridge.fit1)[,100] 
#notice that the coefficients are larger at a small lambda value

#You can obtain the L2 norm corresponding to a given value of lambda by taking the square root 
#of the sum of the squared coefficients, excluding the intercept. Larger values of lambda are
#associated with smaller L2 norms.
sqrt(sum(coef(ridge.fit1)[-c(1,2),1]^2))    
#when lambda=146034.6, the L2 norm = 1.157094e-05    
sqrt(sum(coef(ridge.fit1)[-c(1,2),100]^2))  
#when lambda=14.60346, the L2 norm = 298.5211

#You can calculate the L2 norm corresponding to each value of lambda, and store it in a vector:
L2norms <- sqrt( apply((ridge.fit1$beta[-1,]^2), 2, sum) )



##-- b. Ridge regression with cross-validation --##


#To select the best value of lambda for the ridge regression model, you have to perform 
#cross-validation. First, split the data into a training set and a test set.
set.seed(9)
samp <- sample(nrow(king2), nrow(king2)/2)
train <- king2[samp,]
test <- king2[-samp,]

#For the training and test sets, create a model matrix for x and a vector for y.
xtrain <- model.matrix(sqrt_price ~ ., data=train)
ytrain <- train$sqrt_price
xtest <- model.matrix(sqrt_price ~ ., data=test)
ytest <- test$sqrt_price

#Select a value of lambda by running the cv.glmnet() cross-validation function on the training
#set. The cv.glmnet() function performs 10-fold CV by default.
set.seed(9)
ridge.cv2 <- cv.glmnet(xtrain, ytrain, alpha=0)

#Identify the range of lambdas used.
range(ridge.cv2$lambda)    # [16.29313 - 148456.92700]

#Plot the mean squared error against log(lambda).
#(Note - The vertical line indicates the point at which the MSE is within 1 SE of the minimum.)
plot(ridge.cv2)    

#Identify the the minimum CV error and the lambda associated with that minimum value. 
which.min(ridge.cv2$cvm)    #lowest cvm at index 99
ridge.cv2$cvm[99]   #minimum CV error = 13163.05
bestlam2 <- ridge.cv2$lambda.min    
bestlam2    #lambda=16.29313 (the lowest value in the range)

#On the test set, obtain the MSE associated with the best value of lambda.
ridge.pred2 <- predict(ridge.cv2, s=bestlam2, newx=xtest)
mean((ytest-ridge.pred2)^2)    #test MSE = 13044.97

#Refit the ridge regression on the full data set, using the best value of lambda chosen by
#cross-validation.
ridge.fit2 <- glmnet(x, y, alpha=0, lambda=bestlam2)

#View the coefficients associated with the best value of lambda.
ridge.fit2$a0     
#intercept = 3736.697
ridge.fit2$beta 
#sqft_living    6.584599e-02
#sqft_lot       4.472939e-05
#sqft_living15  4.033972e-02
#sqft_lot15    -2.327191e-04
#bedrooms      -1.285066e+01
#bathrooms      3.290860e+01
#floors         2.374231e+01
#waterfront     2.806795e+02
#condition      1.586562e+01
#grade          6.731627e+01
#yr_built      -1.967834e+00


#The best lambda chosen was 16.29, the smallest value in the range. The range might not have been
#small enought to capture the best lambda value, so I'll re-run cv.glmnet(), but this time, I'll
#use a pre-specified grid of lambda values, which goes as low as λ = 0.01:
grid <- 10^seq(10, -2, length=100)  #100 values that range from 10^10 to 10^02

#Now re-run the analysis with the new grid of values:
set.seed(9)
ridge.cv3 <- cv.glmnet(xtrain, ytrain, alpha=0, lambda=grid)

#Plot the mean squared error against log(lambda).
plot(ridge.cv3)

#Identify the minimum cv error and the lambda associated with that minimum value.
which.min(ridge.cv3$cvm)  #lowest cvm at index 88
ridge.cv3$cvm[88]         #minimum cv error = 13097.58
bestlam3 <- ridge.cv3$lambda.min    
bestlam3    #best lambda = 0.2848036

#On the test set, obtain the MSE associated with the best value of lambda.
ridge.pred3 <- predict(ridge.cv3, s=bestlam3, newx=xtest)
mean((ytest-ridge.pred3)^2)    #test MSE = 13005.98

#Refit the ridge regression on the full data set, using the best value of lambda chosen by
#cross-validation.
ridge.fit3 <- glmnet(x, y, alpha=0, lambda=bestlam3)

#View the coefficients associated with the best value of lambda.
ridge.fit3$a0     
#intercept = 4210.723
ridge.fit3$beta   
#sqft_living    7.254571e-02
#sqft_lot       4.825702e-05
#sqft_living15  3.237017e-02
#sqft_lot15    -2.793335e-04
#bedrooms      -1.846551e+01
#bathrooms      3.330384e+01
#floors         2.450315e+01
#waterfront     2.872624e+02
#condition      1.452258e+01
#grade          7.674568e+01
#yr_built      -2.289031e+00



##-- c. Lasso-- ##


#Fit a lasso model by specifying alpha=1 in the glmnet function. Feed the function the same 
#x matrix and y vector that we had created for running ridge regression:
lasso.fit1 <- glmnet(x, y, alpha=1)

#Examine the range of lambda values used.
range(lasso.fit1$lambda)    #[0.2612046 - 146.0345580]   

#Examine the matrix of coefficients. Each row represents a coeffient for one of the predictors,
#and each column represents one of the lambda values.
#(Notice that many of the coefficients have been shrunken to zero.)
dim(coef(lasso.fit1))    #13 row, 69 columns
coef(lasso.fit1)

#Plot the coefficients against log(lambda). Notice how the coefficients are being shrunk to 
#zero with increasing values of lambda.
plot(lasso.fit1, xvar="lambda", label=TRUE)

#As with ridge regression, when lambda is large (and correspondingly, the L1 norm is small), 
#the coefficient estimates should be smaller, and more of them should be equal to 0. Compare 
#the coefficients when lambda is large versus when it is small:
lasso.fit1$lambda[1]    #lambda = 146.0346
coef(lasso.fit1)[,1]    
#notice that all except one of the coefficients is zero.
lasso.fit1$lambda[69]  #lambda = 0.2612046
coef(lasso.fit1)[,69]  
#notice that the coefficents are larger with a smaller value of lambda.



##-- d. Lasso with cross-validation --##


#As with ridge regression, we have to perform cross-validation using cv.glmnet() to select the 
#best value of lambda for a lasso model. I'll use the same training and test set as I had created 
#for running cross-validation with ridge regression:
set.seed(9)
lasso.cv2 <- cv.glmnet(xtrain, ytrain, alpha=1)

#Identify the range of lambdas used.
range(lasso.cv2$lambda)    # [0.291427 - 148.456927]

#Plot the mean squared error against log(lambda).
#(Note - The vertical line indicates the point at which the MSE is within 1 SE of the minimum.)
plot(lasso.cv2)    

#Identify the the minimum CV error and the lambda associated with that minimum value. 
which.min(lasso.cv2$cvm)    #lowest cvm at index 68
lasso.cv2$cvm[68]      #cvm = 13099.51
lasso.cv2$lambda.min   #lambda = 0.291427 

#Since the best lambda is the minimum value of the range that the model automatically 
#generated, re-run cv.glmnet(), but this time using our prespecified grid of values.
grid <- 10^seq(10, -2, length=100)  #100 values that range from 10^10 to 10^02
set.seed(9)
lasso.cv3 <- cv.glmnet(xtrain, ytrain, alpha=1, lambda=grid)

#Again, plot the mean squared error against log(lambda).
plot(lasso.cv3)  

#Identify the the minimum CV error and the lambda associated with that minimum value. 
which.min(lasso.cv3$cvm)  #lowest cvm at index 100
lasso.cv3$cvm[100]        #minimum cv = 13097.67
lasso.cv3$lambda.min      #lambda = 0.01
#The best value of lambda is still the smallest in our specified range. Base on the plot, 
#we might be reaching an asymptote. The model seems to perform best when all variables are 
#included in the model. For now, we'll stick with lambda=0.01.

#On the test set, obtain the MSE associated with the best value of lambda.
bestlam3 <- lasso.cv3$lambda.min 
lasso.pred3 <- predict(lasso.cv3, s=bestlam3, newx=xtest)
mean((ytest-lasso.pred3)^2)    #test MSE = 13008.01

#Refit the lasso on the full data set, using the best value of lambda chosen by cross-validation.
lasso.fit3 <- glmnet(x, y, alpha=1, lambda=bestlam3)

#View the coefficients associated with the best value of lambda.
lasso.fit3$a0     
#intercept = 4331.952 
lasso.fit3$beta   #coefficients
#sqft_living    7.268738e-02
#sqft_lot       4.747425e-05
#sqft_living15  3.214313e-02
#sqft_lot15    -2.788454e-04
#bedrooms      -1.855279e+01
#bathrooms      3.327918e+01
#floors         2.448539e+01
#waterfront     2.872797e+02
#condition      1.447334e+01
#grade          7.695627e+01
#yr_built      -2.294799e+00
