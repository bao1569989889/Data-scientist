##### 5 - Principal Components Regression (PCR) & Partial Least Squares (PLC) #####


#Open the pls library to run PCR and PLC
install.packages("pls")
library(pls)

#Create a data frame containing only the variables to be included in the analyses.
king2 <- king[,c("sqrt_price", "sqft_living", "sqft_lot", "sqft_living15", "sqft_lot15", 
                 "bedrooms", "bathrooms",  "floors", "waterfront", "condition", "grade", 
                 "yr_built")]



##-- a. Principal components regression (PCR) --##


#Run principle components regression using the pcr() function from the pls library. 
#Set scale=TRUE to standardize each predictor, and validation="CV" to compute 10-fold CV for 
#each possible value of M, the number of principal components used in the regression.
set.seed(9)
pcr.fit1 <- pcr(sqrt_price ~ ., data=king2, scale=TRUE, validation="CV")

#The summary provides two pieces of information for each number of components: 
#1) the CV error in terms of RMSE and 2) the % variance explained in the predictors and in 
#the outcome variables. 
summary(pcr.fit1) 
#The lowest CV error occurs with 10 or 11 components (RMSE = 114.2).    

#Plot the CV errors for each number of components in the regression using the validationplot() 
#function. Set val.type="MSEP" to plot MSE instead of RMSE.
validationplot(pcr.fit1, val.type="MSEP", type="b", pch=20)



##-- b. PCR with cross-validation --##


#Separate the data into a training set and a test set. 
set.seed(9)
samp <- sample(nrow(king2), nrow(king2)/2)
train <- king2[samp,]
test <- king2[-samp,]

#Perform PCR on the training set
set.seed(9)
pcr.fit2 <- pcr(sqrt_price ~ ., data=train, scale=TRUE, validation="CV")
summary(pcr.fit2)
validationplot(pcr.fit2, val.type="MSEP", type="b", pch=20)

#The lowest CV error occurs when either 10 or 11 components are used (RMSE = 114.5).
#Compute the MSE on the test set using 10 components. 
pcr.pred2 <- predict(pcr.fit2, test, ncomp=10)
mean( (test$sqrt_price-pcr.pred2)^2 )    #test MSE = 13015.64

#Fit the PCR on the full data set to examine the % variance explained when 10 components
#are included in the model. 
pcr.fit3 <- pcr(sqrt_price ~ ., data=king2, scale=TRUE)
summary(pcr.fit3)
# % variance explained in x = 98.71%
# % variance explained in y = 68.42%



##-- c. Partial least squares (PLS) with cross-validation --##


#Use the plsr() function from the pls library to run PLS on the training set. 
set.seed(9)
pls.fit1 <- plsr(sqrt_price ~ ., data=train, scale=TRUE, validation="CV")
summary(pls.fit1)     #lowest CV error with 5-11 components (114.5)  

#Plot the CV errors
validationplot(pls.fit1, val.type="MSEP", type="b", pch=20)

#Compute the MSE on the test set using 5 components
pls.pred1 <- predict(pls.fit1, test, ncomp=5)
mean( (test$sqrt_price-pls.pred1)^2 )    #test MSE = 13020.61

#Fit the PCR on the full data set to examine the % variance explained when 5 componenets are
#included in the model. 
pls.fit2 <- plsr(sqrt_price ~ ., data=king2, scale=TRUE)
summary(pls.fit2)   
# % variance explained in x = 74.57%
# % variance explained in y = 68.40%
