##### 9 - Support Vector Machines #####


#Use the e1071 library to fit support vector classifiers and support vector machines.
library(e1071)

#We will use support vector classifiers (SVCs) and support vector machines (SVMs) to predict 
#our dichotomous price2 variable (see the "6 - logistic regression" script) based on the 
#condition and sqft_living predictors. First, create a separate data frame with only the 
#relevant variables:
svm.dat <- king[,c("price2", "sqft_living", "condition")]



##-- a. Support vector classifiers --##


#We will first fit a linear SVC. Plot the observations from svm.dat, colored by price2 class, 
#with condition and sqft_living plotted along the axes.
with(svm.dat, plot(condition, sqft_living, col=price2))

#Fit a support vector classifier to predict price2 by specifying kernel="linear". The cost 
#argument specifies the cost of a violation to the margin. Small costs will result in wide 
#margins and many violations of the margins, whereas large costs will result in narrow margins 
#and fewer violations of the margins. For now, we will use cost=10.
#(Note - the algorithm might take a while to run.)
svc.fit1 <- svm(price2 ~ sqft_living + condition, data=svm.dat, kernel="linear", cost=10, scale=T)

#Calling a summary of the model will show you the parameter values used in the model as well as
#the number of support vectors from each class:
summary(svc.fit1)    

#Plot the points along with the separating classifier.
plot(svc.fit1, svm.dat)  

#If you want to determine the identities of all the support vectors, you can call the index:
svc.fit1$index

#In the svc.fit1 model, we used cost=10. Now we'll perform cross-validation to select the best 
#model from a range of cost values. First split the data into a training set and test set.
set.seed(9)
samp <- sample(nrow(svm.dat), nrow(svm.dat)/2)
train <- svm.dat[samp,]
test <- svm.dat[-samp,]

#The tune() function performs 10-fold cross-validation to select from a range of specified 
#parameter values. Use this function to select from a a range of cost values:  
#(Note - this algorithm will take a while to run.)
svc.tune1 <- tune(svm, price2 ~ sqft_living + condition, data=train, kernel="linear",
                  ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))

#View the cross-validation errors for each of the models. 
summary(svc.tune1)

#Each cost value resulted in similar errors, but the best model occurred with cost=.01 and
#a corresponding error rate of 0.1844347.
bestmod <- svc.tune1$best.model
summary(bestmod)  

#Draw predictions on the test set using the best model obtained from the training set. 
svc.pred1 <- predict(bestmod, test)
table(svc.pred1)    
#8475 observations predicted to be in class 0
#2332 observations predicted to be in class 1

#Determine the classification accuracy.
table(predicted=svc.pred1, observed=test$price2)
mean(svc.pred1==test$price2)    #accuracy rate = 80.68%
mean(svc.pred1!=test$price2)    #error rate = 19.32%



##-- b. Support vector machines --##


#As with SVCs, use the svm() function from the e1071 package to fit an SVM with a non-linear 
#kernel. If we fit a polynomial kernel, then we specify a value for degree. If we fit a radial 
#kernel, then we specify a value for gamma. First, split the data into a training set and a 
#test set.
set.seed(9)
samp <- sample(nrow(svm.dat), nrow(svm.dat)/2)
train <- svm.dat[samp,]
test <- svm.dat[-samp,]

#You can use the svm() function to fit a model with specified values of the parameters. For
#example, here we fit an SVM with a radial kernel, using cost=1 and gamma=1.
svm.fit1 <- svm(price2 ~ sqft_living + condition, data=train, 
                kernel="radial", cost=1, gamma=1, scale=T)
summary(svm.fit1)   

#Instead of using random values for cost and gamma, it is better to perform cross-validation 
#using the tune() function to select the best model from a series of gamma and cost values:
#(Note - this will take a really long time to run. You can reduce the number of parameter 
#values to speed up calculations.)
set.seed(9)
svm.tune1 <- tune(svm, price2 ~ sqft_living + condition, data=train, kernel="radial", scale=T,
                  ranges=list(cost=c(0.01, 0.1, 1, 5), 
                              gamma=c(0.1, 1, 2)))

#View the cross-validation errors for each of the models. 
summary(svm.tune1)    

#The error rates are very similar across all values of cost and gamma, but the best model is 
#associated with a with cost=1 and gamma=1, with a corresponding error rate of 0.1845273.

#Obtain details of the best resulting model.
bestmod1 <- svm.tune1$best.model
summary(bestmod1)    #4233 support vectors, with 2119 from one class and 2114 from the other class

#Finally, draw predictions on the test set using the best model obtained from the training set. 
svm.pred1 <- predict(bestmod1, test)
table(svm.pred1)
#8557 observations predicted to be in class 0
#2250 observations predicted to be in class 1

#Determine the classification accuracy.
table(predicted=svm.pred1, observed=test$price2)
mean(svm.pred1==test$price2)    #accuracy rate = 80.83%
mean(svm.pred1!=test$price2)    #error rate = 19.17%


#You can also fit an SVM with a polynomial kernel rather than a radial kernel. Instead of 
#specifying a value for gamma, you need to specify a value for degree. In this example, I 
#specify cost=1 and degree=2:
svm.fit2 <- svm(price2 ~ sqft_living + condition, data=train, 
                kernel="polynomial", cost=1, degree=2, scale=T)
summary(svm.fit2)       

#Again, it's better to perform cross-validation using the tune() function to select the best 
#model from a series of degree and cost values. You can do this as follows (though I won't run
#it because it will take a long time to calculate):
set.seed(9)
svm.tune2 <- tune(svm, price2 ~ sqft_living + condition, data=train, kernel="polynomial", scale=T,
                  ranges=list(cost=c(0.1, 1, 5), 
                              degree=c(2, 3, 4)))

#View the cross-validation errors for each of the models. 
summary(svm.tune2)    #Best performance with cost=5 and gamma=3 (error=0.1833245)

#The best model has cost=1 and degree=3, with a corresponding error rate of 0.2036844.

#Obtain details of the best resulting model.
bestmod2 <- svm.tune2$best.model
summary(bestmod2)   

#Draw predictions on the test set using the best model obtained from the training set. 
svm.pred2 <- predict(bestmod2, test)
table(svm.pred2)    
#9332 observations predicted to be in class 0
#1475 observations predicted to be in class 1

#Determine the classification accuracy.
table(predicted=svm.pred2, observed=test$price2)
mean(svm.pred2==test$price2)    #accuracy rate = 79.41%
mean(svm.pred2!=test$price2)    #error rate = 20.59%




