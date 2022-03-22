##### 8 - Decision Trees #####


##-- a. Classification trees --##


#Use the tree library to construct classification and regression trees.
library(tree)

#Create a data frame containing only the binary outcome variable, price2 
#(see the "6 - logistic regression" script), plus the relevant predictor variables to be 
#included in the analyses.
king3 <- king[,c("price2", "sqft_living", "sqft_lot", "sqft_living15", "sqft_lot15", 
                 "bedrooms", "bathrooms",  "floors", "waterfront", "condition", "grade", 
                 "yr_built")]

#Use the tree() function from the tree library to fit a classification tree to predict price2.
tree.fit1 <- tree(price2 ~ ., king3)

#Review the model and the details of each split
tree.fit1
summary(tree.fit1)    #misclassification error rate = 16.09%

#Plot the tree
plot(tree.fit1)
text(tree.fit1, pretty=0)

#Instead of fitting the tree to the entire data set, split the data into a training set and a test set 
#to estimate test set error rate. 
set.seed(9)
samp <- sample(nrow(king3), nrow(king3)/2)
train <- king3[samp,]
test <- king3[-samp,]

#Fit the tree on the training set.
tree.fit2 <- tree(price2 ~ ., train)
tree.fit2
summary(tree.fit2)    #misclassification error rate = 16.05%
plot(tree.fit2)
text(tree.fit2, pretty=0)

#Obtain predictions on the test set. Specifying type="class" returns the actual class prediction.
#(If you don't specify type="class", the model returns the probability of being in each class.)
tree.pred2 <- predict(tree.fit2, test, type="class")

#Determine the classification accuracy on the test set.
table(tree.pred2, test$price2)
mean(tree.pred2==test$price2)    #accuracy rate = 83.64%
mean(tree.pred2!=test$price2)    #error rate = 16.36%

#Use the cv.tree() function to prune the tree and perform cross-validation to determine the 
#optimal level of tree complexity. Cost complexity pruning is used in order to select a 
#sequence of trees for consideration. Use the argument FUN=prune.misclass to use classification 
#error rate rather than deviance to guide the cross-validation and pruning process. 
set.seed(9)
tree.cv2 <- cv.tree(tree.fit2, FUN=prune.misclass) 

#Calling the name of the pruned tree shows the number of terminal nodes of each size of tree 
#considered, the corresponding cross-validation error rate (dev), and the value k (the 
#cost-complexity parameter) used. 
tree.cv2    #Trees of size 4 and 8 have the lowest error (dev=1740)

#Plot the error rate as a function of size and k.
plot(tree.cv2$size, tree.cv2$dev, type="b")
plot(tree.cv2$k, tree.cv2$dev, type="b")

#Apply the prune.misclass() function to prune the tree back to 4 nodes.
tree.prune2 <- prune.misclass(tree.fit2, best=4)
tree.prune2
summary(tree.prune2)    #misclassification error rate = 16.05%

#Plot the tree.
plot(tree.prune2)
text(tree.prune2, pretty=0)

#Obtain predictions on the test set using the pruned tree, then determine the classification 
#accuracy.
tree.pred3 <- predict(tree.prune2, test, type="class")
table(tree.pred3, test$price2)
mean(tree.pred3==test$price2)    #accuracy = 83.64%
mean(tree.pred3!=test$price2)    #accuracy = 16.36%
#The classification accuracy didn't improve, but the tree is simpler.



##-- b. Regression trees --##


#Use the tree library to construct classification and regression trees.
library(tree)

#We'll use the king2 data set to fit a regression tree to predict the sqrt_price, a continuous
#variable:
king2 <- king[,c("sqrt_price", "sqft_living", "sqft_lot", "sqft_living15", "sqft_lot15", 
                 "bedrooms", "bathrooms",  "floors", "waterfront", "condition", "grade", 
                 "yr_built")]

#Use the tree() function to fit a regression tree to predict sqrt_price.
tree.fit3 <- tree(sqrt_price ~ ., king2)

#Review the model and the details of each split. 
#(Note - the deviance refers to the sum of squared errors for the tree.)
tree.fit3
summary(tree.fit3)    
#Number of terminal nodes = 10
#residual mean deviance = 17100    

#Plot the tree
plot(tree.fit3)
text(tree.fit3, pretty=0)

#Instead of fitting the tree to the entire data set, split the data into a training set and 
#a test set to estimate test set error rate. 
set.seed(9)
samp <- sample(nrow(king2), nrow(king2)/2)
train <- king2[samp,]
test <- king2[-samp,]

#Fit the tree on the training set.
tree.fit4 <- tree(sqrt_price ~ ., train)
tree.fit4
summary(tree.fit4)    
#Number of terminal nodes = 10
#residual mean deviance = 17130

#Plot the tree
plot(tree.fit4)
text(tree.fit4, pretty=0)

#Obtain predictions on the test set. 
tree.pred4 <- predict(tree.fit4, test)

#Calculate the mean squared error
mean((test$sqrt_price-tree.pred4)^2)    #test MSE = 17257.91

#Prune the tree using the cv.tree() function to determine whether pruning will improve performance
#on the test set.
set.seed(9)
tree.cv4 <- cv.tree(tree.fit4)
tree.cv4

#Plot the deviance as a function of size
plot(tree.cv4$size, tree.cv4$dev, type="b")

#The lowest deviance occurs with the most complex tree of 10 nodes, but let's prune it back 
#to 6 nodes for the sake of having a simpler tree
tree.prune4 <- prune.tree(tree.fit4, best=6)
tree.prune4
summary(tree.prune4)    #residual mean deviance = 19680
plot(tree.prune4)
text(tree.prune4, pretty=0)

#Obtain predictions on the test set using the pruned tree, then calculate the test MSE.
tree.pred5 <- predict(tree.prune4, test)
mean((test$sqrt_price-tree.pred5)^2)    #test MSE = 19316.19

#Plot the relationship between the predicted values and the observed values
plot(tree.pred5, test$sqrt_price)
abline(0,1, col="red")



##-- c. Bagging and random forests --##


#Use the randomForest package to apply bagging and random forests.
library(randomForest)

#Separate the data into a training set and a test set.
set.seed(9)
samp <- sample(nrow(king2), nrow(king2)/2)
train <- king2[samp,]
test <- king2[-samp,]

#Perform bagging on the training data using the randomForest() function. Bagging and random 
#forests use the same function because bagging is simply a special case of random forest in 
#which all predictors are considered for each split in the tree. The argument "mtry" specifies 
#the number of predictors to be considered for each split. To perform bagging, we incude all 11
#predictors in our king2 data frame. 
#(Note - the algorithm will take some time to run.)
set.seed(9)
bag.fit1 <- randomForest(sqrt_price ~ ., data=train, mtry=13, importance=TRUE)

#Call the function name to review a summary of the model.
bag.fit1    
# Number of trees = 500
# MSE = 10877.34, 
# % variance explained = 74.15% (based on out-of-bag estimates)

#View the importance stats for each variable. The %IncMSE column indicates the mean decrease 
#in accuracy of predictions on the out-of-bag samples when a given variable is excluded from 
#the model. The IncNodePurity column is a measure of the total decrease in node impurity that 
#results from splits over that variable, averaged over all trees.
importance(bag.fit1)
varImpPlot(bag.fit1)

#Determine how well the model performs on the test set.
bag.pred1 <- predict(bag.fit1, test)
mean( (test$sqrt_price-bag.pred1)^2 )    #test MSE = 10510.1

#Plot the relationship between the predicted values and the observed values.
plot(bag.pred1, test$sqrt_price)
abline(0,1, col="red")

#Now perform random forest on the training data. By default, the "mtry" argument in the 
#randomForest() function randomly selects p/3 variables for each split a regression tree, 
#and sqrt(p) for each split in a classification tree.
#(Note - the algorithm will take some time to run.)
set.seed(9)
rf.fit1 <- randomForest(sqrt_price ~ ., data=train, importance=TRUE)

#Call the function name to review a summary of the model.
rf.fit1   


#View the importance stats for each variable.
importance(rf.fit1)
varImpPlot(rf.fit1)

#Determine how well the model performs on the test set.
rf.pred1 <- predict(rf.fit1, test)
mean( (test$sqrt_price-rf.pred1)^2 )    #test MSE = 10079.78

#Plot the relationship between the predicted values and the observed values.
plot(rf.pred1, test$sqrt_price)
abline(0,1, col="red")



##-- d. Boosting --##


#Perform boosting using gbm() function within the gbm package. 
install.packages("gbm")
library(gbm)

#Separate the data into a training set and a test set.
set.seed(9)
samp <- sample(nrow(king2), nrow(king2)/2)
train <- king2[samp,]
test <- king2[-samp,]

#Fit a boosted regression tree. Specify distribution="gaussian" for regression. (If we fit a 
#classification tree, we would specify distribution="bernoulli".) 
#Boosting requires adjusting many tuning parameters, including "n.tree", "interaction.depth", and
#"shrinkage". The n.tree argument specifies the number of trees, the interaction.depth argument 
#specifies the depth of each tree, and the shrinkage argument specifies the shrinkage parameter λ. 
#The default value for shrinkage is 0.001 if you do not specify it in the model.
#(Note - the algorithm might take some time to run.)
set.seed(9)
boost.fit1 <- gbm(sqrt_price ~ ., data=train, distribution="gaussian",
                  n.trees=5000, interaction.depth=4)

#Use the summary() function to view the relative influence statistics and relative influence plot.
summary(boost.fit1)

#You can create partial dependence plots for individual variables. These plots illustrate the 
#marginal effect of the selected variable on the response after taking into account the other 
#variables.
plot(boost.fit1, i="grade")
plot(boost.fit1, i="sqft_living")
plot(boost.fit1, i="yr_built")

#Use the boosted model to calculate test set predictions and test MSE error. 
boost.pred1 <- predict(boost.fit1, test, n.trees=5000)
mean( (test$sqrt_price-boost.pred1)^2 )    #test MSE = 11116.79

#Boosting models require a lot of parameter tuning. Here, we will determine how the test MSE 
#changes as a function of the n.trees value in the predict() function.
n.trees <- seq(from=100, to=5000, by=100)
predmat <- predict(boost.fit1, test, n.trees=n.trees)
dim(predmat)    #50 sets of predictions, one for each value of n.trees
err.mat <- apply( (test$sqrt_price-predmat)^2, 2, mean )  #calculate the test MSE for each set 
#of predictions

#Plot the test MSE as a function of n.trees
plot(n.trees, err.mat)
abline(h=min(err.mat), col="red")
which.min(err.mat)    #lowest error occurs when n.tree=700
err.mat[7]     #test MSE for 700 trees = 10583.31 
