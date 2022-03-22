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
#The fit increases sharply at 7 predictors, then levels off
which.max(best.summary1$adjr2)   
#The best fit based on adjr2 occurs when all 17 predictors are included in the model.
best.summary1$adjr2[11]          
#With all 11 predictors, adjusted R^2 = 0.766.

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