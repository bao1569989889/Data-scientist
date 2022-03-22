library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(lattice)
library(caret)
library(e1071)
setwd("/Users/tank/Desktop/ALY6015")
scores<-read.csv("/Users/tank/Desktop/ALY6040 data mining/6040_data_student-mat.csv")
glimpse(scores)
summary(scores)

##EAD Plot##
names(scores)
ggplot(data = scores, aes(x=age,colour=sex))+
  geom_bar(fill="white",width = 0.5)+
  labs(title="Age distribution by sex")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))

ggplot(data = scores, aes(x=studytime,colour=Final.Grade))+
  geom_bar(fill="white",width = 0.5)+
  labs(title="Relationship between studytime and final grade")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))

ggplot(data = scores, aes(x=absences,colour=Final.Grade))+
  geom_bar(fill="white")+
  labs(title="Relationship between absences and final grade")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size =12))

ggplot(data = scores, aes(x=schoolsup,colour=Final.Grade))+
  geom_bar(fill="white",width = 0.5)+
  labs(title="Relationship between schoolsup and final grade")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size =12))

ggplot(data = scores, aes(x=famsup,colour=Final.Grade))+
  geom_bar(fill="white",width = 0.5)+
  labs(title="Relationship between famsup and final grade")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size =12))

# ggplot(data = scores, aes(x=Medu,colour=Final.Grade))+
#   geom_bar(fill="white",width = 0.5)+
#   labs(title="Realationship between traveltime and final grade")+
#   theme(plot.title = element_text(hjust = 0.5,face = "bold",size =12))
# 
# ggplot(data = scores, aes(x=goout,y=failures))+
#   geom_point()+
#   geom_jitter(width = 0.2,height = 0.2)+
#   geom_smooth(method='lm',se=FALSE)+
#   labs(title="Realationship between goout and failures grade")+
#   theme(plot.title = element_text(hjust = 0.5,face = "bold",size =12))
# 
# ggplot(data = scores, aes(x=freetime,colour=Final.Grade))+
#   geom_bar(fill="white",width = 0.5)+
#   labs(title="Realationship between freetime and final grade")+
#   theme(plot.title = element_text(hjust = 0.5,face = "bold",size =12))

# ggplot(data = scores, aes(x=paid,colour=Final.Grade))+
#   geom_bar(fill="white",width = 0.5)+
#   labs(title="Realationship between extra paid classes and final grade")+
#   theme(plot.title = element_text(hjust = 0.5,face = "bold",size =12))

ggplot(data = scores, aes(x=higher,colour=Final.Grade))+
  geom_bar(fill="white",width = 0.5)+
  labs(title="Relationship between desire to education and final grade",x="Desire to education")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size =12))

ggplot(data = scores, aes(x=health,colour=Final.Grade))+
  geom_bar(fill="white",width = 0.5)+
  labs(title="Relationship between health and final grade",x="Health(low->high)")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size =12))

ggplot(data = scores, aes(x=Pstatus,colour=Final.Grade))+
  geom_bar(fill="white",width = 0.5)+
  labs(title="Relationship between parent's cohabitation status and final grade")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size =10))

##detect each column NA
colSums(is.na(scores))

##detect duplicates
sum(duplicated(scores))

#Boxplots check outliers

sapply(scores,class)
scores1<- scores[,c(3,7:8,13:15,24:30)]
glimpse(scores1)
sapply(scores1,class)

par(mfrow=c(4,4)) 
for (i in 1:(length(scores1))){ #length(Amehouse1)=12 variables
  boxplot(x = scores1[i],col = "lightblue", 
          horizontal = TRUE, 
          main = sprintf("Boxplot of the variable: %s", 
                         colnames(scores1[i])),
          xlab = colnames(scores1[i]))
}
dev.off()
#one-hot encode for correlation matrix
dummy<- dummyVars("~.",data=scores,fullRank=T)
date_transformed<- data.frame(predict(dummy,newdata = scores))
ggcorrplot(round(cor(date_transformed), 2), 
           type = "lower", 
           lab = TRUE, 
           lab_size = 1.5,
           tl.cex = 6,
           title = 
             "Correlation matrix of the scores dataset")

#feature rank
library(mlbench)
set.seed(9)
# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model1 <- train(Final.Grade~., data=scores, method="knn", preProcess="scale", trControl=control1)
# estimate variable importance
importance1 <- varImp(model1, scale=FALSE)
# summarize importance
print(importance1)
# plot importance
plot(importance1,main="Feature rank by loess r-squared variable")


###-- a. Logistic regression --###
names(scores)
scores$Final.Grade1 <- ifelse(scores$Final.Grade=="Fail", 0, 1)
table(scores$Final.Grade1)    

scores$Final.Grade1 <- as.factor(scores$Final.Grade1)

#Use the glm() function to fit a logistic regression model
# log.fit1 <- glm(Final.Grade1 ~ Medu+Walc+failures+age+Dalc+address+studytime+absences+traveltime+goout+schoolsup+Pstatus+
#                   freetime+famsup+higher+health+paid, data=scores, family=binomial)
glimpse(scores)
scores2<-scores[,c(1:30,32)]
glimpse(scores2)
log.fit1 <- glm(Final.Grade1 ~.,data=scores2, family=binomial)
summary(log.fit1)

log.prob1 <- predict(log.fit1, type="response")

log.pred1 <- rep("0", nrow(scores))
log.pred1[log.prob1>.5] <- "1"
table(log.pred1)    
#Compare the predicted classifications to the observed data to determine the accuracy of the model.
table(predicted=log.pred1, observed=scores$Final.Grade1)
mean(log.pred1==scores$Final.Grade1)    #accuracy rate=  80.50633%
mean(log.pred1!=scores$Final.Grade1)    #error rate= 19.49367%  



##### 9 - Support Vector Machines #####
svm.dat <- scores2[,c("Final.Grade1","Medu","Walc","failures","address","age",
                      "traveltime","studytime","schoolsup","absences","goout",
                     "Pstatus","reason","higher","health")]
set.seed(9)
samp <- sample(nrow(svm.dat), nrow(svm.dat)/2)
train <- svm.dat[samp,]
test <- svm.dat[-samp,]

#Use the svm() function to fit a model with specified values of the parameters. For example, here we fit an SVM with a radial kernel, using cost=1 and gamma=1.
svm.fit1 <- svm(Final.Grade1 ~ Medu+Walc+failures+address+age+studytime+absences+
                  traveltime+goout+schoolsup+Pstatus+higher+health+reason, data=train, 
                kernel="radial", cost=1, gamma=1, scale=T)
summary(svm.fit1)   

#The tune() function performs 10-fold cross-validation to select from a range of specified 
#parameter values. Use this function to select from a a range of cost values:  
svc.tune1 <- tune(svm, Final.Grade1 ~  Medu+Walc+failures+address+age+studytime+absences+
                    traveltime+goout+schoolsup+Pstatus+higher+health+reason, data=train, kernel="linear",
                  ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))

#View the cross-validation errors for each of the models. 
summary(svc.tune1)

#Each cost value resulted in similar errors, but the best model occurred with cost=.01 and
#a corresponding error rate of 0.1844347.
bestmod <- svc.tune1$best.model
summary(bestmod)  

#Draw predictions on the test set using the best model obtained from the training set. 
svc.pred1 <- predict(bestmod, test)
#Determine the classification accuracy.
table(predicted=svc.pred1, observed=test$Final.Grade1)
mean(svc.pred1==test$Final.Grade1)    
mean(svc.pred1!=test$Final.Grade1)    




