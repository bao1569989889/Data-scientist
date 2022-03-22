install.packages("randomForest")
install.packages("e1071")

library(randomForest)  
library(e1071)  
library(caret)  
library(ggplot2)
library(readxl)
library(dplyr)
library(reshape)
library(tidyverse)

setwd("/Users/tank/Desktop/ALY6040 data mining")
# Create a vector of Excel files to read
files.to.read <- list.files(pattern="xlsx")
# Read each file and write it to csv
lapply(files.to.read, function(f) {
  mushrooms = read_excel("/Users/tank/Desktop/ALY6040 data mining/mushrooms.xlsx")
  write.csv(mushrooms, gsub("xlsx", "csv", f), row.names=FALSE)
})
mushrooms <- read.csv("/Users/tank/Desktop/ALY6040 data mining/mushrooms.csv")
summary(mushrooms)
p<-ggplot(mushrooms,aes(x=cap.shape,  
                    y=cap.surface, 
                    color=class))

p + geom_jitter(alpha=0.3) +  
  scale_color_manual(breaks = c('e','p'),
                     values=c('darkgreen','red'))

p1 <- ggplot(mushrooms,aes(x=stalk.color.below.ring,  
                    y=stalk.color.above.ring,
                    color=class))

p1 + geom_jitter(alpha=0.3) +  
  scale_color_manual(breaks = c('e','p'),
                     values=c('darkgreen','red'))
p2 <- ggplot(mushrooms,aes(x=odor,  
                    y=spore.print.color, 
                    color=class))

p2 + geom_jitter(alpha=0.3) +  
  scale_color_manual(breaks = c('e','p'),
                     values=c('darkgreen','red'))

p3 <- ggplot(mushrooms,aes(x=class,  
                    y=odor, 
                    color = class))

p3 + geom_jitter(alpha=0.2) +  
  scale_color_manual(breaks = c('e','p'),
                     values=c('darkgreen','red'))

p4 <- ggplot(mushrooms,aes(x=class,  
                           y=spore.print.color, 
                           color = class))
p4 + geom_jitter(alpha=0.2) +  
  scale_color_manual(breaks = c('e','p'),
                     values=c('darkgreen','red'))
#Create data for training
set.seed(123)
sample.ind <- sample(2,  
                    nrow(mushrooms),
                    replace = T,
                    prob = c(0.05,0.95))
data.dev <- mushrooms[sample.ind==1,]  
data.val <- mushrooms[sample.ind==2,]  

# Original Data
table(mushrooms$class)/nrow(mushrooms)  

# Training Data
table(data.dev$class)/nrow(data.dev)  

# Testing Data
table(data.val$class)/nrow(data.val)

#Fit Random Forest Model
rf <- randomForest(class ~ ., 
                  ntree = 100,
                  data = data.dev)
plot(rf)
print(rf)

# Variable Importance
#This plot indicates what variables had the greatest impact in the classification model.
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")
#Variable Importance
var.imp <- data.frame(importance(rf,type=2))
var.imp
# make row names as columns

# importance(x, type=NULL, class=NULL, scale=TRUE, ...)
# Arguments
# x	
# an object of class randomForest
# type	
# either 1 or 2, specifying the type of 
# importance measure (1=mean decrease in accuracy, 
#                     2=mean decrease in node impurity).

var.imp$Variables <- row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
data.dev$predicted.response <- predict(rf , data.dev)# Training Data
data.dev$predicted.response

# Create Confusion Matrix
print(  
  confusionMatrix(data = data.dev$predicted.response,  
                  reference = data.dev$class,
                  ))

# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val)# Testing Data

# Create Confusion Matrix
print(
  confusionMatrix(data=data.val$predicted.response,
                  reference=data.val$class,))
