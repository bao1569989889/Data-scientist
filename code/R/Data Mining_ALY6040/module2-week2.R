
#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')
install.packages('readxl')
install.packages('randomForest')
install.packages('e1071')

#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)
library(readxl)
library(ggplot2)
library(randomForest)  
library(e1071)

setwd("/Users/tank/Desktop/ALY6040 data mining")

# Create a vector of Excel files to read
files.to.read <- list.files(pattern="xlsx")

# Read each file and write it to csv
lapply(files.to.read, function(f) {
  mushrooms = read_excel("/Users/tank/Desktop/ALY6040 data mining/mushrooms.xlsx")
  write.csv(mushrooms, gsub("xlsx", "csv", f), row.names=FALSE)
})

mushrooms <- read.csv("/Users/tank/Desktop/ALY6040 data mining/mushrooms.csv")

# structure of the data
str(mushrooms)
summary(mushrooms)
# number of rows with missing values
nrow(mushrooms) - sum(complete.cases(mushrooms))

# deleting redundant variable `veil.type`
mushrooms$veil.type <- NULL

#analyzing the odor variable
table(mushrooms$class,mushrooms$odor)

number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class,col)
  sum(t == 0)
})

# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]
number.perfect.splits
# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")

#data splicing
set.seed(12345)
train <- sample(1:nrow(mushrooms),
                size = ceiling(0.80*nrow(mushrooms)),
                replace = FALSE)
# training set
mushrooms_train <- mushrooms[train,]
# test set
mushrooms_test <- mushrooms[-train,]

# Original Data
table(mushrooms$class)/nrow(mushrooms)  

# Decision Trees Training Data
table(mushrooms_train$class)/nrow(mushrooms_train)  

# Decision Trees Testing Data
table(mushrooms_test$class)/nrow(mushrooms_test)

# penalty matrix
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)

# building the classification tree with rpart
tree <- rpart(class~.,
              data=mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")
# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)

# choosing the best complexity parameter "cp" to prune the tree
cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

# tree prunning using the best complexity parameter. For more in
tree <- prune(tree, cp=cp.optim)

#Testing the model
pred <- predict(object=tree,mushrooms_test[-1],type="class")

#Calculating accuracy
t <- table(mushrooms_test$class,pred) 
confusionMatrix(t) 


#Random forest

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

# Random Forest Training Data
table(data.dev$class)/nrow(data.dev)  

# Random forest Testing Data
table(data.val$class)/nrow(data.val)

#Fit Random Forest Model
rf = randomForest(class ~ ., 
                  ntree = 100,
                  data = data.dev)
plot(rf)
print(rf)

# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")
#Variable Importance
var.imp <- data.frame(importance(rf,type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val)

# Create Confusion Matrix
print(
  confusionMatrix(data=data.val$predicted.response,
                  reference=data.val$class,))







































