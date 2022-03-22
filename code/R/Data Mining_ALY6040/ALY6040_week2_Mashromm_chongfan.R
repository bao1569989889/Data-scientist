
#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')
install.packages('readxl')
install.packages("randomForest")
install.packages("e1071")

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
# number of rows with missing values
nrow(mushrooms) - sum(complete.cases(mushrooms))

# deleting redundant variable `veil.type`
mushrooms$veil.type <- NULL

#analyzing the odor variable
table(mushrooms$class,mushrooms$odor)
# apply(X, MARGIN, FUN, ...)
# 参数列表：
# X:数组、矩阵、数据框
# MARGIN: 按行计算或按按列计算，1表示按行，2表示按列
# FUN: 自定义的调用函数
#计算在class：e和p的条件下，其于每个变量的类型的总数出现0的次数
number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class,col)
  sum(t == 0)
})
number.perfect.splits

# Descending order of perfect splits
#降序排列每一个变量
order <- order(number.perfect.splits,decreasing = TRUE)
order
number.perfect.splits <- number.perfect.splits[order]
number.perfect.splits
# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")


#data splicing（拼接）
#sample进行随机抽样，向量中抽取元素的个数，需要加一个参数size
# 所谓无放回抽样，也就是说某个元素一旦被选择，该总体中就不会再有该元素。
# 如果是有放回抽样，则需添加一个参数repalce=T：
#nrow(mushrooms)=8124
set.seed(12345)
train <- sample(1:nrow(mushrooms),
                size = ceiling(0.80*nrow(mushrooms)),
                replace = FALSE)
train
# training set
mushrooms_train <- mushrooms[train,]
mushrooms_train
# test set
mushrooms_test <- mushrooms[-train,]
mushrooms_test


# penalty matrix
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
penalty.matrix

# building the classification tree with rpart
#For classification splitting,the list can contain the 
#loss matrix (component loss)
tree <- rpart(class~.,
              data=mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")
tree
# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)

# choosing the best complexity parameter "cp" to prune the tree
cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

# tree prunning using the best complexity parameter. For more in
tree <- prune(tree, cp=cp.optim)

#Testing the model
pred <- predict(object=tree,mushrooms_test[-1],type="class")
pred
#Calculating accuracy
t <- table(mushrooms_test$class,pred) 
confusionMatrix(t) 

#Testing the model with training data
pred1 <- predict(object=tree,mushrooms_train[-1],type="class")

#Calculating accuracy
t1 <- table(mushrooms_train$class,pred1) 
confusionMatrix(t1) 


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

# Training Data
table(data.dev$class)/nrow(data.dev)  

# Testing Data
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
data.dev$predicted.response <- predict(rf , data.dev)

# Create Confusion Matrix
print(  
  confusionMatrix(data = data.dev$predicted.response,  
                  reference = data.dev$class,))

# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val)

# Create Confusion Matrix
print(
  confusionMatrix(data=data.val$predicted.response,
                  reference=data.val$class,))






























