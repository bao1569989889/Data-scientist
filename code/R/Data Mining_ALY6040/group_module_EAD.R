library(dplyr)
library(ggplot2)

#input data
setwd("/Users/tank/Desktop/ALY6015")
Amehouse<- read.csv("/Users/tank/Desktop/ALY6040 data mining/kc_house_data.csv",stringsAsFactors =FALSE)
summary(Amehouse)#21613obs,21variables
sapply(Amehouse, mode) #return matrix ,vector

##################### ---EDA--- ################################
#rename data frame
Amehouse<-rename(Amehouse,c("Sales_ID"="id", "Sold_Date"="date", "Sales_Price"="price", 
                            "Living_Space"="sqft_living", "Lot_size"="sqft_lot", 
                            "View_rating"="view", 
                            "Living_space_above_ground"="sqft_above", 
                            "Basement_Space"="sqft_basement"))
# convert date into character
Amehouse$sale_date<-as.character(as.Date(Amehouse$Sold_Date,format = "%Y%m%d"),format = "%Y%m")
Amehouse$sale_year<-as.numeric(as.character(as.Date(Amehouse$Sold_Date,format = "%Y%m%d"),format = "%Y"))
glimpse(Amehouse)

#Calculate House Age
for(i in 1:nrow(Amehouse)){
  if(Amehouse$yr_renovated[i] == 0){
    Amehouse$house_age <- 2021 - Amehouse$yr_built}else{
      Amehouse$house_age <-2021 - Amehouse$yr_renovated
    }
}

#Correlation matrix
Amehouse1<- Amehouse[,c(3:21,23:24)]
library(ggcorrplot)
par(mfrow=c(1,1))
ggcorrplot(round(cor(Amehouse1), 2), 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2,
           tl.cex = 6,
           title = 
             "Correlation matrix of the house dataset")

###### check duplicate data (id)#######
table(duplicated(Amehouse$Sales_ID))
# select duplicate data
duplicate_data<-Amehouse[duplicated(Amehouse$Sales_ID),]
# delete duplicate data
Amehouse <- Amehouse[!duplicated(Amehouse$Sales_ID),]
summary(Amehouse)#21436

####detect each column NA####
colSums(is.na(Amehouse))

####Boxplots check outliers####
Amehouse2<- Amehouse[,c(3,6,13,14)]
par(mfrow=c(2,2)) 
for (i in 1:(length(Amehouse2))){ #length(Amehouse1)=12 variables
  boxplot(x = Amehouse2[i],col = "lightblue", 
          horizontal = TRUE, 
          main = sprintf("Boxplot of the variable: %s", 
                         colnames(Amehouse2[i])),
          xlab = colnames(Amehouse2[i]))
}


#Identifing outliers number and percentage
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) |
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}
outlier <- data.frame(variable = character(), 
                      sum_outliers = integer(),
                      stringsAsFactors=FALSE)
for (j in 1:(length(Amehouse1))){
  variable <- colnames(Amehouse1[j])
  for (i in Amehouse1[j]){
    sum_outliers <- sum(is_outlier(i))
  }
  row <- data.frame(variable,sum_outliers)
  outlier <- rbind(outlier, row)
}

#Identifying the percentage of outliers
for (i in 1:nrow(outlier)){
  if (outlier[i,2]/nrow(Amehouse1) * 100 >= 1){  
    print(paste(outlier[i,1], 
                "=", 
                round(outlier[i,2]/nrow(Amehouse1) * 100, digits = 2),
                "%"))
  }
}

#repalce  the 33 bedrooms outlier to 3 bedrooms
Amehouse["bedrooms"][Amehouse["bedrooms"]==33] <- 3
summary((Amehouse$bedrooms))

##subset3 delate the number of bedrooms and bathrooms is equal to zero.
Amehouse3 <-filter(Amehouse,Amehouse$bedrooms!=0 & Amehouse$bathrooms!=0)
summary(Amehouse3)#21420

# plot house selling over year
ggplot(Amehouse,aes(x=sale_date))+
  geom_bar(fill="lightblue")+
  ggtitle("House Selling Records Over Time") +
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5))# center theme
#scatter plot
ggplot(Amehouse,aes(y=Sales_Price,x=bedrooms,col=bedrooms))+
  geom_point()
ggplot(Amehouse,aes(y=Sales_Price,x=bathrooms,col=bathrooms))+
  geom_point()
#plot House Price Distribution
ggplot(Amehouse3, aes(x = Sales_Price)) +
  geom_histogram(col = 'black', fill = 'blue', binwidth = 200000, center = 100000) +
  theme_linedraw() + 
  theme(plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), #title settings
        plot.subtitle = element_text(face = "italic")) + #subtitle settings
  labs(x = 'Price (USD)', y = 'Frequency', title = "House Sales in King County, USA",
       subtitle = "Price distribution") + #name subtitle
  scale_y_continuous(labels = scales::comma, limits = c(0,8000), breaks = c(0,2000,4000,6000,8000)) + 
  scale_x_continuous(labels = scales::comma)

#plot House Price Distribution by geographical location
#install.packages("maps")
library(maps)
library(ggplot2)
#Obtain the map data of King County from the maps library 
washington <- map_data("county", region="washington")
king.county <- subset(washington, subregion=="king")
head(king.county)
#Examine the housing locations based on price by varing the point colors by price.
#First, separate the price into quantiles.
qa <- quantile(Amehouse3$Sales_Price, c(0, .2, .4, .6, .8, 1))
Amehouse3$Sales_Price_q <- cut(Amehouse3$Sales_Price, qa, 
                    labels=c("0-78000", "78000-29000", "29000-399950", "399950-703068.8", "703068.8-7700000"),
                    include.lowest=TRUE) 

#Now plot the price quantiles.
ggplot() +
  geom_path(data=king.county, aes(x=long, y=lat, group=group), color="black") +
  geom_point(data=Amehouse3, aes(x = long, y = lat, color=Sales_Price_q)) +
  scale_color_brewer(palette="GnBu") +
  labs(x="longitude", y="latitude", color="Price Percentile",
       title = "House Sales in King County, USA",
       subtitle="Geographical distribution of houses in King County") +
  coord_quickmap() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), #title settings
        plot.subtitle = element_text(face = "italic"))

#Building Construction over year
Amehouse3 %>%
  group_by(yr_built) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = yr_built, y = n)) +
  geom_line(color = 'black') +
  geom_point(color = 'blue', size = 0.5) +
  geom_smooth(method="lm", color = 'red', size = 0.5) +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0, face = 'bold',color = 'black'),
        plot.subtitle = element_text(face = "italic")) +
  labs(x = 'Year', y = 'Total', title = "House Sales in King County, USA",
       subtitle = "Number of house built in 1900 - 2015") +
  scale_x_continuous(breaks=seq(1900, 2015, 10))

rbPal <- colorRampPalette(c('blue','green'))
rbPal2 <- colorRampPalette(c('black','red'))
colors1 <- rbPal(13)
colors2 <- rbPal2(13)
#Lot_size~Living_Space
ggplot(Amehouse3, aes(x =Living_Space, y =Lot_size)) + 
  geom_jitter(alpha = 0.5, aes(shape = as.factor(condition), color = as.factor(grade))) +
  scale_color_manual(values = colors1) +
  theme_linedraw() +
  theme(legend.title = element_text(size=10),
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'),
        plot.subtitle = element_text(face = "italic")) +
  labs(x = 'Living Area (sq.ft)', y = 'Lot Area (sq.ft)', title = "House Sales in King County, USA",
       subtitle = "House built in 1900 - 2015") +
  guides(color = guide_legend(title = "Grade"),
         shape = guide_legend(title = 'Condition')) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

#Sales_Price~Living_Space
ggplot(Amehouse3, aes(x = Living_Space, y = Sales_Price)) + 
  geom_jitter(alpha = 0.5, aes(shape = as.factor(condition), color = as.factor(grade))) +
  scale_color_manual(values = colors1) +
  theme_linedraw() +
  theme(legend.title = element_text(size=10),
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'),
        plot.subtitle = element_text(face = "italic")) +
  labs(x = 'Living Area (sq.ft)', y = ' Sale Price', title = "House Sales in King County, USA",
       subtitle = "House built in 1900 - 2015") +
  guides(color = guide_legend(title = "Grade"),
         shape = guide_legend(title = 'Condition')) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

#Lot_size~Living_Space
ggplot(Amehouse3, aes(x =Living_Space, y =Lot_size)) + 
  geom_jitter(alpha = 0.5, aes(color = as.factor(bedrooms), shape = as.factor(floors), size = bathrooms)) +
  scale_color_manual(values = colors2) +
  theme_linedraw() +
  theme(legend.title = element_text(size=10),
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'),
        plot.subtitle = element_text(face = "italic")) +
  labs(x = 'Living Area (sq.ft)', y = 'Lot Area (sq.ft)', title = "House Sales in King County, USA",
       subtitle = "House built in 1900 - 2015") +
  guides(color = guide_legend(title = "Bedrooms"),
         shape = guide_legend(title = 'Floors'),
         size = guide_legend(title = 'Bathrooms')) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

#Price~Living_Space
ggplot(Amehouse3, aes(x =Living_Space, y = Sales_Price)) + 
  geom_jitter(alpha = 0.5, aes(color = as.factor(bedrooms), shape = as.factor(floors), size = bathrooms)) +
  scale_color_manual(values = colors2) +
  theme_linedraw() +
  theme(legend.title = element_text(size=10),
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'),
        plot.subtitle = element_text(face = "italic")) +
  labs(x = 'Living Area (sq.ft)', y = 'Sale Price', title = "House Sales in King County, USA",
       subtitle = "House built in 1900 - 2015") +
  guides(color = guide_legend(title = "Bedrooms"),
         shape = guide_legend(title = 'Floors'),
         size = guide_legend(title = 'Bathrooms')) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

#plot Price vs. Bedrooms
par(mfrow=c(1,1)) 
boxplot(Sales_Price~bedrooms, data=Amehouse3, col=(c(2:8)), main="Price vs. Bedrooms", 
        xlab="Bedrooms", ylab="Price",yaxt="n")
marks <- c(0,2000000,4000000,6000000,8000000)
axis(2,at=marks,labels=format(marks,scientific=FALSE), hadj=0.9, cex.axis=0.7, las=2)

#Plot Price vs. Bathrooms
boxplot(Sales_Price~bathrooms, data=Amehouse3, col=(c(2:20)), main="Price vs. Bathrooms", 
        xlab="Bathrooms", ylab="Price",yaxt="n")
marks <- c(0,2000000,4000000,6000000,8000000)
axis(2,at=marks,labels=format(marks,scientific=FALSE), hadj=0.9, cex.axis=0.7, las=2)

#calculate sqrt_price
Amehouse3$sqrt_price <- sqrt(Amehouse3$Sales_Price)
head(Amehouse3)

#summary statistics
summary(Amehouse3$sqrt_price)
sd(Amehouse3$sqrt_price)

#histogram distribution of sqrt_price
ggplot(Amehouse3, aes(x=sqrt_price)) +
  geom_histogram(binwidth=50, col = 'black', fill = 'blue') +
  scale_x_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title = "House Sales in King County, USA",subtitle="Distribution of sqrt_price") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, face = 'bold',color = 'black'),
        plot.subtitle = element_text(face = "italic"))


############## ---feature selection--- ################
library(mlbench)
library(lattice)
library(caret)
#Create a data frame containing only the variables to be included in the analyses.
Amehouse4 <- Amehouse3[,c(4:21,23,24,26)]
sapply(Amehouse4,class)
set.seed(123)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(sqrt_price~., data=Amehouse4, method="knn", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance,main="Feature rank by loess r-squared variable")

#################### ---random forests--- ######################
library(randomForest)
Amehouse5<- Amehouse4[,c("Living_Space","grade","sqft_living15","Living_space_above_ground","lat",
                         "bathrooms","View_rating","bedrooms","Basement_Space","floors","sqrt_price")]
summary(Amehouse5$sqrt_price)
Amehouse5$sqrt_price<-as.factor(ifelse(Amehouse5$sqrt_price>=706.7,"high","low"))
summary(Amehouse5$sqrt_price)
#Separate the data into a training set and a test set.
set.seed(123)
samp <- sample(1:nrow(Amehouse5), size=nrow(Amehouse5)*0.8,replace=FALSE)
train <- Amehouse5[samp,]
test <- Amehouse5[-samp,]

set.seed(123)
bag.fit <- randomForest(sqrt_price ~ ., data=train,do.trace=T,importance=TRUE)

#Call the function name to review a summary of the model.
bag.fit    
plot(bag.fit)
#plot importance variables
importance(bag.fit)
varImpPlot(bag.fit)

#Determine how well the model performs on the test set.
test$bag.pred <- predict(bag.fit, test)
summary(test$bag.pred)
summary(test$sqrt_price)
confusionMatrix(test$bag.pred, test$sqrt_price)#based on top 10 variavles
#0.8973
#################### ---K——means--- ######################
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
distance <- get_dist(Amehouse4)

# Determine number of clusters
fviz_nbclust(Amehouse4, kmeans, method = "silhouette")
k2 <- kmeans(Amehouse4, centers = 2, nstart = 25)
str(k2)
k2
#plot
fviz_cluster(k2, data = Amehouse4)
