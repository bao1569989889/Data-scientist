library(dplyr)
library(ggplot2)
setwd("/Users/tank/Desktop/ALY6040")
Amehouse<- read.csv("/Users/tank/Desktop/ALY6040 data mining/kc_house_data.csv",stringsAsFactors =FALSE)
str(Amehouse)
p<-summary(Amehouse)
p
sapply(Amehouse, mode)
table(Amehouse$bedrooms)
# cinvert date into character
Amehouse$sale_date<-as.character(as.Date(Amehouse$date,format = "%Y%m%d"),format = "%Y%m")
Amehouse$sale_year<-as.numeric(as.character(as.Date(Amehouse$date,format = "%Y%m%d"),format = "%Y"))
glimpse(Amehouse)

# plot house selling over year
ggplot(Amehouse,aes(x=sale_date))+
  geom_bar(fill="lightblue")+
  ggtitle("House Selling Records Over Time") +
  theme(plot.title = element_text(hjust = 0.5))

#Calculate House Age
for(i in 1:nrow(Amehouse)){
if(Amehouse$yr_renovated[i] == 0){
  Amehouse$age <- 2021 - Amehouse$yr_built}else{
    Amehouse$age <-2021 - Amehouse$yr_renovated
  }
}
str(Amehouse)
summary(Amehouse)
#for(){
#翻新大于>0:销售时间大于翻新时间（销售之后翻新），房子年限=翻新时间。如果，销售时间小于翻新时间（销售之前翻新），房子年限=建房时间
#翻新大于=0:房子年限=建房时间
# a<-abs(Amehouse$yr_renovated-Amehouse$yr_built)
# table(a)
# b<-Amehouse$sale_year-abs(Amehouse$yr_renovated-Amehouse$yr_built)
# table(b)
# for(i in 1:nrow(Amehouse)){
#   if(Amehouse$yr_renovated[i] == 0){
#     Amehouse$age <- Amehouse$yr_built} else{
#           if(Amehouse$sale_year>=Amehouse$yr_renovated){
#             Amehouse$age <- Amehouse$yr_renovated
#           }else{
#             Amehouse$age <- Amehouse$yr_built
#           }
#         }
#     }
      
# for(i in 1:nrow(Amehouse)){
#   if(Amehouse$yr_renovated[i]>0){
#     Amehouse$yr_built[[i]] <- Amehouse$yr_renovated[[i]]
#   }esle{
#     Amehouse$yr_built[[i]] <- Amehouse$yr_built[[i]]
#   }
# }
#sapply(Amehouse, mode)
#lapply(Amehouse, mean)

#Check class BIAS 
# table(data$quality)
# round(prop.table((table(data$quality))),2)
}

#Subset1
Amehouse1<- Amehouse[,c(3:14)]#12 variables
str(Amehouse1)
summary(Amehouse1)

#Independent variable
#Boxplots
##length(Amehouse1)=12 variables
par(mfrow=c(4,3)) # 4 x 3 pictures on one plot
for (i in 1:(length(Amehouse1))){
  boxplot(x = Amehouse1[i],col = "lightblue", 
          horizontal = TRUE, 
          main = sprintf("Boxplot of the variable: %s", 
                         colnames(Amehouse1[i])),
          xlab = colnames(Amehouse1[i]))
}
#Histograms
par(mfrow=c(4,3))
for (i in 1:(length(Amehouse1))){
  hist(x = Amehouse1[[i]], col = "lightblue",
       main = sprintf("Histogram of the variable: %s",
                      colnames(Amehouse1[i])), 
       xlab = colnames(Amehouse1[i]))
}

#subset2
Amehouse2<- Amehouse[,c(15:21,23)]#8 variables
str(Amehouse2)
summary(Amehouse2)

par(mfrow=c(4,2))
for (i in 1:(length(Amehouse2))){
  boxplot(x = Amehouse2[i],col = "lightblue", 
          horizontal = TRUE, 
          main = sprintf("Boxplot of the variable: %s", 
                         colnames(Amehouse2[i])),
          xlab = colnames(Amehouse2[i]))
}

#Histograms
par(mfrow=c(4,2))
for (i in 1:(length(Amehouse2))){
  hist(x = Amehouse2[[i]], col = "lightblue",
       main = sprintf("Histogram of the variable: %s",
                      colnames(Amehouse2[i])), 
       xlab = colnames(Amehouse2[i]))
}

#subset3
Amehouse3<- Amehouse[,c(3:21,23)]
str(Amehouse3)
s1<-summary(Amehouse3)
s1
#Bivariate analysis
#Correlation matrix
#install.packages("ggcorrplot")
library(ggcorrplot)
par(mfrow=c(1,1))
ggcorrplot(round(cor(Amehouse3), 2), 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2,
           title = 
             "Correlation matrix of the house dataset")

#detect each column NA
#sum(is.na(Amehouse))
colSums(is.na(Amehouse))

#Outliers
#Identifing outliers
#for{
#quantile(x, 0.25): 0.25---1st Qu.;0.75--3st Qu.;
#IQR(x):computes interquartile range of the x values.(3分位于1分位的差）
#quantile(x, 0.25) - 1.5 * IQR(x):下限
#quantile(x, 0.75) + 1.5 * IQR(x):上限
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) |
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}
outlier <- data.frame(variable = character(), 
                      sum_outliers = integer(),
                      stringsAsFactors=FALSE)
for (j in 1:(length(Amehouse3))){
  variable <- colnames(Amehouse3[j])
  for (i in Amehouse3[j]){
    sum_outliers <- sum(is_outlier(i))
  }
  row <- data.frame(variable,sum_outliers)
  outlier <- rbind(outlier, row)
}

#Identifying the percentage of outliers
#nrow(outlier)=21；nrow(Amehouse)=21613
#for{
#if (outlier[i,2]/nrow(Amehouse) * 100 >= 6)：技术算异常值百分比，大于6列出变量名
#paste (..., sep = " ", collapse = NULL):链接字符串向量
  

for (i in 1:nrow(outlier)){
  if (outlier[i,2]/nrow(Amehouse) * 100 >= 1){  
    print(paste(outlier[i,1], 
                "=", 
                round(outlier[i,2]/nrow(Amehouse) * 100, digits = 2),
                "%"))
  }
}
#Inputting outlier values percentage>=2%（离群值）
for (i in 1:nrow(outlier)){
  for (j in 1:nrow(Amehouse3)){
    if (Amehouse3[[j, i]] > as.numeric(quantile(Amehouse3[[i]], 0.75) + 
                                  1.5 * IQR(Amehouse3[[i]]))){
      if (i == c(1:5)){
        Amehouse3[[j, i]] <- round(mean(Amehouse3[[i]]), digits = 0)
      } else{
        if (i == 8){
          Amehouse3[[j, i]] <- round(mean(Amehouse3[[i]]), digits = 0)
        }else{
          if (i == c(10:12)){
            Amehouse3[[j, i]] <- round(mean(Amehouse3[[i]]), digits = 0)
          }else{
            if (i == 13){
              Amehouse3[[j, i]] <- round(mean(Amehouse3[[i]]), digits = 0)
            }else{
              if (i == c(18:19)){
                Amehouse3[[j, i]] <- round(mean(Amehouse3[[i]]), digits = 0)
              }
            }
          }
        }
      }
    }
  }
}
s2<-summary(Amehouse3)
s2
#delete bedrooms outlier and 
Amehouse4 <- filter(Amehouse3,Amehouse3$bedrooms!=33)
summary(Amehouse4)
# delete duplicate data
sum(duplicated(Amehouse4))
Amehouse5<-Amehouse4[!duplicated(Amehouse4, fromLast=TRUE), ] 
summary(Amehouse5)

#for{
#replace data that over or less 99% and 1%
q1 <- quantile(Amehouse5$sqft_lot, 0.01)
q1
q99 <- quantile(Amehouse5$sqft_lot, 0.99)       
q99
Amehouse5[Amehouse5$sqft_lot < q1,]$sqft_lot <- q1  
Amehouse5[Amehouse5$sqft_lot > q99,]$sqft_lot < -q99 
summary(Amehouse5$sqft_lot)


