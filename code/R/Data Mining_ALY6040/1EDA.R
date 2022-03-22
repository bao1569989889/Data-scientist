##### 1 - Exploratory Data Analysis and Data Visualization ####


#Import the data
king <- read.csv("/Users/tank/Desktop/ALY6040 data mining/kc_house_data.csv")
head(king)
dim(king)      #21613 observations, 21 columns
names(king)

#Check the data type of each variable
sapply(king, class)

#Change zipcode to a factor
king$zipcode <- as.factor(king$zipcode)

#Find any missing values
sum(is.na(king))    #No missing values



##-- a. Map the houses in King County --##

#install.packages("maps")
library(maps)
library(ggplot2)

#Obtain the map data of King County from the maps library 
washington <- map_data("county", region="washington")
king.county <- subset(washington, subregion=="king")
head(king.county)

#Plot the outline of King County.
ggplot() +
  geom_path(data=king.county, aes(x=long, y=lat, group=group), color="black") +
  coord_quickmap() +
  theme_bw() 

#Examine house locations across the county by plotting the the coordinate positions of each 
#house in the king data set, using geom_point():
ggplot() +
  geom_path(data=king.county, aes(x=long, y=lat, group=group), color="black") +
  geom_point(data=king, aes(x = long, y = lat), alpha=.1) +
  coord_quickmap() +
  theme_bw() 

#Examine the distribution of zipcodes by coloring the points based on zipcode
ggplot() +
  geom_path(data=king.county, aes(x=long, y=lat, group=group), color="black") +
  geom_point(data=king, aes(x = long, y = lat, color=zipcode)) +
  guides(color=FALSE) +
  scale_color_viridis_d() +
  coord_quickmap() +
  theme_bw() 

#Examine the housing locations based on price by varing the point colors by price.
#First, separate the price into quantiles.
qa <- quantile(king$price, c(0, .2, .4, .6, .8, 1))
king$price_q <- cut(king$price, qa, 
                    labels=c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                    include.lowest=TRUE) 

#Now plot the price quantiles.
ggplot() +
  geom_path(data=king.county, aes(x=long, y=lat, group=group), color="black") +
  geom_point(data=king, aes(x = long, y = lat, color=price_q)) +
  scale_color_brewer(palette="BuPu") +
  labs(x="longitude", y="latitude", color="Price Percentile",
       title = "House Sales in King County, USA",
       subtitle="Geographical distribution of houses in King County") +
  coord_quickmap() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), #title settings
        plot.subtitle = element_text(face = "italic"))



##-- b. Summary statistics and plots of each variable--##


# Outcome variables
# 1. price
# 2. sqrt_price (see below)

# Predictor variables
# 3. sqft_living
# 4. sqft_lot
# 5. sqft_above
# 6. sqft_basement
# 7. sqft_living15
# 8. sqft_lot15
# 9. bedrooms
# 10. bathrooms
# 11. floors
# 12. waterfront
# 13. condition
# 14. grade
# 15. yr_built

#Additional variables
# 16. yr_renovated
# 17. zipcode


#Open ggplot2 library for plotting.
library(ggplot2)


# 1. price

#summary statistics
summary(king$price)
sd(king$price)
#range = [$75,000 - $7,700,000]
#mean = $540,088.10
#sd = $367,127
#median = $450,000

#histogram distribution of price
ggplot(king, aes(x=price)) +
  geom_histogram(binwidth=100000, color="black", fill="white") +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)) +
  labs(title="Distribution of price") +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5))


# 2. sqrt_price（计算平方foptages 房价）

#Since price is heavily positively skewed, perform a square root transformation on price.
#This new sqrt_price variable will be used as the main outcome variable in analyses. 
king$sqrt_price <- sqrt(king$price)
head(king)

#summary statistics
summary(king$sqrt_price)
sd(king$sqrt_price)
#range of sqrt(price) = [273.86 - 2774.89]
#mean of sqrt(price) = 706.30
#sd of sqrt(price) = 203.07
#median of sqrt(price) = 670.82

#histogram distribution of sqrt_price
ggplot(king, aes(x=sqrt_price)) +
  geom_histogram(binwidth=50, color="black", fill="white") +
  scale_x_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title="Distribution of sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 3. sqft_living

#summary statistics
summary(king$sqft_living)
sd(king$sqft_living)
#range = [290 - 13540]
#mean = 2080
#sd = 918.44   
#median = 1910

#histogram distribution of sqft_living
ggplot(king, aes(x=sqft_living)) +
  geom_histogram(binwidth=500,  color="black", fill="white") +
  scale_x_continuous(breaks=c(2000, 4000, 6000, 8000, 10000, 12000)) +
  scale_y_continuous(breaks=c(1000, 2000, 3000, 4000, 5000)) +
  labs(title="Distribution of sqft_living") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 4. sqft_lot

#summary statistics
summary(king$sqft_lot)
sd(king$sqft_lot)
#range = [520 - 1,651,359]
#mean = 15107
#sd = 41420.51
#median = 7618

#histogram distribution of sqft_lot (limit the upper end of the range to 200000)
ggplot(king, aes(x=sqft_lot)) +
  geom_histogram(binwidth=2000,  color="black", fill="white") +
  scale_x_continuous(limits=c(0, 200000), breaks=c(5e4, 1e5, 1.5e5, 2e5, 2.5e5, 3e5)) +
  scale_y_continuous(breaks=c(1000, 2000, 3000, 4000)) +
  labs(title="Distribution of sqft_lot") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 5. sqft_above

#summary statistics
summary(king$sqft_above)
sd(king$sqft_above)
#range = [290 - 9410]
#mean = 1788.39
#sd = 828.09
#median = 1560

#histogram distribution of sqft_above
ggplot(king, aes(x=sqft_above)) +
  geom_histogram(binwidth=250,  color="black", fill="white") +
  scale_x_continuous(breaks=c(2000, 4000, 6000, 8000)) +
  labs(title="Distribution of sqft_above") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 6. sqft_basement

#summary statistics
summary(king$sqft_basement)
sd(king$sqft_basement)
#range = [0, 4820]
#mean = 291.5
#sd = 442.575
#median = 0 (most houses don't have a basement)

#histogram distribution of sqft_basement
ggplot(king, aes(x=sqft_basement)) +
  geom_histogram(binwidth=100,  color="black", fill="white") +
  scale_y_continuous(breaks=c(2500, 5000, 7500, 10000, 12500)) +
  labs(title="Distribution of sqft_basement") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 7. sqft_living15 

#summary statistics
summary(king$sqft_living15)
sd(king$sqft_living15)
#range = [399 - 6210]
#mean = 1987
#685.39
#median = 1840

#histogram distribution of sqft_living15
ggplot(king, aes(x=sqft_living15)) +
  geom_histogram(color="black", fill="white") +
  scale_x_continuous(breaks=c(1000, 2000, 3000, 4000, 5000, 6000)) +
  labs(title="Distribution of sqft_living15") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 8. sqft_lot15

#summary statistics
summary(king$sqft_lot15)
sd(king$sqft_lot15)
#range = [651 - 871200]
#mean = 12768
#sd = 27304.18
#median = 7620

#histogram distribution of sqft_lot15
ggplot(king, aes(x=sqft_lot15)) +
  geom_histogram(binwidth=2000,  color="black", fill="white") +
  scale_x_continuous(limits=c(0, 300000), breaks=c(5e4, 1e5, 1.5e5, 2e5, 2.5e5, 3e5)) +
  scale_y_continuous(breaks=c(1000, 2000, 3000, 4000, 5000)) +
  labs(title="Distribution of sqft_lot15") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 9. bedrooms

#summary statistics
table(king$bedrooms)  #range = [0 - 33]
#The observation with 33 bedrooms is probably an input error. Change it to 3 bedrooms.)
which(king$bedrooms==33)
king$bedrooms[15871] = 3
table(king$bedrooms)   #range = [0 - 11]

summary(king$bedrooms)
sd(king$bedrooms)      
#range = [0 - 11]
#mean = 3.37
#sd = 0.908
#median = 3

#bar plot distribution of bedrooms
ggplot(king, aes(x=bedrooms))+ 
  geom_bar(fill="black") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11)) +
  scale_y_continuous(breaks=c(2000, 4000, 6000, 8000, 10000)) +
  labs(title="Counts of bedrooms") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 10. bathrooms

#summary statistics
summary(king$bathrooms)
sd(king$bathrooms)    
#range = [0 - 8]
#mean = 2.115
#sd = 0.770
#median = 2.25

#bar plot distribution of bathrooms
ggplot(king, aes(x=bathrooms)) +
  geom_bar(fill="black") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8)) +
  scale_y_continuous(breaks=c(1000, 2000, 3000, 4000, 5000)) +
  labs(title="Counts of bathrooms") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 11. floors

#summary statistics
summary(king$floors)
sd(king$floors) 
#range = [1, 3.5]
#mean = 1.49
#sd = 0.540
#median = 1.5

#bar plot distribution of floors
ggplot(king, aes(x=floors)) +
  geom_bar(fill="black") +
  scale_x_continuous(breaks=c(1, 1.5, 2, 2.5, 3, 3.5)) +
  scale_y_continuous(breaks=c(2000, 4000, 6000, 8000, 10000)) +
  labs(title="Counts of floors") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 12. waterfront 

#summary statistics (0 = does not have a waterfront view, 1 = has a waterfront view)
table(king$waterfront)   
#21450 houses with no waterfront 
#163 houses with a waterfront  
prop.table(table(king$waterfront))    
#99.25% with no waterfront 
#0.75% with a waterfront 

#bar plot distribution of waterfront
ggplot(king, aes(x=waterfront)) +
  geom_bar(fill="black") +
  scale_x_continuous(breaks=c(0,1)) +
  expand_limits(y=24000) +
  annotate("text", x=0, y=23000, label="99.25%") +
  annotate("text", x=1, y=1700, label="0.75%") +
  labs(title="Counts of waterfront") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 13. condition

#summary statistics
summary(king$condition)
sd(king$condition)
#range = [1, 5]
#mean = 3.41
#sd = 0.65
#median= 3

#bar plot distribution of condition
ggplot(king, aes(x=condition)) +
  geom_bar(fill="black") +
  scale_y_continuous(breaks=c(2500, 5000, 7500, 10000, 12500)) +
  labs(title="Counts of condition") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 14. grade

#summary statistics
summary(king$grade)
sd(king$grade)
#range = [1 - 13]
#mean = 7.66
#sd = 1.175
#median = 7

#bar plot distribution of grade
ggplot(king, aes(x=grade)) +
  geom_bar(fill="black") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13)) +
  scale_y_continuous(breaks=c(2000, 4000, 6000, 8000, 10000)) +
  labs(title="Counts of grade") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 15. yr_built

#summary statistics
summary(king$yr_built)
sd(king$yr_built)
#range = [1900 - 2015]
#mean = 1971
#sd = 29.37
#median = 1975

#histogram distribution of yr_built
ggplot(king, aes(x=yr_built)) +
  geom_histogram(binwidth=5, color="black", fill="white") +
  labs(title="Distribution of yr_built") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 16. yr_renovated

#Determine the number of houses that were renovated at some point in the past.
king$yr_reno2 <- ifelse(king$yr_renovated==0,0,1)
table(king$yr_reno2)    
#20699 unrenovated houses
#914 renovated houses     

#Determine the average year the houses were built, based on whether they were renovated or not
aggregate(king[c("yr_built")], king[c("yr_reno2")], mean, na.rm=TRUE) 
aggregate(king[c("yr_built")], king[c("yr_reno2")], sd, na.rm=TRUE) 
#average yr built among non-renovated houses = 1972.40, sd = 28.855 
#average yr built among renovated houses = 1939.53, sd = 22.621


# 17. zipcode

length(unique(king$zipcode))    #70 zipcodes total
table(king$zipcode)             #Number of houses in each zipcode         
mean(table(king$zipcode))       #mean number of houses per zipcode = 308.7571
sd(table(king$zipcode))         #sd of number of houses per zipcode = 142.2673



##-- c. Plot the correlations among the variables--##


#Examine the correlations among the variables
king.cor <- with(king, cbind(price, sqrt_price, sqft_living, sqft_lot, sqft_above, 
                             sqft_basement, sqft_living15, sqft_lot15, bedrooms, bathrooms, 
                             floors, waterfront, condition, grade, yr_built))
cors <- cor(king.cor)
round(cors, digits=2)

#Plot the correlations
library(corrplot)
corrplot(cors, tl.col = "black", tl.srt = 45)



##-- d. Plot the relationship between sqrt_price and each predictor variable--##


# 1. Relationship between sqft_living and sqrt_price (scatter plot)
ggplot(king, aes(x=sqft_living, y=sqrt_price)) +
  geom_point(size=.5, alpha=.3) +
  scale_x_continuous(breaks=c(2000, 4000, 6000, 8000, 10000, 12000)) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title="Relationship between sqft_living and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 2. Relationship between sqft_lot and sqrt_price (scatter plot)
ggplot(king, aes(x=sqft_lot, y=sqrt_price)) +
  geom_point(size=.5, alpha=.3) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title="Relationship between sqft_lot and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 3. Relationship between sqft_above and sqrt_price (scatter plot)
ggplot(king, aes(x=sqft_above, y=sqrt_price)) +
  geom_point(size=.5, alpha=.3) +
  scale_x_continuous(breaks=c(2000, 4000, 6000, 8000, 10000)) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title="Relationship between sqft_above and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 4. Relationship between sqft_basement and sqrt_price (scatter plot)
ggplot(king, aes(x=sqft_basement, y=sqrt_price)) +
  geom_point(size=.5, alpha=.3) +
  scale_x_continuous(breaks=c(1000, 2000, 3000, 4000)) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title="Relationship between sqft_basement and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 5. Relationship between sqft_living15 and sqrt_price (scatter plot)
ggplot(king, aes(x=sqft_living15, y=sqrt_price)) +
  geom_point(size=.5, alpha=.3) +
  scale_x_continuous(breaks=c(1000, 2000, 3000, 4000, 5000, 6000)) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title="Relationship between sqft_living15 and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 6. Relationship between sqft_lot15 and sqrt_price 
ggplot(king, aes(x=sqft_lot15, y=sqrt_price)) +
  geom_point(size=.5, alpha=.3) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title="Relationship between sqft_lot15 and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 7. Relationship between bedrooms and sqrt_price (box plot)
ggplot(king, aes(x=factor(bedrooms), y=sqrt_price)) +
  geom_boxplot(aes(fill=factor(bedrooms))) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  scale_fill_viridis_d() +
  guides(fill=FALSE) +
  labs(x="bedrooms", title="Relationship between bedrooms and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 8. Relationship between bathrooms and sqrt_price (scatter plot)
ggplot(king, aes(x=bathrooms, y=sqrt_price)) +
  geom_point(size=.5, position=position_jitter(width=.05)) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8)) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title="Relationship between bathrooms and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 9. Relationship between floors and sqrt_price (box plot)
ggplot(king, aes(x=factor(floors), y=sqrt_price)) +
  geom_boxplot(aes(fill=factor(floors))) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  scale_fill_viridis_d() +
  guides(fill=FALSE) +
  labs(x="floors", title="Relationship between floors and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 10. Relationship between waterfront and sqrt_price (box plot)
ggplot(king, aes(x=factor(waterfront), y=sqrt_price)) +
  geom_boxplot(aes(fill=factor(waterfront))) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  scale_fill_viridis_d() +
  guides(fill=FALSE) +
  labs(x="waterfront", title="Relationship between waterfront and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 11. Relationship between condition and sqrt_price (box plot)
ggplot(king, aes(x=factor(condition), y=sqrt_price)) +
  geom_boxplot(aes(fill=factor(condition))) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  scale_fill_viridis_d() +
  guides(fill=FALSE) +
  labs(x="condition", title="Relationship between condition and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 12. Relationship between grade and sqrt_price (box plot)
ggplot(king, aes(x=factor(grade), y=sqrt_price)) +
  geom_boxplot(aes(fill=factor(grade))) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  scale_fill_viridis_d() +
  guides(fill=FALSE) +
  labs(x="grade", title="Relationship between grade and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# 13. Relationship between yr_built and sqrt_price (scatter plot)
ggplot(king, aes(x=yr_built, y=sqrt_price)) +
  geom_point(size=.5, alpha=.5) +
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500)) +
  labs(title="Relationship between yr_built and sqrt_price") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))

