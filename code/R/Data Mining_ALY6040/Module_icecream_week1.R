install.packages("magrittr")
library("tidyverse")
library("magrittr")
library("dplyr")
library("ggplot2")
setwd("/Users/tank/Desktop/ALY6040 data mining")

icecream <- readxl::read_xlsx("/Users/tank/Desktop/ALY6040 data mining/KickstarterData.xlsx")

str(icecream)
glimpse(icecream)
summary(icecream)

#Check missing data
# is.na(icecream$`Deposit Amount`)
#icecream %>% summarise(count=sum(is.na(icecream$`Household Income`)))
icecream %>% summarise(na = sum(is.na(`Deposit Amount`)),
                       med = median(`Ice Cream Products Consumed Per Week`, na.rm=TRUE))
icecream %>% summarise(na = sum(is.na(`Ice Cream Products Consumed Per Week`)),
                                med = median(`Ice Cream Products Consumed Per Week`, na.rm=TRUE))

#replecae NA data with median and mean
icecream <- icecream %>% 
  mutate(`Deposit Amount` 
                    = replace(`Deposit Amount`,
                              is.na(`Deposit Amount`),
                              median(`Deposit Amount`,na.rm = TRUE)))
icecream <- icecream %>% 
  mutate(`Ice Cream Products Consumed Per Week`
         = replace(`Ice Cream Products Consumed Per Week`,
                   is.na(`Ice Cream Products Consumed Per Week`),
                   mean(`Ice Cream Products Consumed Per Week`,na.rm = TRUE)))

#check NA agian
icecream %>% summarise(count=sum(is.na(`Deposit Amount`)))
icecream %>% summarise(count=sum(is.na(`Ice Cream Products Consumed Per Week`)))
summary(icecream)
glimpse(icecream)

# Delect String NA
icecream1 <- icecream %>% filter(icecream$`Household Income`!="NA")
glimpse(icecream1)
summary(icecream1)

#Check anomalies and delete outliers
p <- ggplot(icecream,aes(y=`Deposit Amount`,x=`Gender`))+
  geom_boxplot()+
  geom_point()+
  geom_jitter(shape=16, position=position_jitter(0.2))
p

# no applicable method for 'filter' applied to an object of class "logical"
# icecream2 <- filter(icecream$`Deposit Amount`< 2500)

icecream2 <- icecream1 %>% filter(icecream1$`Deposit Amount`< 2500)
glimpse(icecream2)
summary(icecream2)
p1 <- ggplot(icecream2,aes(y=`Deposit Amount`,x=`Gender`))+
  geom_boxplot(col="red")+
  geom_point()+
  geom_jitter(shape=16, posirtion=position_jitter(0.2),col ="blue")
p1

#plot
ggplot(icecream2,aes(x=`Ice Cream Products Consumed Per Week`,color=`Gender`))+
  geom_histogram(bins = 40,binwidth = 0.5,fill="grey")

ggplot(icecream2,aes(x=`Preferred Color of Device`,colour=`Gender`))+
  geom_bar(fill="grey",width=0.5)
  
ggplot(icecream2,aes(x=`Favorite Flavor Of Ice Cream`,colour=`Gender`))+
  geom_bar(fill="grey",width=0.5)

ggplot(icecream2,aes(x=`How many desserts do you eat a week`,colour=`Gender`))+
  geom_bar(fill="grey",width=0.5)

ggplot(icecream2,aes(x=`Donated To Kick Starter Before`,colour=`Household Income`))+
  stat_count(geom= "bar",width = 0.5,fill="grey")
