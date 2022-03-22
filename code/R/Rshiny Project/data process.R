library("datasets")


library("shiny")
library("ggpubr")
library("magrittr")
library("dplyr")
library("tidyr")
library("plyr")
library("tidyverse")
library("ggplot2")
library("hrbrthemes")
library("gmodels")
library("plotly")
library("skimr")
library("devtools")
library("visdat")
library("DataExplorer")
library("psych")
library("gapminder")
library("readxl")
library("writexl")
library("ggpubr")
library("rstatix")
library("pwr")
library("BSDA")
library("psych")
library("corrgram")
library("GGally")
library("data.table")


install.packages('rsconnect')

rsconnect::setAccountInfo(name='baoxiaodajb',
                          token='7250241C8B7F3E92F4C2D31B1C79ADDE',
                          secret='<SECRET>')

data = read.csv("2015_Street_Tree_Census_-_Tree_Data.csv")


data1 <- data %>% select(-1, -2,-3,-5,-11,-23,-25,-26,-27,-28,-29,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45)

a <- (ftable(data1$health,data1$borough))

r <- as.data.frame.matrix(a)

colnames(r) <- c("Bronx", "Brooklyn", "Manhattan","Queens","Staten Island")

row.names(r)[row.names(r) == "2"] <- "Fair"
row.names(r)[row.names(r) == "3"] <- "Good"
row.names(r)[row.names(r) == "4"] <- "Poor"
r <- r[-1,]


row.names(r)




r2 <- as.data.frame.matrix(ftable(data1$status,data1$borough))

colnames(r2) <- c("Bronx", "Brooklyn", "Manhattan","Queens","Staten Island")

row.names(r2)[row.names(r2) == "1"] <- "Alive"
row.names(r2)[row.names(r2) == "2"] <- "Dead"
row.names(r2)[row.names(r2) == "3"] <- "Stump"

s = summary(data1)

d <- as.data.frame(tapply(data1$tree_dbh,data1$spc_common,mean))
d <- tibble::rownames_to_column(d)
d <- filter(d, d$rowname != "")
colnames(d) <- c("tree species","tree diameter mean")
d <- d[order(d$`tree diameter mean`),]

d1<-head(d,66) 
d2<- tail(d,66)


barplot(height = d$`tree diameter mean`,names = d$`tree species`)

