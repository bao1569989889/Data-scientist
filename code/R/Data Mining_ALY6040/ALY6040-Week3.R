install.packages("factoextra")
install.packages("cluster")
library(readxl)
library(ggplot2)
library(lattice)
library(caret)
library(plyr)
library(dplyr)
library(ggcorrplot)
library(cluster)
library(factoextra)
setwd("/Users/tank/Desktop/ALY6040 data mining")

# Create a vector of Excel files to read
files.to.read <- list.files(pattern="xlsx")

# Read each file and write it to csv
lapply(files.to.read, function(f) {
  ice = read_excel("/Users/tank/Desktop/ALY6040 data mining/KickstarterData_Facts-1.xlsx")
  write.csv(ice, gsub("xlsx", "csv", f), row.names=FALSE)
})
ice <- read.csv("/Users/tank/Desktop/ALY6040 data mining/KickstarterData_Facts-1.csv",stringsAsFactors = FALSE)
str(ice)
summary(ice)
# number of rows with missing values
colSums(is.na(ice))
# check duplicate data
sum(duplicated(ice$Donate.ID))
#one-hot encode for all data
dmy <- dummyVars(" ~.", data = ice)
dat_transformed <- data.frame(predict(dmy, newdata = ice))
glimpse(dat_transformed)

ggcorrplot(round(cor(dat_transformed), 2), 
           type = "lower", 
           lab = TRUE, 
           lab_size = 1.5,
           tl.cex = 6,
           title = 
             "Correlation matrix of the icecubed dataset")
#fit a regression model
l1<-glm(How.many.desserts.do.you.eat.a.week~Ice.Cream.Products.Consumed.Per.Week+Genderfemale+Deposit.Amount , data=dat_transformed)
summary(l1)

# distribution correction ___Binning
summary(ice$How.many.desserts.do.you.eat.a.week)
bins <- c(-Inf, 5, 9, Inf)
bin_names <- c("Low", "Mid7", "High")
ice$How.many.desserts.do.you.eat.a.week_new <- cut(ice$How.many.desserts.do.you.eat.a.week, breaks = bins, labels = bin_names)
summary(ice$How.many.desserts.do.you.eat.a.week)
summary(ice$How.many.desserts.do.you.eat.a.week_new)

ice1<-ice[,c(1,4,6,11,12)]
str(ice1)

# # Determine number of clusters
fviz_nbclust(ice1, kmeans, method = "wss")
fviz_nbclust(ice1, kmeans, method = "silhouette")
# wss <- (nrow(ice1)-1)*sum(apply(ice1,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(ice1,
#                                       centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit1 <- kmeans(ice1, 2,nstart = 25)
fit2 <- kmeans(ice1, 3,nstart = 25)
# fit2 <- kmeans(ice1, 4,nstart = 25)
# fit3 <- kmeans(ice1, 5,nstart = 25)
# 3 cluster solution
par(mfrow=c(2,2))
# fviz_cluster(fit1, data = ice1)
# fviz_cluster(fit2, data = ice1)
fviz_cluster(fit1, geom = "point",  data = ice1) + ggtitle("k = 2")
fviz_cluster(fit2, geom = "point",  data = ice1) + ggtitle("k = 3")
# p2 <- fviz_cluster(fit2, geom = "point",  data = ice1) + ggtitle("k = 4")
# p3 <- fviz_cluster(fit3, geom = "point",  data = ice1) + ggtitle("k = 5")
# library(gridExtra)
# grid.arrange(p1, p2, p3, nrow = 2)
# get cluster means
# aggregate(ice1,by=list(fit1$cluster),FUN=mean)
#print(fit1)

#PCA
install_github("vqv/ggbiplot")
library(usethis)
library(devtools)
library(scales)
library(grid)
library(ggbiplot)
library("FactoMineR")
res.pca <- PCA(ice[,c(1,4,6,11,12)], graph = FALSE,ncp=3)
summary(res.pca)
# Eigenvalues 
eig.val <- get_eigenvalue(res.pca)
eig.val
# fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
#plot
fviz_pca_var(res.pca)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

ice.pca <- prcomp(ice[,c(1,4,6,11,12)], center = TRUE,scale. = TRUE)
summary(ice.pca)


# Results for Variables 提取PCA结果中变量的信息
# res.var <- get_pca_var(res.pca)
# res.var$coord          # Coordinates
# res.var$contrib        # Contributions to the PCs
# res.var$cos2           # Quality of representation 
# 
# test.pr<-princomp(res.pca,cor=TRUE)

# str(ice.pca)
# fviz_pca_biplot(ice.pca,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = "#696969"  # Individuals color
# )


# names(ice1)
# mydata_pcr <-princomp(ice[,c(1,4,6,11,12)],data=ice,cor=T)
#  summary(mydata_pcr,loadings = TRUE)
 