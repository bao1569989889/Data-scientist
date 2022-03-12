library(datasets)
data(iris)
View(iris)

# how many different species there are present in the data set. 
unique(iris$Species)

data(mtcars)
head(mtcars,5)
?mtcars

library(ggplot2)
ggplot(aes(x=disp,y=mpg,),data=mtcars)+geom_point()
ggplot(aes(x=disp,y=mpg,),data=mtcars)+geom_point()+ggtitle("Displacement vs miles per gallon")
ggplot(aes(x=disp,y=mpg,),data=mtcars)+geom_point()+
  ggtitle("Displacement vs miles per gallon")+
  labs(x='Displacement', y="Miles per Gallon")

mtcars$vs <- as.factor(mtcars$vs)
ggplot(aes(x=vs,y=mpg),data=mtcars)+geom_boxplot()
ggplot(aes(x=vs,y=mpg),data=mtcars)+geom_boxplot(alpha=0.3)+
  theme(legend.position='none')
ggplot(aes(x=wt),data=mtcars)+geom_histogram(binwidth=.5)


library(GGally)
ggpairs(iris, mapping=ggplot2::aes(colour=Species))
