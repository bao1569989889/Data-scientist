# :	冒号运算符，用于创建一系列数字的向量。
# %in%	用于判断元素是否在向量里，返回布尔值，有的话返回 TRUE，没有返回 FALSE。
# %*%	用于矩阵与它转置的矩阵相乘。
# sqrt(n)	n的平方根
# exp(n)	自然常数e的n次方，
# log(m,n)	m的对数函数，返回n的几次方等于m
# log10(m)	相当于log(m,10)

 #repeat
repeat{
if(condition){
  break
 }
}

#example
v<-c("google","Rundo")
cnt<-2
repeat{
  print(v)
  cnt<-cnt+1
  if(cnt>5){
    break
  }
 }

# while 
while(condition){
  statements
}

#example
cnt<-2
while(cnt<7){ 
  print(v)
  cnt = cnt+1 #"="和“<-"相同
}

#for
for (value in vector){
  statements
}

#example
for (i in 1:5){
  print(v)
}
V <- letters[1:4]
for(i in V){
  print(i)
}
V1 <- LETTERS[1:4]
for(i in V1){
  print(i)
}

V2<- LETTERS[1:10]
for(i in V2){
  if(i=="D"){#不能i=="D"|"G":only for numeric, logical or complex types
    next
    }
  print(i)
  }
#############################
异常值处理
盖帽法：整行替换数据框里99%以上和1%以下的点，将99%以上的点值=99%的点值；小于1%的点值=1%的点值

q1 <- quantile(hon_01$USER_AGE, 0.01)        #取得时1%时的变量值  
q99 <- quantile(hon_01$USER_AGE, 0.99)       #取得时99%时的变量值
hon_01[hon_01$USER_AGE < q1,]$USER_AGE <- q1  
hon_01[hon_01$USER_AGE > q99,]$USER_AGE < -q99  
summary(hon_01$USER_AGE)      #盖帽法之后，查看数据情况  
————————————————
版权声明：本文为CSDN博主「bigdata老司机」的原创文章，遵循CC 4.0 BY-SA版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/yawei_liu1688/article/details/79175433