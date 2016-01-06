##Q1#######

fun1 <- function(x){
  y <- 0
  if(x%%2 == 0){
    print('eeny')
    y <- 1
  }
  if(x%%3 == 0){
    print('meeny')
    y <- 1
  }
  if(x%%5 == 0){
    print('miny')
    y <- 1
  }
  if(y == 0){
    print('moe')
  }
}

##ex:
fun1(60)
fun1(59)
fun1(25)


##Q2#######

fun2 <- function(v){
  c <- apply(v, 2, function(x) x%%2 == 0)
  print(sum(c == TRUE))
}

##ex:
v <- cbind(10,4,6,9,8)
fun2(v)


##Q3#######

fun3 <- function(l){
  c <- apply(l, 2, function(x) substring(x, 1, 1)=='R'||substring(x, 1, 1)=='r')
  print(sum(c == TRUE))
}

##ex:
l <- cbind(7, 'Ret', 'io', 89.9, 'ryu')
fun3(l)


##Q4#######

fun4 <- function(w,x,y,z){
  mat1 <- matrix(w, nrow=x, ncol=y)
  mat2 <- matrix(z, nrow=y, ncol=x)
  mat <- rbind(mat1, t(mat2))
  return(mat)
}

##ex:
x1 <- fun4(1,2,3,4)
x1


##Q5#######

fun_diamonds <- function(diamonds){
  count <- sum(diamonds$cut == "Ideal")
  cutset <- subset(diamonds, cut == "Ideal")
  sum <- sum(cutset$price)
  return(sum/count)
}

##ex:
setwd("C:/Users/amrita/Desktop/ABI papers/R.Proficiency.Test")
diamonds <- read.table("diam.txt", header=TRUE)
fun_diamonds(diamonds)


##Q6#######

#6.1
temp_hist <- function(temperatures){
  hist(temperatures$temp, breaks = 5, xlab = "Temperatures", main = "Histogram of temperatures")
}

#6.2
temp_boxplot <- function(temperatures){
  boxplot(temperatures$temp, xlab = "Temperatures", main = "Boxplot of temperatures")
}

##ex:
temps <- read.table("temp.txt", header=TRUE)
temp_hist(temps)
temp_boxplot(temps)


##Q7#######

library(ggplot2)

fun_plot <- function(dat1, dat2, dat3){
  ggplot() + geom_point(data=dat1, aes(x=dat1$x, y=dat1$y), color='red', shape=1, size=4) + 
    geom_point(data=dat2, aes(x=dat2$x, y=dat2$y), color='black', shape=5, size=4) + 
    geom_point(data=dat3, aes(x=dat3$x, y=dat3$y), color='blue', shape=4, size=4) + 
    xlab("x") + ylab("y") + ggtitle("Plot of three datasets")
}

##ex:
dat1 <- read.table("dat1.txt", header=TRUE)
dat2 <- read.table("dat2.txt", header=TRUE)
dat3 <- read.table("dat3.txt", header=TRUE)
fun_plot(dat1, dat2, dat3)

