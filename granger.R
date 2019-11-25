#Magdalena Smieszek

#library
library(lmtest)
library(zoo)

#Using logisticmaps to create a model 
N <- 2000; M <- 500; start.a <- c(0.1,0.7,0.4,0.9);strong_of_coupling.b <- 0.5
orbit <- logistic.map(r,a=start.a,b=strong_of_coupling.b,N=N,M=M)
x <- orbit$x
y <- orbit$y


#Using logisticmaps to create a model
#nowa <-read.table("Data Path",sep=" ")
#x <- nowa[,1]
#y <- nowa[,2]

#'detrend' 
x <-detrend(x)
y <-detrend(y)

y1 <-grangertest(x,y, order = 1)
x1 <-grangertest(y,x, order = 1)
print(y1)
print(x1)





