
#calculation of SCT-symbolic coupling trace

#using logistic map
N <- 1500; M <- 500; start.a <- c(0.1,0.7,0.4,0.9);strong_of_coupling.b <- 0.5;r=4
orbit <- logistic.map(r,a=start.a,b=strong_of_coupling.b,N=N,M=M)
x <- orbit$x
y <- orbit$y

#load data
#nowa <-read.table("Data_Path")
#x <- nowa[,1]
#y <- nowa[,2]

#SCT calculation
data <- SCT(x,y)
print(data)



