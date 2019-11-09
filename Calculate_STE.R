#Magdalena Śmieszek
#calculation of transfer entropy (TE) or symbolic transfer entropy (STE) based on data generated from a logistic map or on loaded data

library("RTransferEntropy")
library("TSPred")


#using a logistic map
N <- 6000; M <- 5500; start.a <- c(0.1,0.7,0.4,0.9);strong_of_coupling.b <- 0.5
orbit <- logistic.map(r,a=start.a,b=strong_of_coupling.b,N=N,M=M)
x <- orbit$x
y <- orbit$y
#load data
nowa <-read.table("Data_path")
x <- nowa[,1]
y <- nowa[,2]

#detrend if we use STE we don't must use detrend
x <-detrend(x)
y <-detrend(y)

#using STE function
data <- STE(x,y)
 
x<-data$tabx
y<-data$taby

#calculate TE or STE if we use STE function
shannon_te <- transfer_entropy(x,y,lx=1,ly=1,quantiles = c(2.5, 97.5))
print(shannon_te)



