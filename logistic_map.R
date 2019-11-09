#Magdalena Smieszek
# Logistics map - creating a dynamic model behaving chaotically
logistic.map<- function(r=4,a=runif(4),b=0.5,N=300, M=300,type='withouttrendu'){
  ## r: bifurcation parameter
  ## a : initial parameter
  ## x,y:  model value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  x <- 1:N
  y <- 1:N
  x <- a
  y <- a
    
  # calculate logistic map
  if (type == 'withouttrendu'){
  for(i in 3:(N-1)){
    x[i+1] <-(1-b)*r*x[i] * (1 - x[i])+b*y[i-2]
    y[i+1] <-r*y[i] * (1 - y[i])
  }
  x<-x[(N-M):N]
  y<-y[(N-M):N]
  }
  # adding a trend to the data
  else if (type == 'trend'){
     for(i in 3:(N-1)){
      y[i+1] <-r*y[i] * (1 - y[i]) + rnorm(1, 0,0.0000000001)
      x[i+1] <-r*x[i] * (1 - x[i]) + rnorm(1, 0,0.0000000001)
   }

  
  x<-x[(N-M):N]
  y<-y[(N-M):N]
  
  xm <-max(x)
  ym <-max(y)
   
  tx <- seq(0, xm, length.out = (M+1))
  ty <- seq(0, ym, length.out = (M+1))
  x <- x+tx #or x <- x+(tx*5)
  y <- y-(ty/2)
  }
  result <- list(x=x,y=y)
  return(result) 
}


#Use the logistic.map function and display values on the graph

r <-4;N <- 1500; M <- 500; start.a <- c(0.1,0.7,0.4,0.9);strong_of_coupling.b <- 0.5
orbit <- logistic.map(r,a=start.a,b=strong_of_coupling.b,N=N,M=M)
plot.ts(cbind(orbit$x,orbit$y))
title("Mapa logistyczna parametry b=0.5")



x <- orbit$x
y <- orbit$y
