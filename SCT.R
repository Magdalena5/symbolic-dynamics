#Magdalena Smieszek
#A function that creates symbolization for the SCT-symbolic coupling trace method and calculates the T value


SCT <- function(x,y){
  
  #The data length x must be equal to the length of data y
  if (length(x) !=length(y)){
    print('Error: Length x not equal length y ')
  }
  

  
  ##Calculating the difference between adjacent data for x and y
  resultx <- numeric(length(x)-1)
  resulty <- numeric(length(y)-1)
  for (i in (seq_along(x)-1)) {
    resultx[i] <-x[i+1]-x[i]
    resulty[i] <-y[i+1]-y[i]
  }
  ##Replacing data with values 0 and 1 for x and y
  sx <- numeric(length(x)-1)
  sy <- numeric(length(y)-1)
  
  for (i in seq_along(resultx)) {
    if (resultx[i] > 0){
      sx[i] <- 1
    }else if (resultx[i] <= 0){
      sx[i] <- 0
    }else{
      print(NaN)
    }
  }
  

  for (i in seq_along(resulty)) {
    if (resulty[i] > 0){
      sy[i] <- 1
    }else if (resulty[i] <= 0){
      sy[i] <- 0
    }else{
      print(NaN)
    }}
  
  
  #creating words with a length of 3

  wx <- numeric(1)
  wy <- numeric(1)
  n<-(length(sx)-3)
  for (i in 1:n){ 
    x1<-toString(sx[i])
    x2<-toString(sx[i+1])
    x3<-toString(sx[i+2])
    wx[i]<-paste(x1,x2,x3,sep='',collapse='')
  }
  for (i in 1:n){ 
    y1<-toString(sy[i])
    y2<-toString(sy[i+1])
    y3<-toString(sy[i+2])
    wy[i]<-paste(y1,y2,y3,sep='',collapse='')
  }
  #creating a matrix with delays
  n <-length(wx)
  Tx <- rep(0,4)
  for (j in 1:5){
    a <- matrix(0,8,8, dimnames = list(c("000","001","010","011","100","101","110","111"), c("000","001","010","011","100","101","110","111")))
    for (i in 1:(n-j)){
      if (!is.null(wx[i+j]=="000") & !is.null(wy[i]=="000") & wx[i+j]=="000" & wy[i]=="000"){
        a[1,1]<- a[1,1] +1}
      else if (!is.null(wx[i+j]=="001") & !is.null(wy[i]=="000") & wx[i+j]=="001" & wy[i]=="000"){
        a[2,1]<- a[2,1] +1
      }else if (!is.null(wx[i+j]=="010") & !is.null(wy[i]=="000") & wx[i+j]=="010" & wy[i]=="000"){
        a[3,1]<- a[3,1] +1
      }else if (!is.null(wx[i+j]=="011") & !is.null(wy[i]=="000") & wx[i+j]=="011" & wy[i]=="000"){
        a[4,1]<- a[4,1] +1
      }else if (!is.null(wx[i+j]=="100") & !is.null(wy[i]=="000") & wx[i+j]=="100" & wy[i]=="000"){
        a[5,1]<- a[5,1] +1
      }else if (!is.null(wx[i+j]=="101") & !is.null(wy[i]=="000") & wx[i+j]=="101" & wy[i]=="000"){
        a[6,1]<- a[6,1] +1
      }else if (!is.null(wx[i+j]=="110") & !is.null(wy[i]=="000") & wx[i+j]=="110" & wy[i]=="000"){
        a[7,1]<- a[7,1] +1
      }else if (!is.null(wx[i+j]=="111") & !is.null(wy[i]=="000") & wx[i+j]=="111" & wy[i]=="000"){
        a[8,1]<- a[8,1] +1
      }
      else if (!is.null(wx[i+j]=="000") & !is.null(wy[i]=="001") & wx[i+j]=="000" & wy[i]=="001"){
        a[1,2]<- a[1,2] +1
      }else if (!is.null(wx[i+j]=="001") & !is.null(wy[i]=="001") & wx[i+j]=="001" & wy[i]=="001"){
        a[2,2]<- a[2,2] +1
      }else if (!is.null(wx[i+j]=="010") & !is.null(wy[i]=="001") & wx[i+j]=="010" & wy[i]=="001"){
        a[3,2]<- a[3,2] +1
      }else if (!is.null(wx[i+j]=="011") & !is.null(wy[i]=="001") & wx[i+j]=="011" & wy[i]=="001"){
        a[4,2]<- a[4,2] +1
      }else if (!is.null(wx[i+j]=="100") & !is.null(wy[i]=="001") & wx[i+j]=="100" & wy[i]=="001"){
        a[5,2]<- a[5,2] +1
      }else if (!is.null(wx[i+j]=="101") & !is.null(wy[i]=="001") & wx[i+j]=="101" & wy[i]=="001"){
        a[6,2]<- a[6,2] +1
      }else if (!is.null(wx[i+j]=="110") & !is.null(wy[i]=="001") & wx[i+j]=="110" & wy[i]=="001"){
        a[7,2]<- a[7,2] +1
      }else if (!is.null(wx[i+j]=="111") & !is.null(wy[i]=="001") & wx[i+j]=="111" & wy[i]=="001"){
        a[8,2]<- a[8,2] +1
      }
      else if (!is.null(wx[i+j]=="000") & !is.null(wy[i]=="010") & wx[i+j]=="000" & wy[i]=="010"){
        a[1,3]<- a[1,3] +1
      }else if (!is.null(wx[i+j]=="001") & !is.null(wy[i]=="010") & wx[i+j]=="001" & wy[i]=="010"){
        a[2,3]<- a[2,3] +1}
      else if (!is.null(wx[i+j]=="010") & !is.null(wy[i]=="010") & wx[i+j]=="010" & wy[i]=="010"){
        a[3,3]<- a[3,3] +1}
      else if (!is.null(wx[i+j]=="011") & !is.null(wy[i]=="010") & wx[i+j]=="011" & wy[i]=="010"){
        a[4,3]<- a[4,3] +1}
      else if (!is.null(wx[i+j]=="100") & !is.null(wy[i]=="010") & wx[i+j]=="100" & wy[i]=="010"){
        a[5,3]<- a[5,3] +1}
      else if (!is.null(wx[i+j]=="101") & !is.null(wy[i]=="010") & wx[i+j]=="101" & wy[i]=="010"){
        a[6,3]<- a[6,3] +1}
      else if (!is.null(wx[i+j]=="110") & !is.null(wy[i]=="010") & wx[i+j]=="110" & wy[i]=="010"){
        a[7,3]<- a[7,3] +1}
      else if (!is.null(wx[i+j]=="111") & !is.null(wy[i]=="010") & wx[i+j]=="111" & wy[i]=="010"){
        a[8,3]<- a[8,3] +1
      }
      else if (!is.null(wx[i+j]=="000") & !is.null(wy[i]=="011") & wx[i+j]=="000" & wy[i]=="011"){
        a[1,4]<- a[1,4] +1}
      else if (!is.null(wx[i+j]=="001") & !is.null(wy[i]=="011") & wx[i+j]=="001" & wy[i]=="011"){
        a[2,4]<- a[2,4] +1}
      else if (!is.null(wx[i+j]=="010") & !is.null(wy[i]=="011") & wx[i+j]=="010" & wy[i]=="011"){
        a[3,4]<- a[3,4] +1}
      else if (!is.null(wx[i+j]=="011") & !is.null(wy[i]=="011") & wx[i+j]=="011" & wy[i]=="011"){
        a[4,4]<- a[4,4] +1}
      else if (!is.null(wx[i+j]=="100") & !is.null(wy[i]=="011") & wx[i+j]=="100" & wy[i]=="011"){
        a[5,4]<- a[5,4] +1}
      else if (!is.null(wx[i+j]=="101") & !is.null(wy[i]=="011") & wx[i+j]=="101" & wy[i]=="011"){
        a[6,4]<- a[6,4] +1}
      else if (!is.null(wx[i+j]=="110") & !is.null(wy[i]=="011") & wx[i+j]=="110" & wy[i]=="011"){
        a[7,4]<- a[7,4] +1}
      else if (!is.null(wx[i+j]=="111") & !is.null(wy[i]=="011") & wx[i+j]=="111" & wy[i]=="011"){
        a[8,4]<- a[8,4] +1
      }
      else if (!is.null(wx[i+j]=="000") & !is.null(wy[i]=="100") & wx[i+j]=="000" & wy[i]=="100"){
        a[1,5]<- a[1,5] +1}
      else if (!is.null(wx[i+j]=="001") & !is.null(wy[i]=="100") & wx[i+j]=="001" & wy[i]=="100"){
        a[2,5]<- a[2,5] +1}
      else if (!is.null(wx[i+j]=="010") & !is.null(wy[i]=="100") & wx[i+j]=="010" & wy[i]=="100"){
        a[3,5]<- a[3,5] +1}
      else if (!is.null(wx[i+j]=="011") & !is.null(wy[i]=="100") & wx[i+j]=="011" & wy[i]=="100"){
        a[4,5]<- a[4,5] +1}
      else if (!is.null(wx[i+j]=="100") & !is.null(wy[i]=="100") & wx[i+j]=="100" & wy[i]=="100"){
        a[5,5]<- a[5,5] +1}
      else if (!is.null(wx[i+j]=="101") & !is.null(wy[i]=="100") & wx[i+j]=="101" & wy[i]=="100"){
        a[6,5]<- a[6,5] +1}
      else if (!is.null(wx[i+j]=="110") & !is.null(wy[i]=="100") & wx[i+j]=="110" & wy[i]=="100"){
        a[7,5]<- a[7,5] +1}
      else if (!is.null(wx[i+j]=="111") & !is.null(wy[i]=="100") & wx[i+j]=="111" & wy[i]=="100"){
        a[8,5]<- a[8,5] +1
      }
      else if (!is.null(wx[i+j]=="000") & !is.null(wy[i]=="101") & wx[i+j]=="000" & wy[i]=="101"){
        a[1,6]<- a[1,6] +1}
      else if (!is.null(wx[i+j]=="001") & !is.null(wy[i]=="101") & wx[i+j]=="001" & wy[i]=="101"){
        a[2,6]<- a[2,6] +1}
      else if (!is.null(wx[i+j]=="010") & !is.null(wy[i]=="101") & wx[i+j]=="010" & wy[i]=="101"){
        a[3,6]<- a[3,6] +1}
      else if (!is.null(wx[i+j]=="011") & !is.null(wy[i]=="101") & wx[i+j]=="011" & wy[i]=="101"){
        a[4,6]<- a[4,6] +1}
      else if (!is.null(wx[i+j]=="100") & !is.null(wy[i]=="101") & wx[i+j]=="100" & wy[i]=="101"){
        a[5,6]<- a[5,6] +1}
      else if (!is.null(wx[i+j]=="101") & !is.null(wy[i]=="101") & wx[i+j]=="101" & wy[i]=="101"){
        a[6,6]<- a[6,6] +1}
      else if (!is.null(wx[i+j]=="110") & !is.null(wy[i]=="101") & wx[i+j]=="110" & wy[i]=="101"){
        a[7,6]<- a[7,6] +1}
      else if (!is.null(wx[i+j]=="111") & !is.null(wy[i]=="101") & wx[i+j]=="111" & wy[i]=="101"){
        a[8,6]<- a[8,6] +1
      }
      else if (!is.null(wx[i+j]=="000") & !is.null(wy[i]=="110") & wx[i+j]=="000" & wy[i]=="110"){
        a[1,7]<- a[1,7] +1}
      else if (!is.null(wx[i+j]=="001") & !is.null(wy[i]=="110") & wx[i+j]=="001" & wy[i]=="110"){
        a[2,7]<- a[2,7] +1}
      else if (!is.null(wx[i+j]=="010") & !is.null(wy[i]=="110") & wx[i+j]=="010" & wy[i]=="110"){
        a[3,7]<- a[3,7] +1}
      else if (!is.null(wx[i+j]=="011") & !is.null(wy[i]=="110") & wx[i+j]=="011" & wy[i]=="110"){
        a[4,7]<- a[4,7] +1}
      else if (!is.null(wx[i+j]=="100") & !is.null(wy[i]=="110") & wx[i+j]=="100" & wy[i]=="110"){
        a[5,7]<- a[5,7] +1}
      else if (!is.null(wx[i+j]=="101") & !is.null(wy[i]=="110") & wx[i+j]=="101" & wy[i]=="110"){
        a[6,7]<- a[6,7] +1}
      else if (!is.null(wx[i+j]=="110") & !is.null(wy[i]=="110") & wx[i+j]=="110" & wy[i]=="110"){
        a[7,7]<- a[7,7] +1}
      else if (!is.null(wx[i+j]=="111") & !is.null(wy[i]=="110") & wx[i+j]=="111" & wy[i]=="110"){
        a[8,7]<- a[8,7] +1
      }
      else if (!is.null(wx[i+j]=="000") & !is.null(wy[i]=="111") & wx[i+j]=="000" & wy[i]=="111"){
        a[1,8]<- a[1,8] +1}
      else if (!is.null(wx[i+j]=="001") & !is.null(wy[i]=="111") & wx[i+j]=="001" & wy[i]=="111"){
        a[2,8]<- a[2,8] +1}
      else if (!is.null(wx[i+j]=="010") & !is.null(wy[i]=="111") & wx[i+j]=="010" & wy[i]=="111"){
        a[3,8]<- a[3,8] +1}
      else if (!is.null(wx[i+j]=="011") & !is.null(wy[i]=="111") & wx[i+j]=="011" & wy[i]=="111"){
        a[4,8]<- a[4,8] +1}
      else if (!is.null(wx[i+j]=="100") & !is.null(wy[i]=="111") & wx[i+j]=="100" & wy[i]=="111"){
        a[5,8]<- a[5,8] +1}
      else if (!is.null(wx[i+j]=="101") & !is.null(wy[i]=="111") & wx[i+j]=="101" & wy[i]=="111"){
        a[6,8]<- a[6,8] +1}
      else if (!is.null(wx[i+j]=="110") & !is.null(wy[i]=="111") & wx[i+j]=="110" & wy[i]=="111"){
        a[7,8]<- a[7,8] +1
      }else if(!is.null(wx[i+j]=="111") & !is.null(wy[i]=="111") & wx[i+j]=="111" & wy[i]=="111"){
        a[8,8]<- a[8,8] +1
      }
      
    }
    N<- length(wx)-j
    a <-a/N;
    rotate <- function(a) t(apply(a, 2, rev))
    # calculation of the sum of words occurring on the diagonal and opposite diagonal for the matrix
    adiam<-rotate(a)
    JSDsym<-sum(diag(a))
    JSDdiam<-sum(diag(adiam))
    Tx[j]<-JSDsym-JSDdiam
  }
  
  

  
  # calculation of critical delta T
  delta_T<-2.7005*n^(-0.5179);
  result <- list(Tx=Tx,delta_T=delta_T)
  return (result)
}
