##Magdalena Smieszek
## Symbolization for the method: (STE)-symbolic transfer entropy



STE <- function(x,y){
  
  # a sliding window for uploading data from the smallest to the largest in the window
  data_newx<-slidingWindows(x, 3)
  data_new_sortx<-t(apply(data_newx, 1, sort))
  data_newy<-slidingWindows(y, 3)
  data_new_sorty<-t(apply(data_newy, 1, sort))
  
  #creating tables for words
  N<-length(x)-2
  tabx <- numeric(N)
  taby <- numeric(N)
  
  # Creating words and saving to tables
  
  for (i in 1:N){
    if (data_newx[i,1]== data_new_sortx[i,1] & data_newx[i,2]== data_new_sortx[i,2] & data_newx[i,3]== data_new_sortx[i,3]){
      tabx[i]= 123}
    else if( data_newx[i,1]== data_new_sortx[i,1] & data_newx[i,3]== data_new_sortx[i,2] & data_newx[i,2]== data_new_sortx[i,3]) {
      tabx[i]= 132}
    else if( data_newx[i,2]== data_new_sortx[i,1] & data_newx[i,1]== data_new_sortx[i,2] & data_newx[i,3]== data_new_sortx[i,3]){
      tabx[i]= 213}
    else if( data_newx[i,2]== data_new_sortx[i,1] & data_newx[i,3]== data_new_sortx[i,2] & data_newx[i,1]== data_new_sortx[i,3]){
      tabx[i]= 231}
    else if( data_newx[i,3]== data_new_sortx[i,1] & data_newx[i,1]== data_new_sortx[i,2] & data_newx[i,2]== data_new_sortx[i,3]){
      tabx[i]= 312}
    else if( data_newx[i,3]== data_new_sortx[i,1] & data_newx[i,2]== data_new_sortx[i,2] & data_newx[i,1]== data_new_sortx[i,3]){
      tabx[i]= 321}
  }
  
  for (i in 1:N){
    if (data_newy[i,1]== data_new_sorty[i,1] & data_newy[i,2]== data_new_sorty[i,2] & data_newy[i,3]== data_new_sorty[i,3]){
      taby[i]= 123}
    else if( data_newy[i,1]== data_new_sorty[i,1] & data_newy[i,3]== data_new_sorty[i,2] & data_newy[i,2]== data_new_sorty[i,3]){
      taby[i]= 132}
    else if( data_newy[i,2]== data_new_sorty[i,1] & data_newy[i,1]== data_new_sorty[i,2] & data_newy[i,3]== data_new_sorty[i,3]){ 
      taby[i]= 213}
    else if( data_newy[i,2]== data_new_sorty[i,1] & data_newy[i,3]== data_new_sorty[i,2] & data_newy[i,1]== data_new_sorty[i,3]){
      taby[i]= 231}
    else if( data_newy[i,3]== data_new_sorty[i,1] & data_newy[i,1]== data_new_sorty[i,2] & data_newy[i,2]== data_new_sorty[i,3]){
      taby[i]= 312}
    else if( data_newy[i,3]== data_new_sorty[i,1] & data_newy[i,2]== data_new_sorty[i,2] & data_newy[i,1]== data_new_sorty[i,3]){
      taby[i]= 321}
  }
  result <- list(tabx=tabx,taby=taby)
  return (result)
}


