source ("readUSPS.R")
source ("freeman.R")
uspsData = readUSPSdata(".")
tabFreeman = list()
for(i in 1:11){
  
  tabFreeman[[i]] = freeman(uspsData[[1]][,,i])
}



levenshtein <- function( x, y ) {
  m = length(x)
  n = length(y)
  mat <- matrix(0 , nrow=m+1, ncol=n+1)
  
  for( i in 1:(m+1)){
    mat[i,1] = i-1
  }
  for( i in 1:(n+1)){
    mat[1,i] = i-1
  }
  
  for(i in 2:(m+1)){
    for(j in 2:(n+1)){
      tmp1 = mat[i-1,j]+1
      tmp2 = mat[i, j-1]+1
      tmp3 = mat[i-1,j-1]+1 
      if(x[i-1] == y[j-1]){
        tmp3 = tmp3-1
      }
      mat[i ,j] = min(tmp1,
                      tmp2,
                      tmp3)
    }
  }
  return (mat[m+1,n+1]);
}