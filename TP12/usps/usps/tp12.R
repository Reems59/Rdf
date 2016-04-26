source ("readUSPS.R")
source ("freeman.R")
uspsData = readUSPSdata(".")
tabFreeman = list()
for(i in 1:11000){
  tabFreeman[[i]] = freeman(uspsData[[1]][,,i])
}

list = list()
nbdonne_apprentissage = 50
nbdonne_test = 3
listdonne = array(0, dim=10)
cpt =1
for(i in 1:10){
  for(j in 1:nbdonne_apprentissage){
    x1 <- runif(1, 1, 1100)
    while( x1 %in% listdonne ){
      x1 <- runif(1, 1, 1100)
    }
    listdonne[j] = x1
    list[[cpt]] = tabFreeman[[(i-1)* 1100 + x1]]
    cpt = cpt +1
  }
  listdonne = array(0, dim= 10)
}
nbClasse = knn(50, list, 10, 50, tabFreeman[1])
cat('Valeur trouvée : ', nbclasse, '. Valeur réelle : 1')





levenshtein <- function( x, y ) {
  m = length(x)
  n = length(y)
  
  if(m == 0){
    return (n)
  }
  if(n == 0){
    return (m)
  }
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
      if(x[[i-1]] == y[[j-1]]){
        tmp3 = tmp3-1
      }
      mat[i ,j] = min(tmp1,
                      tmp2,
                      tmp3)
    }
  }
  return (mat[m+1,n+1]);
}

knn <- function( k, tab, m, n, test) {
  tabDistance = mat <- matrix(0 , nrow=m, ncol=n)
  for(i in 1:m){
    for(j in 1:n){
      tabDistance[i ,j] = levenshtein(tab[[(j-1)*m+i]], test)
    }
  }
  tabMinDist = array(0, c(1,m))
  maxi = max(tabDistance)
  for(i in 1:k){
    tmp = which.min(tabDistance)
    tabDistance[tmp] = maxi
    if(tmp%%m == 0){
      tabMinDist[m] = tabMinDist[m] +1
    } else {
      tabMinDist[tmp%%m] = tabMinDist[tmp%%m] +1
    }
  }
  return (which.max(tabMinDist))
}
