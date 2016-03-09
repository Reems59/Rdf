# Chargement de la base de noms d'animaux
source ("rdfAnimaux.txt")
n = length(noms);
for (i in 1:n){
  print (noms[i])
} 

str2int <- function(x) { strtoi(charToRaw(x),16L)-96 }

mat = matrix(rep(0,26*n),nrow=26, ncol=n);
for (i in 1:n)
{
  c = str2int(noms[i]);
  mat[c,i] <- 1;
}

h <- function(x) {
  int = str2int(x)  
  sum(mat[int,])/length(noms)
}
most_h <- function(){
  tmp = -1
  
  for (i in 1:26){
    if(sum(mat[i,]) > tmp){
      tmp = sum(mat[i,])
      lettre = i 
    }
  }
  lettre
}

entropie <- function(x){
  h <- h(x) 
  h <- (h/length(noms))
  hinv = 1-h
  entropie = - (log2(h^h) + log2(hinv^hinv))
}