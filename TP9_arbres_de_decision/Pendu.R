# Chargement de la base de noms d'animaux
source ("rdfAnimaux.txt")
n = length(noms);
for (i in 1:n){
  print (noms[i])
} 

str2int <- function(x) { strtoi(charToRaw(x),16L)-96 }

int2str <- function(x) { intToUtf8(x + 64) }

mat = matrix(rep(0,26*n),nrow=26, ncol=n);
for (i in 1:n)
{
  c = str2int(noms[i]);
  mat[c,i] <- 1;
}

h <- function(mat) {
  h <- numeric(26)
  for (i in 1:26){
    h[i] <- sum(mat[i,])/length(mat[1,])
  }
  h
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

entropie <- function(mat){
  h <- h(mat) 
  proba <- (h/length(mat[1,]))
  probaInv = 1-proba
  - (log2(proba^proba) + log2(probaInv^probaInv))
}

sousEnsembleAvec <- function(matrice, lettre){
  len = length(matrice[1,])
  tmp = matrix(rep(0,26*len),nrow=26, ncol=len);
  cpt = 0
  for(i in 1:len){
    if(matrice[lettre, i] == 1){
      cpt = cpt +1
      tmp[,cpt] = matrice[,i]
    }
  }
  if(cpt == 0){ 
    newMat = matrix(rep(0,26*0),nrow=26, ncol=0);
  } else {
    newMat = matrix(rep(0,26*cpt),nrow=26, ncol=cpt);
    for(i in 1:cpt){
      newMat[,i] = tmp[,i]
    }
  }
  newMat
}

sousEnsembleSans <- function(mat, lettre){
  len = length(mat[1,])
  tmp = matrix(rep(0,26*len),nrow=26, ncol=len);
  cpt = 0
  for(i in 1:len){
    if(mat[lettre, i] == 0){
      cpt = cpt +1
      tmp[,cpt] = mat[,i]
    }
  }
  if(cpt == 0){ 
    newMat = matrix(rep(0,26*0),nrow=26, ncol=0);
  } else {
    newMat = matrix(rep(0,26*cpt),nrow=26, ncol=cpt);
    for(i in 1:cpt){
      newMat[,i] = tmp[,i]
    }
  }
  newMat
}

partage <- function(mots){
  ent = entropie(mots)
  lettre = wich.max(ent)
  ensContient = c()
  ensContientPas = c()
  
}

trouveLeMot <- function(matrice){
  for(i in 1:n){
    if(identical(mat[,i], matrice[,1])){
      mot = noms[i];
    }
  }
  mot
}

jouer <- function(){
  matrice = mat
  lettreDemande = c()
  cpt = 1
  while(length(matrice[1,]) > 1){
    ent = entropie(mat)
    for(var in lettreDemande){
      ent[var] = 0
    }
    
    lettre = which.max(ent)
    lettreDemande[cpt] = lettre
    cat("Le mot contient-il la lettre", int2str(lettre), "?")
    rep = readline()
    if(rep == "oui"){
      matrice = sousEnsembleAvec(matrice, lettre)
    } else {
      matrice = sousEnsembleSans(matrice, lettre)
    }
    cpt = cpt +1
  }
  if(length(matrice[1,]) == 1){
    mot = trouveLeMot(matrice)
    cat("le mot est", mot)
  } else {
    cat("mot non trouvé")
  }
}

lancementArbre <- function(){
  arbreDeDecision(mat, c(), 1, "")
}

arbreDeDecision <- function(matrice, lettreDemandeDecision, cptArbreDecision, texte){
  if(length(matrice[1,]) == 1){
    mot <-  trouveLeMot(matrice)
    cat(texte, mot, "\n")
  } else if(length(matrice[1,]) > 1){
    ent <-  entropie(mat)
    for(var in lettreDemandeDecision){
      ent[var] = 0
    }
    
    lettre <-  which.max(ent)
    lettreDemandeDecision[cptArbreDecision] = lettre
    tmp <- cptArbreDecision
    cptArbreDecision <-  cptArbreDecision +1
    texteTmp <- paste(texte, " ,avec", int2str(lettre))
    arbreDeDecision(sousEnsembleAvec(matrice, lettre), lettreDemandeDecision, cptArbreDecision, texteTmp)
    texteTmp <- paste(texte, " ,sans", int2str(lettre))
    arbreDeDecision(sousEnsembleSans(matrice, lettre), lettreDemandeDecision, cptArbreDecision, texteTmp)
  }
}
