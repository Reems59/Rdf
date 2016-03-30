library ("EBImage")
# Chargement d'une image en niveaux de gris
rdfReadGreyImage <- function (nom) {
  image <- readImage (nom)
  if (length (dim (image)) == 2) {
    image
  } else {
    channel (image, 'red')
  }
}

nom <- "allFaces.png";
image <- rdfReadGreyImage (nom)

# création de stackedFaces :
# Empilement des visages 40x33 dans une matrice stackedFaces 40x33x400
# Attention, exécution possiblement longue ~30 sec, pas de panique...)
stackedFaces= array(0, dim=c(40,33,400)) # numLignes, numColonnes, numFaces
for(i in 0:19){
  for(j in 0:19){
    stackedFaces[,,(i*20 + j + 1)] = image[(1+i*33):((i+1)*33),(1+j*40):((j+1)*40)]
  }
}
#il faut calculer l'entropie pour chaque pixel de chacunes des 400 images.
entropies = array(0, dim=c(40,40,33))
for(x in 1:40){
    for(i in 1:40){
      for(j in 1:33){
        cat("x =", x)
        proba = (sum(stackedFaces[i,j,1+(x-1)*10:x*10]) / 10)
        probaInv = 1- proba
        entropies[x, i, j] = - (log2(proba^proba) + log2(probaInv^probaInv))
      }
    }
}

entropieMax = which.max(entropies)
entX = entropieMax%%40
if(entX == 0){
  entX = 1
  entY = entropiesMax / 40
}
entY = (entropieMax / 40 )+1
#View(which(entropies == max(entropies), arr.ind=TRUE))
boolean <- c()
for(i in 1:400){
  if(stackedFaces[entX, entY, i] == 0){
    boolean[i] = 0
  } else {
    boolean[i] = 1
  }
}

