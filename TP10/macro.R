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


# création de stackedFaces :
# Empilement des visages 40x33 dans une matrice stackedFaces 40x33x400
# Attention, exécution possiblement longue ~30 sec, pas de panique...)

initStackedFaces <- function(){
  
  stackedFaces= array(0, dim=c(40,33,400)) # numLignes, numColonnes, numFaces
  
  for(i in 0:19){
    for(j in 0:19){
      stackedFaces[,,(i*20 + j + 1)] = image[(1+i*33):((i+1)*33),(1+j*40):((j+1)*40)]
    }
  }
  stackedFaces
}
#il faut calculer l'entropie pour chaque pixel de chacunes des 40 images.
entropie <- function(stackedFaces, etiquettes, booleans, branche){
  entropies = array(0, dim=c(40,33))
  probabilite = array(0, dim=c(40,40,33))
  if(branche == 1){
    indexB = sum(booleans)
  }else{
    indexB = length(booleans) -  sum(booleans)
  }
  for(x in 1:40){
    for(i in 1:40){
      for(j in 1:33){
        somme = cpt = 0
        for(z in 1:indexB){
          if(booleans[z] == branche && etiquettes[z] == x){
            somme = somme + stackedFaces[i, j, z]
            cpt = cpt +1
          }
          proba = somme / cpt
        }
        
        probabilite[x, i, j] = proba
      }
    }
  }
  for(i in 1:40){
    for(j in 1:33){
      proba = (sum(probabilite[, i, j]) / 40)
      probaInv = 1- proba
      entropies[i, j] = - (log2(proba^proba) + log2(probaInv^probaInv))
    }
  }
  entropies
}

createArbre <- function(stackedFaces, booleans, etiquettes){
  nbTotal = length(stackedFaces[1,1,])
  somme = sum(booleans)
  stackedFaces1= array(0, dim=c(40,33,(nbTotal - somme))) 
  stackedFaces2= array(0, dim=c(40,33,somme)) 
  etiquettes1 = c()
  etiquettes2 = c()
  cpt1 = cpt2 =1
  bool = 0
  for(i in 1 : (nbTotal -1)){
    if(etiquettes[i] != etiquettes[i+1]){
      bool = 1
    }
  }
  
  if(bool == 0){
    cat("classe n° ", etiquettes[1], "\n")
    return()
  } 
  cat("on continue \n")
  for(i in 1:nbTotal){
    if(booleans[i] == 0){
      stackedFaces1[,,cpt1] = stackedFaces[,,i]
      etiquettes1[cpt1] = etiquettes[i]
      cpt1 = cpt1 +1
    }else{
      stackedFaces2[,,cpt2] = stackedFaces[,,i]
      etiquettes2[cpt2] = etiquettes[i]
      cpt2 = cpt2 +1
    }
  }
  
  cat("un tour !! \n")
  createSousArbre(stackedFaces1, etiquettes1)
  createSousArbre(stackedFaces2, etiquettes2)
}

createSousArbre <- function(stackedFaces, etiquettes){
  nbTotal = length(stackedFaces[1,1,])
  booleans = array(0, dim = c(1,nbTotal))
  cat("sous arbre \n")
  entropies1 = entropie(stackedFaces, etiquettes, booleans, 0)
  
  entropieMax = which(entropies1 == max(entropies1), arr.ind=TRUE)
  boolean <- c()
  classCount = array(0, dim=c(2, 40))
  classRatio = array(0, dim = c(2,40))
  
  for(i in 1:nbTotal){
    if(stackedFaces[entropieMax[1,1], entropieMax[1,2],i] < 0.5){
      boolean[i] = 0
      classCount[1,etiquettes[i]] = classCount[1,etiquettes[i]] + 1
    } else {
      boolean[i] = 1
      classCount[2,etiquettes[i]] = classCount[2,etiquettes[i]] + 1
    }
  }
  for(i in 1:40){
    classRatio[1,i] = classCount[1, i]/10
    classRatio[2,i] = classCount[2, i]/10
  }
  
  #createArbre(stackedFaces, booleans, etiquettes)
}


###### MAIN ######


nom <- "allFaces.png";
image <- rdfReadGreyImage (nom)

stackedFacesT = initStackedFaces()

etiquettesT = c()
for(i in 1:400){
  if( i %% 10 == 0){
    etiquettesT[i] = as.integer(i/10)
  }else{
    etiquettesT[i] = as.integer(i / 10) +1
  }
  
}

booleanT = array(0, dim = c(1,400))

entropies1T = entropie(stackedFacesT, etiquettesT, booleanT, 0)
#entropies2 = entropie(stackedFaces, etiquettes, booleans, 1)

entropieMaxT = which(entropies1T == max(entropies1T), arr.ind=TRUE)
View(which(entropies1T == max(entropies1T), arr.ind=TRUE))
booleanT <- c()
classCountT = array(0, dim=c(2, 40))
classRatioT = array(0, dim = c(2,40))
#on cr??e un tableau de boolean en fonction des probabilit?s du pixel de pivot dans chaque classe d'image
# comme nous avons une probabilit? pour chaque pixel, il faut definir un seuil pour mettre ? 0 ou ? 1 le pixel. Le seuil choisi est la moyenne des valeurs pour la classe ayant l'entropie la plus elev?.

for(i in 1:400){
  if(stackedFacesT[entropieMaxT[1,1], entropieMaxT[1,2],i] < 0.5){
    booleanT[i] = 0
    classCountT[1,etiquettesT[i]] = classCountT[1,etiquettesT[i]] + 1
  } else {
    booleanT[i] = 1
    classCountT[2,etiquettesT[i]] = classCountT[2,etiquettesT[i]] + 1
  }
}
for(i in 1:40){
  classRatioT[1,i] = classCountT[1, i]/10
  classRatioT[2,i] = classCountT[2, i]/10
  
}

createArbre(stackedFacesT, booleanT, etiquettesT)





































entropiePREVIOUS <- function(stackedFaces){
  entropiesT = array(0, dim=c(40,33))
  probabiliteT = array(0, dim=c(40,40,33))
  for(x in 1:40){
    for(i in 1:40){
      for(j in 1:33){
        proba = (sum(stackedFaces[i,j,(1+(x-1)*10):(x*10)]) / 10)
        probabiliteT[x, i, j] = proba
        probaInv = 1- proba
      }
    }
  }
  for(i in 1:40){
    for(j in 1:33){
      proba = (sum(probabiliteT[, i, j]) / 40)
      probaInv = 1- proba
      entropiesT[i, j] = - (log2(proba^proba) + log2(probaInv^probaInv))
    }
  }
  entropiesT
}








