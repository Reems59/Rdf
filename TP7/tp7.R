#-- Kroll Rémi Charneux Dimitri


library("MASS")
library("lattice")
# -----1.1------
load(file='x_app.data');
load(file='classe_app.data');
load(file='x_test.data');
load(file='classe_test.data');


couleur<-rep('black', length(x_app));
couleur[classe_app==1]='red'
couleur[classe_app==2]='green'
couleur[classe_app==3]='blue'

couleur2<-rep('black', length(x_test));
couleur2[classe_test==1]='red'
couleur2[classe_test==2]='green'
couleur2[classe_test==3]='blue'

plot(x_app, col=couleur);
plot(x_test, col=couleur2);

# -----2.1------
cov = cov(x_app)
X <- x_app
Vp<- eigen(cov)
pente <- Vp$vectors[2,1]/Vp$vectors[1,1];
#abline(a = 0, b = pente, col = "red");

scalarProduct <- X
scalarProduct <- X %*% (Vp$vectors[,1]) / sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1]))
x_app_ACP <- X
y = classe_app
x_app_ACP[,1]= scalarProduct *Vp$vectors[1,1];
x_app_ACP[,2]= scalarProduct *Vp$vectors[2,1];

points(x_app_ACP[y==1,], col="red")
points(x_app_ACP[y==2,], col="green")
points(x_app_ACP[y==3,], col="blue")

# -----2.3------

x_app_ACP.lda<-lda(scalarProduct,classe_app)
assigne_app<-predict(x_app_ACP.lda)
# Estimation des taux de bonnes classifications
table_classification_app <-table(classe_app, assigne_app$class)
# table of correct class vs. classification
diag(prop.table(table_classification_app, 1))
# total percent correct
taux_bonne_classif_app <-sum(diag(prop.table(table_classification_app)))
# couleur de la classe 1 LABEL ORIGINAL
couleur<-rep("black",length(x_app)) ;
couleur[classe_app==1]='red'
couleur[classe_app==2]='green'
couleur[classe_app==3]='blue'
# forme de la classe 1 LABEL ASSIGNATION
shape<-rep(1,length(x_app)) ;

shape[assigne_app$class==1]=1 ;
shape[assigne_app$class==2]=2 ;
shape[assigne_app$class==3]=3 ;
# Affichage des projections apprentissage class´ees
plot(x_app,col=couleur,pch=shape,xlab = "X1", ylab = "X2")


# -----2.4------

cov = cov(x_test)
X <- x_test
Vp<- eigen(cov)
pente <- Vp$vectors[2,1]/Vp$vectors[1,1];
abline(a = 0, b = pente, col = "red");

scalarProduct <- X
scalarProduct <- X %*% (Vp$vectors[,1]) / sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1]))
x_app_ACP <- X
y = classe_test
x_app_ACP[,1]= scalarProduct *Vp$vectors[1,1];
x_app_ACP[,2]= scalarProduct *Vp$vectors[2,1];

points(x_app_ACP[y==1,], col="red")
points(x_app_ACP[y==2,], col="green")
points(x_app_ACP[y==3,], col="blue")

x_app_ACP.lda<-lda(scalarProduct,classe_test)
assigne_app<-predict(x_app_ACP.lda)
# Estimation des taux de bonnes classifications
table_classification_app <-table(classe_test, assigne_app$class)
# table of correct class vs. classification
diag(prop.table(table_classification_app, 1))
# total percent correct
taux_bonne_classif_app <-sum(diag(prop.table(table_classification_app)))
# couleur de la classe 1 LABEL ORIGINAL
couleur<-rep("black",length(x_test)) ;
couleur[classe_test==1]='red'
couleur[classe_test==2]='green'
couleur[classe_test==3]='blue'
# forme de la classe 1 LABEL ASSIGNATION
shape<-rep(1,length(x_test)) ;

shape[assigne_app$class==1]=1 ;
shape[assigne_app$class==2]=2 ;
shape[assigne_app$class==3]=3 ;
# Affichage des projections apprentissage class´ees
plot(x_test,col=couleur,pch=shape,xlab = "X1", ylab = "X2")
#abline(a = 0, b = pente, col = "red");


#Q5
mean1 <- colMeans(x_app[classe_app==1,])
mean2 <- colMeans(x_app[classe_app==2,])
mean3 <- colMeans(x_app[classe_app==3,])

# moyenne mean de l'ensemble des données d'apprentissage
mean <- colMeans(x_app)

S1 <- cov(x_app[classe_app==1,])
S2 <- cov(x_app[classe_app==2,])
S3 <- cov(x_app[classe_app==3,])

Sw = S1+S2+S3
# covariance inter-classe
Sb = (mean1-mean)%*%t(mean1-mean) + (mean2-mean) %*%t(mean2-mean) + (mean3-mean)%*%t(mean3-mean)

#Q6

#resolution equation
invSw = solve(Sw)
invSw_by_Sb = invSw %*% Sb
Vp <- eigen(invSw_by_Sb)

# Affichage de la droite corrspondant au vecteur propre
# dont la valeur propre la plus élevée
pente <- Vp$vectors[2,1]/Vp$vectors[1,1]
abline(a = 0, b  = pente, col = "blue")
#produit scalaire
scalarProduct_app <- x_app
scalarProduct_app <- x_app %*% (Vp$vectors[,1] / sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1])))

#projection des points
x_app_ACP <- x_app
x_app_ACP[,1] = scalarProduct_app * Vp$vectors[1,1]
x_app_ACP[,2] = scalarProduct_app * Vp$vectors[2,1]


#Q7
 
 
 #affichage des points projetes
 points(x_app_ACP[classe_app==1,], col="red")
 points(x_app_ACP[classe_app==2,], col="green")
 points(x_app_ACP[classe_app==3,], col="blue")
 
 #////////////////// ALD ///////////////////////
 x_app_ACP.lda<-lda(ScalarProduct_app, classe_app)
 assigne_app<-predict(x_app_ACP.lda, newdata = ScalarProduct_app)
 # Estimation des taux de bonnes classifications
 table_classification_app <-table(classe_app, assigne_app$class)
 print("matrice de confusion :")
 print(table_classification_app)
 
 # table of correct class vs. classification
 diag(prop.table(table_classification_app, 1))
 # total percent correct
 taux_bonne_classif_app <-sum(diag(prop.table(table_classification_app)))
 print(paste("taux de bonne classification", taux_bonne_classif_app))
 
 # forme : les classe d'assignation fournie par l'ALD
 shape<-rep(1,n_app)
 shape[assigne_app$class==2]=2
 shape[assigne_app$class==3]=3
 # Affichage des projections apprentissage classees
 plot(x_app,col=couleur2,pch=shape,xlab = "X1", ylab = "X2")



#Q8

  #produit scalaire
  scalarProduct_test <- x_test
  scalarProduct_test <- x_test %*% (Vp$vectors[,1] / sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1])))
  #projection des points
  x_test_ACP <- x_test
  x_test_ACP[,1] = scalarProduct_test * Vp$vectors[1,1]
  x_test_ACP[,2] = scalarProduct_test * Vp$vectors[2,1]
  #affichage des points projetes
  points(x_test_ACP[classe_test==1,], col="red")
  points(x_test_ACP[classe_test==2,], col="green")
  points(x_test_ACP[classe_test==3,], col="blue")
  #////////////////// ALD ///////////////////////
  x_app_ACP.lda<-lda(ScalarProduct_test, classe_app)
  assigne_app<-predict(x_app_ACP.lda, newdata = ScalarProduct_test)
  # Estimation des taux de bonnes classifications
  table_classification_app <-table(classe_app, assigne_app$class)
  print("matrice de confusion :")
  print(table_classification_app)

  
  # table of correct class vs. classification
  diag(prop.table(table_classification_app, 1))
  # total percent correct
  taux_bonne_classif_app <-sum(diag(prop.table(table_classification_app)))
  print(paste("taux de bonne classification", taux_bonne_classif_app))
  
  # forme : les classe d'assignation fournie par l'ALD
  shape<-rep(1,n_app)
  shape[assigne_app$class==2]=2
  shape[assigne_app$class==3]=3
  # Affichage des projections apprentissage classees
  plot(x_test,col=couleur,pch=shape,xlab = "X1", ylab = "X2")