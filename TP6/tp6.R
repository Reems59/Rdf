
# -----1.1------
x = load(file='simul-2016.Rdata');
couleur<-rep('black',n_app)
couleur[classe_app==1]='red'
couleur[classe_app==2]='blue'
couleur[classe_app==3]='green'

couleur[classe_test==1]='red'
couleur[classe_test==2]='blue'
couleur[classe_test==3]='green'

plot(x_app, col=couleur)

# -----1.2------
M1<-seq(1,2)
M1[1] = mean(x_app[classe_app==1,1])
M1[2] = mean(x_app[classe_app==1,2])

M2<-seq(1,2)
M2[1] = mean(x_app[classe_app==2,1])
M2[2] = mean(x_app[classe_app==2,2])

M3<-seq(1,2)
M3[1] = mean(x_app[classe_app==3,1])
M3[2] = mean(x_app[classe_app==3,2])

#covariance
Sigma1<-matrix(1,2,2)
for(i in 1:2){
  for(j in 1:2){
    Sigma1[i,j]=cov(as.vector(x_app[classe_app==1,i]),
                    as.vector(x_app[classe_app==1,j]))
  }
}

Sigma2<-matrix(1,2,2)
for(i in 1:2){
  for(j in 1:2){
    Sigma2[i,j]=cov(as.vector(x_app[classe_app==2,i]),
                    as.vector(x_app[classe_app==2,j]))
  }
}

Sigma3<-matrix(1,2,2)
for(i in 1:2){
  for(j in 1:2){
    Sigma3[i,j]=cov(as.vector(x_app[classe_app==3,i]),
                    as.vector(x_app[classe_app==3,j]))
  }
}

S1Theorique = s1 %*% t(s1)
S2Theorique = s2 %*% t(s2)
S3Theorique = s3 %*% t(s3)

# -----1.3------
library("MASS")
library("lattice")
# Grille d'estimation de la densite de probabilite en 50 intervalles selon 1er attribut
xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
# Grille d'estimation de la densite de probabilite en 50 intervalles selon 2eme attribut
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)
grille<-expand.grid(x1=xp1,x2=xp2)
x_app.lda<-lda(x_app,classe_app)
# Estimation des densites de probabilites a posteriori dans Zp
grille=cbind(grille[,1],grille[,2])
Zp<-predict(x_app.lda,grille)
# classe 3 versus 2 et 1
zp<-Zp$post[,3]-pmax(Zp$post[,2],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50),add=TRUE,levels=0,drawlabels=FALSE)

# classe 2 versus 3 et 1
zp<-Zp$post[,2]-pmax(Zp$post[,3],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50),add=TRUE,levels=0,drawlabels=FALSE)


# -----1.4------
assigne_test<-predict(x_app.lda, newdata=x_test)
# Estimation des taux de bonnes classifications
table_classification_test <-table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <-sum(diag(prop.table(table_classification_test)))

# Creation du vecteur contenant le code de la forme des donnees test assignees aux classes - code initialise a 1
shape<-rep(1,n_test) ;
# forme des donnees assignees a la classe 2
#1 = rond, 2 = triangle et 3 = croix
shape[assigne_test$class==1]=1 ;
shape[assigne_test$class==2]=3 ;
shape[assigne_test$class==3]=2 ;
# Affichage avec code couleur et forme adaptees
plot(x_test,col=couleur,pch=shape,xlab = "X1", ylab = "X2");





