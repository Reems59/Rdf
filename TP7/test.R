
library ("EBImage")
library("MASS")
library("lattice")

(load(file='x_app.data'))
(load(file='classe_app.data'))
(load(file='x_test.data'))
(load(file='classe_test.data'))

n_test <- 300

couleur2<-rep('red',n_test)
couleur2[classe_test==2]='blue'
couleur2[classe_test==3]='green'

plot(x_test, col = couleur2, main="test")

#//////////// Analyse linéaire discriminante /////////////
# Grille d estimation de la densite de probabilite en 50 intervalles selon 1er attribut
xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
# Grille d estimation de la densite de probabilite en 50 intervalles selon 2eme attribut
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)
grille<-expand.grid(x1=xp1,x2=xp2)
x_app.lda<-lda(x_app,classe_app)
grille = cbind(grille[,1], grille[,2])
Zp<-predict(x_app.lda,grille)
assigne_test<-predict(x_app.lda, newdata=x_test)
# Estimation des taux de bonnes classifications
table_classification_test <-table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <-sum(diag(prop.table(table_classification_test)))
print(paste("taux de bonne classification : ", taux_bonne_classif_test))

# Creation du vecteur contenant le code de la forme des donnees test assignees aux classes - code initialise a 1
shape<-rep(1,n_test)
# forme des donnees assignees a la classe 2
shape[assigne_test$class==2]=2
shape[assigne_test$class==3]=3
# Affichage avec code couleur et forme adaptees
plot(x_test,col=couleur2,pch=shape,xlab = "X1", ylab = "X2")

xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)
grille<-expand.grid(x1=xp1,x2=xp2)
grille=cbind(grille[,1],grille[,2])
Zp<-predict(x_app.lda,grille)
zp<-Zp$post[,3]-pmax(Zp$post[,2],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50),add=TRUE,levels=0,drawlabels=FALSE)

zp2<-Zp$post[,2]-pmax(Zp$post[,3],Zp$post[,1])
contour(xp1,xp2,matrix(zp2,50),add=TRUE,levels=0,drawlabels=FALSE)