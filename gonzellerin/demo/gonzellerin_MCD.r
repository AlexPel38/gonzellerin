#Creation de deux vecteurs
x<-c(154,167,177,158,190,187,165,175,172,181,178,167,196,184,167,169,176,174,165,187,174,165,159,192,176,154,159)
length(x)
y<-c(67,87,65,67,84,77,69,75,84,76,79,81,93,78,85,76,68,90,84,67,64,53,66,75,64,76,67)
length(y)

#Nom des variables
nom<-c("taille","poids")

#Application de la fonction MCD
(objet<-MCD(x,y,var.nom = nom))

#Application de la fonction permettant la representation graphique
plot(objet)

#Application de la fonction permettant de predire la valeur de Y en fonction de l objet et de la varible (explicative) X
x_explic<-167
prediction(objet,x=x_explic)


