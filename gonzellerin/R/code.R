#' Objet d analyse de deux variables quantitatives
#' @param x Premiere variable de l analyse (quantitative)
#' @param y deuxieme variable de l analyse (quantitative)
#' @param var.nom Stocker ou Definir le nom des variables
#'@importFrom stats cor cov pt var
#'@examples
#'MCD(x=c(153,165,176),y=c(57,65,73),var.nom=c("taille", "poids"))
#' @export
MCD<- function(x,y,var.nom=c("x","y")){ # creation objet MCD avec comme parametre x,y(vecteurs) et var.nom le nom des vecteurs
  #Controle pour savoir si les 2 vecteurs ont la meme longueurs
  if (length(x) != length(y) || length(x) < 2){
    stop("les vecteurs sont de longueurs differentes") #gestion erreur
  }


  instance <- list( #creation d une instance qui prend une liste
    summary_x=summary(x),
    summary_y=summary(y),
    nomx=var.nom[1],
    nomy=var.nom[2],
    x.values=x,
    y.values=y,
    beta1=cov(x,y)/var(x),
    r=cor(x,y),
    size=length(x)
  )
  instance$beta0 <- mean(y)- instance$beta1*mean(x)
  instance$d<-sum((y-(instance$beta0+instance$beta1*x))^2)
  instance$t <- instance$r/sqrt((1-instance$r^2)/(instance$size-2))
  instance$pvalue <- 2.0*pt(abs(instance$t),instance$size-2,lower.tail=F)

  class(instance) <- "mcd" #definition de la classe
  #renvoyer le résultat
  return(instance)
}

#' Affiche un resume de l objet
#' @param objet objet contenant les valeurs de la variable x et y sous forme de vecteur et cree  grace a la fonction MCD
#' @examples
#' objet<-MCD(x=c(3,5,6),y=c(7,5,3),var.nom=c("mois", "jour"))
#' fonction<-print(objet)
#' @export
print.mcd <- function(objet){
  #affichage ameliore
  cat("n = ",objet$size,"\n")
  cat("Resume variable X (",objet$nomx,") :","\n")
  for (i in 1:6){
    cat(names(summary(objet$x))[i],":",objet$summary_x[i]," ")
    if(i==6){
      cat("\n","\n")
    }
  }
  cat("Resume variable Y (",objet$nomy,") :","\n")
  for (i in 1:6){
    cat(names(summary(objet$y))[i],":",objet$summary_y[i]," ")
    if(i==6){
      cat("\n","\n")
    }
  }

  cat("Droite MCO =",objet$beta1,"x +",objet$beta0,"\n")
  cat("Distance entre droite et nuage de points =",objet$d,"\n")
  cat("coefficient de correlation = ",objet$r,"\n")
  cat("test statistic = ",objet$t,"\n")
  cat("p-value = ",objet$pvalue,"\n")
}

#' Fonction permettant d obtenir une representation graphique de l objet cree
#' @param objet objet contenant les valeurs de la variable x et y sous forme de vecteur et cree  grâce a la fonction MCD
#'@importFrom graphics abline plot points segments
#'@examples
#' objet<-MCD(x=c(3,5,6),y=c(7,5,3),var.nom=c("mois", "jour"))
#' fonction<-plot(objet)
#' @export
plot.mcd <- function(objet){
  plot(objet$x.values,objet$y.values,xlab=objet$nomx,ylab=objet$nomy,main=paste("Correlation =",round(objet$r,4)))
  points(objet$x,objet$beta0+objet$beta1*objet$x,pch=19,cex=0.75, col="red")
  abline(objet$beta0,objet$beta1)
  segments(x0=objet$x.values,y0=objet$beta0+objet$beta1*objet$x.values,x1=objet$x.values,y1=objet$y.values,lty=3,col="blue")
}



#definir une fonction generique prediction
#' Title
#' @param objet objet contenant les valeurs de la variable x et y sous forme de vecteur et cree  grâce a la fonction MCD
#' @param ... autres
#' @export
prediction <- function(objet,...){
  UseMethod(generic="prediction")
}

#fonction montrer pour l objet mcd
#' Prediction de Y en fonction de l objet et de X
#' @param objet objet contenant les valeurs de la variable x et y sous forme de vecteur et cree  grâce a la fonction MCD
#' @param x valeur de la variable explicative
#' @export
#' @example prediction.mcd(objet=MCD(x=c(3,5,6),y=c(7,5,3),var.nom=c("mois", "jour")),x=6)
prediction.mcd <- function(objet,x){
  cat("Y = ",objet$beta1*x+objet$beta0,"\n")
}
