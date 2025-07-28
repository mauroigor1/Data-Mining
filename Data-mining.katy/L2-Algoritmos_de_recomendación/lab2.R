#En resumen, esta es la gráfica de
#el número de grupos versus la inercia intra-clases
#Los gráficos obtenidos permiten identificar el "codo"

##Cargar librerias
#install.packages("FactoMineR", dependencies = TRUE)
#install.packages("calibrate", dependencies = TRUE)
library(FactoMineR)
library(ggplot2)
library(calibrate)
library(lattice)
library(rattle)
library(base)
library(cluster)

wss <- function(d) {
  sum(scale(d, scale = FALSE)^2) 
} 
#Hace la suma del cuadrado de las diferencias de cada dato 
#con el promedios
#esta formula hace alusión a la inercia intra-clases

wrap <- function(i, hc, x) {
  cl <- cutree(hc, i) 
  #Dado un árbol hc resultante de hclust, cutree corta el árbol 
  #en la cantidad de grupos deseados dados por i.
  #si i es escalar, entonces cutree entrega un vector con asignación de 
  #cada individuo a un grupo
  #si i es un vector, entonces cutree entrega una matriz donde cada 
  #columna j corresponde al elemento j de i, y la correspondiente 
  #asignación de cada individuo a un grupo.
  spl <- split(x, cl)
  #Divide el arreglo x en grupos definidos por cl
  #split entrega las listas que contienen los valores 
  #asociados para cada grupo.
  wss <- sum(sapply(spl, wss))
  #Aplica wss a spl, con lo cual se obtiene un vector con los valores 
  #de wss a cada lista de spl. Luego suma éstos.
  #Esta corresponde exactamente a la inercia intra-clases.
  wss
}
iris2 <- iris[, 1:4] 
#Extrae desde la primera hasta la cuarta columna
#del conjunto de datos iris que trae incorporado R.
cl <- hclust(dist(iris2), method = "ward.D")
#dist(iris2): matriz de distancias
#hace hclust con el método ward
res <- sapply(seq.int(1, nrow(iris2)), wrap, hc = cl, x = iris2)
#Aplica la función wrap a los siguientes parámetros
#i=seq.int(1, nrow(iris2)), hc=cl, x=iris .
#Primero corta al árbol en 1 a 150 grupos y lo guarda en cl
#que es una matriz de 150 por i, donde cada componente de ella
#tiene como valor el grupo al cual pertenece el individuo.
#Luego separa los datos de la tabla iris2 con respecto a cada grupo
#y los guarda como lista en spl (una lista por cada grupo).
#Por último calcula la inercia intra-clases (wss) para cada valor 
#de grupos a considerar (de 1 a 150).
#La variable res es un vector de largo 150 contiene los valores de las 
#inercias intra-clases para cada caso.
plot(seq_along(res), res, type = "b", pch = 19)
#Grafica la cantidad de grupos (1 a 150) versus la 
#inercia intra-clases
plot(seq_along(res[1:50]), res[1:50], type = "o", pch = 19)
#Grafica la cantidad de grupos (1 a 50) versus la
#inercia intra-clases



##3-1

tabla<-read.csv("EjemploAlgoritmosRecomendacion.csv",sep=";",dec=",")
tabla2 <- tabla[,-1] 
##3-2
#prcomp(tabla[,-1])
res.pca<-PCA(tabla2,scale.unit=TRUE,ncp=5,ind.sup=NULL,quanti.sup=NULL,quali.sup=NULL,row.w=NULL,col.w=NULL,graph=TRUE,axes=c(1,2)) #análisis de componentes principales


plot(res.pca$ind$coord[,c(1,2)], ylim=c(-2,2))

text(res.pca$ind$coord[,c(1)],res.pca$ind$coord[,c(2)],labels=tabla[,1])

hc2<-HCPC(res.pca,kk=Inf,nb.clust=2) #cluster usando componentes principales con 2 grupos
hc3<-HCPC(res.pca,kk=Inf,nb.clust=3) #cluster usando componentes principales con 3 grupos


plot(hc2, choice ="map", draw.tree = FALSE, 
     ind.names = FALSE, centers.plot = TRUE) #centros de gravedad de los 2 grupos (para la 3.8)

plot(hc3, choice ="map", draw.tree = FALSE,
     ind.names = FALSE, centers.plot = TRUE) #centros de gravedad de los 3 grupos (para la 3.8)
##3.3

distancias<-dist(tabla2)

#res.complete<-hclust(distancias,method="complete") ##salto maximo
#res.average<-hclust(distancias,method="average") #promedio
#res.single<-hclust(distancias,method="single")##salto minimo
res.ward.D<-hclust(distancias,method="ward.D")
#res.ward.D2<-hclust(distancias,method="ward.D2")
#par(mfrow=c(2,2))
#plot(res.median)
#plot(res.complete)
plot(res.ward.D)
#plot(res.ward.D2)



##3.4-3.5
res <- sapply(seq.int(1, nrow(tabla2)), wrap, hc = res.ward.D, x = tabla2)
plot(seq_along(res[1:50]), res[1:50], type = "o", pch = 19)



ct<-cutree(res.ward.D,k= 7) #árbol cortado según el codo dado 

plot(res.ward.D) ##lo puse de nuevo porque tiene que ir justo antes de la línea que viene después
rect.hclust(res.ward.D, k = 7, which = NULL, x = NULL, h = NULL,
            border = 2, cluster = NULL)
clusplot(na.omit(tabla2), ct, color=TRUE, shade=TRUE, main='Clusters con hclust',lines=0) ##graficar clusters con hclust



#####3.6
tabla$Grupo<-1
for (i in 1:dim(tabla)[1]){
  tabla$Grupo[i]<-ct[i]
  }

write.csv(tabla,file="EjemploAlgoritmosRecomendacionHCLUST.csv",sep=";",dec=".")

##3.7
##Kmeans para hacer 2 y 3 grupos

km2<-kmeans(tabla2,centers=2)
clusplot(tabla2,km2$cluster,main='2 Clusters con K-means', lines=0)

km2$centers




km3<-kmeans(tabla2,centers=3)
clusplot(tabla2,km3$cluster,main='3 Clusters con K-means', lines=0)

km3$centers


##función para determinar el codo con kmeans
wss <- (nrow(tabla)-1)*sum(apply(tabla2,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(tabla2,
                                     centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Número Clusters",
     ylab="Suma de cuadrados intra-cluster")  ##con este método, el número de clusters óptimo es 6

km6<-kmeans(tabla2,centers=6)

##3.8
centros.hclust<-centers.hclust(tabla2,res.ward.D, nclust=7, use.median=FALSE)
centros.kmeans<-km6$centers

##graficar centros de gravedad para hclust
plot(centros.hclust[1,],type='o',ylim=c(0,7),main="Centros de gravedad con hclust",ylab="centro",xlab="componente")
lines(centros.hclust[2,],type='o',col=2)
lines(centros.hclust[3,],type='o',col=3)
lines(centros.hclust[4,],type='o',col=4)
lines(centros.hclust[5,],type='o',col=5)
lines(centros.hclust[6,],type='o',col=6)
lines(centros.hclust[7,],type='o',col=8)
legend("bottomright", c("cluster 1", "cluster 2", "cluster 3"), col = c(1, 2, 3),lwd=1)
legend("topright", c("cluster 4","cluster 5","cluster 6","cluster 7"), col = c(4, 5, 6,8),lwd=1)


##graficar centros de gravedad para kmeans
plot(centros.kmeans[1,],type='o',ylim=c(0,7),col=1,main="Centros de gravedad con kmeans",ylab="centro",xlab="componente")
lines(centros.kmeans[2,],type='o',col=2)
lines(centros.kmeans[3,],type='o',col=3)
lines(centros.kmeans[4,],type='o',col=4)
lines(centros.kmeans[5,],type='o',col=5)
lines(centros.kmeans[6,],type='o',col=6)
legend("bottomright", c("cluster 1", "cluster 2", "cluster 3"), col = c(1, 2, 3),lwd=1)
legend("topright", c("cluster 4","cluster 5","cluster 6"), col = c(4, 5, 6),lwd=1)

clusplot(tabla2,km6$cluster,main='Clusters con K-means', lines=0)

##3.9
tabla[tabla$X=="Leo",]
tabla[tabla$X=="Justin",]
tabla[tabla$X=="Teresa",]

