
install.packages("ggplot2")
install.packages("scatterplot3d")
install.packages("corrplot")
library(ggplot2)
library(scatterplot3d)
library(corrplot)

#Para los colores de la matriz
col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
                           "cyan", "#007FFF", "blue","#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))  
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F"))   
wb <- c("white","black")
#colores para los graficos de la matriz

#EJERCICIO 1
#a)
Y <- read.csv("EjemploAlgoritmosRecomendacion.csv", sep=';', dec=',',header=TRUE,row.names=1)
library(ggplot2) 
nrow <- dim(Y)[1]
ncol <- dim(Y)[2]
entrega <- 1:nrow
precio <- 1:nrow
durabilidad <-1:nrow 
for(i in 1:nrow){
  entrega[i]<- Y[i,"Velocidad.Entrega"];
  precio[i] <- Y[i,"Precio"];
  durabilidad[i] <- Y[i,"Durabilidad"];}
#gráfico con plot
plot(entrega,precio, xlab = "Entrega", 
     ylab = "Precio",main = "Entrega versus Precio " ) 
#gráfico con qplot
  qplot(entrega,precio, col = "red", xlab="Entrega", ylab="Precio", 
      main = "Entrega versus precio")+
    theme(plot.title = element_text(hjust = 0.5))+ #centra e título
    theme(legend.position = "none") #quita la leyenda

#b)
#gráfico en 3D
scatterplot3d(entrega,precio,durabilidad,
              main= "Entrega versus precio versus durabilidad",
              xlab = "Entrega",
              ylab = "Precio",
              zlab = "Durabilidad",
              pch = 16, color="steelblue")

#c)
#Matriz de correlación
windows(width = 6, height = 6)
corY <- cor(Y)
corrplot(corY, method="number", 
         title= "Matriz de correlación con valores",
         mar=c(0,0,1,0))
corrplot(corY, title = "Matriz de correlación con círculos",
         mar=c(0,0,1,0))
corrplot(corY, order="hclust", addrect=2, 
         col=wb, bg="gold2", 
         title = "Matriz de correlación usando 'hclust'",
         mar =c(0,0,1,0)) # Clasifica de acuerdo a clusters
corrplot(abs(corY),order="AOE", 
         col=col3(200), cl.lim=c(0,1),
         title = "Matriz de correlación usando 'AOE'",
         mar = c(0,0,1,0)) 
#Las mas correlacionadas cerca de la diagonal


#d)
#Detección de puntos atípicos mediante Boxplot
##Sólo tres variables tenían valores atípicos
boxplot(Y)
boxplot(Y$Imagen.Producto, 
        main = "Diagrama de Caja para la Imagen del Producto")
eps <- 0.05
z<-locator()
text(z, rownames(Y[which(Y[,4]>z$y-eps & Y[,4]<z$y+eps),]), adj = 0) 
##locator no da el valor exacto, por eso se busca la coincidencia en un intervalo chico
#Acá se entrega a Salome

boxplot(Y$Servicio.Retorno, main ="Diagrama de Caja para el Servicio  de Retorno")
w<-locator()

for (i in 1:length(w$y)){
  if (length(rownames(Y[which(Y[,6]>w$y[i]-eps & Y[,6]<w$y[i]+eps),]))>1){  ##si más de un valor aparece en ese intervalo
    for (j in 1:length(rownames(Y[which(Y[,6]>w$y[i]-eps & Y[,6]<w$y[i]+eps),]))){
      text(y=w$y[i],x=2/3+j/3, rownames(Y[which(Y[,6]>w$y[i]-0.1 & Y[,6]<w$y[i]+0.1),])[j],adj=0) ##para que no se superpongan los
    }
  }
  else{
    text(w$y[i], rownames(Y[which(Y[,6]>w$y[i]-eps & Y[,6]<w$y[i]+eps),]),adj=0)
  }

}
#Entrega a Emilia Josephine, Salome y Philip


boxplot(Y$Valor.Educativo, 
        main = "Diagrama de Caja para el Valor Educativo")
w2<-locator()
text(w2, rownames(Y[which(Y[,5]>w2$y-eps & Y[,5]<w2$y+eps),]), adj = 0) ##locator no da el valor exacto, por eso se busca la coincidencia en un intervalo chico
#Entrega a Teofan


##Ejercicio 2
x<-c(1,2,3,4,5)
y<-1:5
z<-seq(1,5)
x==y
y==z #Solo despliega TRUE, por transitividad x==z


#Ejercicio 3
#Con c() debemos definirlo componente a componenete
y <- c(1,3,5,7)
y <- seq(1,8,2)
#Ejercicio 4
x<-c(2,-5,4,6,-2,8)
j <- 0
for(i in 1:length(x)){
  if(x[i]>0){
    j<- j+1;
  }
}

y<-integer(j)
j <- 1
for(i in 1:length(x)){
  if(x[i]>0){
    y[j]<-x[i]
    j <- j+1;
  }
}
y
#######################3
j <- 0
for(i in 1:length(x)){
  if(x[i]< 0){
    j<- j+1;
  }
}
z<-integer(j)
j <- 1
for(i in 1:length(x)){
  if(x[i]<0){
    z[j]<-x[i]
    j <- j+1;
  }
}
z
#######################3
v<- integer(length(x)-1)
for(i in 1:length(x)-1){
    v[i]<-x[i+1]
}
v
##############################
if(length(x)%%2==0){
  
w<-integer(length(x)/2)

} else  {
  w <- integer((length(x)/2)+1)
}


for(i in 1:length(w)){
  w[i]<-x[2*i-1]
  
}
w
