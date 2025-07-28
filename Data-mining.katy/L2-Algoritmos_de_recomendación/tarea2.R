#En resumen, esta es la gráfica de
#el número de grupos versus la inercia intra-clases
#Los gráficos obtenidos permiten identificar el "codo"

##Cargar librerias
install.packages("FactoMineR", dependencies = TRUE)
install.packages("calibrate", dependencies = TRUE)
library(FactoMineR)
library(ggplot2)
library(calibrate)
library(lattice)

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
