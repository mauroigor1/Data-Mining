###Ejemplo variabilidad Error

setwd("~/Dropbox/Cursos UdeC/Cursos pregrado/Data Mining/Calibraci??n de modelos")
datos <- iris
head(datos)
dim(datos) #150
suppressMessages(suppressWarnings(library(kknn)))
# Vamos a generar 10 veces al azar una tabla de testing de tamaño 75 y una tabla de aprendizaje de tamano 75.
# Es decir, 50% para testeo y 50% para entrenamiento
v.error.tt<-rep(0,10)

for(i in 1:10) {
  muestra <- sample(1:150,75) 
  #1:150 , 150 es la cantidad de filas de datos
  #75 es la cantidad de datos que se tomarán
  ttesting <- datos[muestra,]
  taprendizaje <- datos[-muestra,]
  modelo <- train.kknn(Species~.,data=taprendizaje,kmax=5)
  #Validación cruzada dejando uno fuera (LOOCV)
  #kmax = número máximo de k vecinos

###Predicción sobre la tabla de testeo
  prediccion <- predict(modelo,ttesting[,-5]) 
  # Matriz de Confusión
  MC<-table(ttesting[,5],prediccion)
  # Porcentaje de buena clasificacion y de error
  acierto<-sum(diag(MC))/sum(MC)
  error <- 1-acierto
  v.error.tt[i] <- error
}
plot(v.error.tt,col="red",type="b",main="Variación del Error",xlab="Número de iteración",ylab="Esti
     mación del Error")

###Predicciones sobre la tabla completa

v.error.tc<-rep(0,10)
for(i in 1:10) {
  modelo <- train.kknn(Species~.,data=datos,kmax=5)
  prediccion <- predict(modelo,datos[,-5])
  # Matriz de Confusion
  MC<-table(datos[,5],prediccion)
  # Porcentaje de buena clasificacion y de error
  acierto<-sum(diag(MC))/sum(MC)
  error <- 1- acierto
  v.error.tc[i] <- error
}
plot(v.error.tt,col="red",type="b",ylim=c(min(v.error.tt,v.error.tc),max(v.error.tt,v.error.tc)+0.05),
     main="Variación del Error",xlab="Número de iteración",ylab="Estimación del Error")
points(v.error.tc,col="blue",type="b")
legend("topright",legend=c("Tabla de Testing","Tabla completa"),col=c("red","blue"),lty=1,lwd=1)

###Validación cruzada dejando uno fuera

v.error.loo<-rep(0,10)
n <- dim(datos)[1]
# Se hace 10 veces para verificar que no varía
for(i in 1:10) {
  errori <- 0
  # Este ciclo es el que hace "leave one out" (dejar uno afuera)
  for(j in 1:n) {
    muestra <- j
    ttesting <- datos[muestra,]
    taprendizaje <- datos[-muestra,]
    modelo <- train.kknn(Species~.,data=taprendizaje,kmax=5)
    prediccion <- predict(modelo,ttesting[,-5])
    if(prediccion != ttesting$Species)
      errori <- errori+1
  }
  v.error.loo[i] <- errori/n
}
plot(v.error.loo, col = "green", type = "b", ylim = c(min(v.error.loo, v.error.tt,v.error.tc), 
max(v.error.loo,v.error.tt,v.error.tc) + 0.05), main = "Variación del Error", xlab = "Número de iteración",
ylab = "Estimación del Error")
points(v.error.tc,col="blue",type="b")
points(v.error.tt, col = "red", type = "b")
legend("topright", legend = c("Tabla Completa","Tabla de Testing","Promedio uno afuera"), col =
         c("blue","red","Green"), lty = 1, lwd = 1)

###Validación cruzada usando K grupos
install.packages("caret",dependencies=TRUE)
suppressMessages(library(caret))
# Este paquete es usado para generar los grupos al azar
n <- dim(datos)[1] # Aquí n=150
n_grupos <- 6
n_promedio <- 10
## Vamos a generar el modelo dejando un grupo para testing y los demás datos para aprendizaje.
v.error.kg<-rep(0,n_promedio)
# Hacemos validación cruzada n_promedio veces para ver que el error se estabiliza
for(i in 1:n_promedio) {
  errori <- 0
  # Esta instrucción genera los n_grupos grupos (Folds)
  grupos <- createFolds(1:n,n_grupos) # grupos$Foldi es el i-ésimo grupo
  # Este ciclo es el que hace "cross-validation" (validación cruzada) con 6 grupos (Folds)
  for(k in 1:n_grupos) {
    muestra <- grupos[[k]] # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[muestra,]
    taprendizaje <- datos[-muestra,]
    modelo <- train.kknn(Species~.,data=taprendizaje,kmax=5)
    prediccion <- predict(modelo,ttesting[,-5])
    # Matriz de Confusión
    MC<-table(ttesting[,5],prediccion)
    # Porcentaje de buena clasificación y de error
    acierto<-sum(diag(MC))/sum(MC)
    error <- 1 - acierto
    errori <- errori + error
  }
  v.error.kg[i] <- errori/n_grupos
}
plot(v.error.kg, col = "magenta", type = "b", ylim = c(min(v.error.kg, v.error.tc,v.error.tt,v.error.loo), 
max(v.error.kg, v.error.tc,v.error.tt,v.error.loo) + 0.05), main = "Variación del Error", 
xlab = "Número de iteración", ylab = "Estimación del Error")
points(v.error.tc, col = "blue", type = "b")
points(v.error.tt, col = "red", type = "b")
points(v.error.loo, col = "green", type = "b")
legend("topright", legend = c("K-esimo grupo","Tabla completa","Tabla Testing","Promedio uno fuera"), 
       col = c("magenta", "blue","red","green"), lty = 1, lwd = 1)

###Validación cruzada
# El siguiente paquete es usado para generar los grupos al azar
suppressWarnings(suppressMessages(library(e1071)))
setwd("~/Dropbox/Cursos UdeC/Cursos pregrado/Data Mining/Calibración de modelos")
datos <- read.csv("MuestraCredito5000.csv", sep = ";", header = T)
n <- dim(datos)[1]
deteccion.no.radial <- rep(0, 5)
deteccion.no.linear <- rep(0, 5)
deteccion.no.polynomial <- rep(0, 5)
deteccion.no.sigmoid <- rep(0, 5)
# Validación cruzada 5 veces
tiempo.usual <- system.time(
  for (i in 1:5) {
    grupos <- createFolds(1:n, 10)  # Crea los 10 grupos
    no.radial <- 0
    no.linear <- 0
    no.polynomial <- 0
    no.sigmoid <- 0
    # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
    # grupos (Folds)
    for (k in 1:10) {
      muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
      ttesting <- datos[muestra, ]
      taprendizaje <- datos[-muestra, ]
      modelo <- svm(BuenPagador ~ ., data = taprendizaje, kernel ="radial")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting[, 6]
      MC <- table(Actual, prediccion)
      # Detección de los NO Pagadores
      no.radial <- no.radial + MC[1, 1]
      modelo <- svm(BuenPagador ~ ., data = taprendizaje, kernel ="linear")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting[, 6]
      MC <- table(Actual, prediccion)
      # Detección de los NO Pagadores
      no.linear <- no.linear + MC[1, 1]
      modelo <- svm(BuenPagador ~ ., data = taprendizaje, kernel ="polynomial")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting[, 6]
      MC <- table(Actual, prediccion)
      # Detección de los NO Pagadores
      no.polynomial <- no.polynomial + MC[1, 1]
      modelo <- svm(BuenPagador ~ ., data = taprendizaje, kernel ="sigmoid")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting[, 6]
      MC <- table(Actual, prediccion)
      # Detección de los NO Pagadores
      no.sigmoid <- no.sigmoid + MC[1, 1]
    }
    deteccion.no.radial[i] <- no.radial
    deteccion.no.linear[i] <- no.linear
    deteccion.no.polynomial[i] <- no.polynomial
    deteccion.no.sigmoid[i] <- no.sigmoid
  }
)
plot(deteccion.no.radial, col = "magenta", type = "b",
     ylim = c(min(deteccion.no.radial,
                  deteccion.no.linear, deteccion.no.polynomial,
                  deteccion.no.sigmoid), max(deteccion.no.radial,
                                             deteccion.no.linear,
                                             deteccion.no.polynomial,
                                             deteccion.no.sigmoid) +200),
     main = "Detección del NO pagador en SVM",
     xlab = "Número de iteración",
     ylab = "Cantidad de NO pagadores detectados")
points(deteccion.no.linear, col = "blue", type = "b")
points(deteccion.no.polynomial, col = "red", type = "b")
points(deteccion.no.sigmoid, col = "green", type = "b")
legend("topright", legend = c("Radial", "Linear", "Polynomial","Sigmoid"),
       col = c("magenta", "blue", "red", "green"), lty = 1, lwd = 1)

###Calibración svm, versión paralela
install.packages("snow", dependencies=TRUE)
install.packages("formula.tools", dependencies = TRUE)
suppressWarnings(suppressMessages(library(snow)))
clp <- makeCluster(5, type = "SOCK")
# Constructor del cluster
ignore <- clusterEvalQ(clp, {
  suppressWarnings(suppressMessages(library(e1071)))
  suppressWarnings(suppressMessages(library(formula.tools)))
  ejecutar.prediccion <- function(datos, formula, muestra,metodo,
                                  ...) {
    ttesting <- datos[muestra, ]
    taprendizaje <- datos[-muestra, ]
    modelo <- metodo(formula, data = taprendizaje, ...)
    prediccion <- predict(modelo, ttesting, type = "class")
    # Obtiene la variable dependiente de la f??rmula. Se ocupa
    # usar el paquete formula.tools
    variable.discriminante <- lhs.vars(formula)
    MC <- table(ttesting[, variable.discriminante], prediccion)
    return(MC)
  } })
n <- dim(datos)[1]
algoritmos <- c("radial", "linear", "polynomial", "sigmoid")
deteccion.no.radial <- rep(0, 5)
deteccion.no.linear <- rep(0, 5)
deteccion.no.polynomial <- rep(0, 5)
deteccion.no.sigmoid <- rep(0, 5)
tiempo.paralelo <- system.time(
  for (i in 1:5) {
    grupos <- createFolds(1:n, 10)
    no.radial <- 0
    no.linear <- 0
    no.polynomial <- 0
    no.sigmoid <- 0
    for (k in 1:10) {
      muestra <- grupos[[k]]
      ### Inserta estas 2 variables en cada pe??n
      clusterExport(clp, "datos")
      clusterExport(clp, "muestra")
      resultado <- clusterApply(clp, algoritmos,
                                function(pkernels) {
                                  MC <- ejecutar.prediccion(datos, BuenPagador ~.,
                                                            muestra, svm, kernel = pkernels)
                                  no.val <- MC[1, 1]
                                  valores <- list(Tipo <- pkernels, Resultado <-no.val,MC <- MC)
                                  valores
                                })
      for (j in 1:length(algoritmos)) {
        if (resultado[[j]][[1]] == "radial")
          no.radial <- no.radial + resultado[[j]][[2]]
        else if (resultado[[j]][[1]] == "linear")
          no.linear <- no.linear + resultado[[j]][[2]]
        else if (resultado[[j]][[1]] == "polynomial")
          no.polynomial <- no.polynomial + resultado[[j]][[2]]
        else if (resultado[[j]][[1]] == "sigmoid")
          no.sigmoid <- no.sigmoid + resultado[[j]][[2]]
      }
    }
    deteccion.no.radial[i] <- no.radial
    deteccion.no.linear[i] <- no.linear
    deteccion.no.polynomial[i] <- no.polynomial
    deteccion.no.sigmoid[i] <- no.sigmoid
  } )
stopCluster(clp)
plot(deteccion.no.radial, col = "magenta", type = "b", ylim =
       c(min(deteccion.no.radial,deteccion.no.linear,
             deteccion.no.polynomial, deteccion.no.sigmoid),
         max(deteccion.no.radial, deteccion.no.linear, deteccion.no.polynomial,
             deteccion.no.sigmoid) + 200), main = "Detección del NO pagador en SVM",
     xlab = "Número de iteración", ylab = "Cantidad de NO pagadores detectados")
points(deteccion.no.linear, col = "blue", type = "b")
points(deteccion.no.polynomial, col = "red", type = "b")
points(deteccion.no.sigmoid, col = "green", type = "b")
legend("topright", legend = c("Radial", "Linear", "Polynomial",
                              "Sigmoid"), col = c("magenta", "blue", "red", "green"), lty = 1,lwd =1)

cbind(tiempo.usual, tiempo.paralelo)
