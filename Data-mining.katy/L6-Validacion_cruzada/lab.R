#### Lab seleccion y calibración de modelos ######

setwd("C:/Users/Katerin/Google Drive/2017/DATA MINING/L6-Validacion_cruzada")

## Problema 1
datos <- read.csv("SAheart.csv", sep = ";", dec = ".")

# 1) Comparando con la cantidad de SI detectados

install.packages("caret",dependencies=TRUE) #para hacer grupos al azar
install.packages("pbkrtest",dependencies=TRUE)
install.packages("quantreg",dependencies=TRUE)
library(caret)
library(ada)

head(datos)
n <- dim(datos)[1] #462
n.grupos <- 6
n.promedio <- 10
deteccion.discrete <- rep(0, n.promedio)
deteccion.real <- rep(0, n.promedio)
deteccion.gentle <- rep(0, n.promedio)

# Validación cruzada 10 veces
  set.seed(20)
  for (i in 1:n.promedio) {
    grupos <- createFolds(1:n, n.grupos)  # Crea los 6 grupos
    discrete <- 0
    real <- 0
    gentle <- 0
    # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 6
    # grupos (Folds)
    for (k in 1:n.grupos) {
      muestra <- grupos[[k]] 
      # Por ser una lista requiere de doble paréntesis
      ttesting <- datos[muestra, ]
      taprendizaje <- datos[-muestra, ]
      modelo<- ada(chd~.,data=taprendizaje,iter=20,nu=1,type="discrete")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting$chd
      MC <- table(Actual, prediccion)
      # Detección de los SI chd
      discrete <- discrete + MC[2, 2]
      
      modelo<- ada(chd~.,data=taprendizaje,iter=20,nu=1,type="real")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting$chd
      MC <- table(Actual, prediccion)
      # Detección de los SI chd
      real <- real + MC[2, 2]
      
      modelo<-ada(chd~.,data=taprendizaje,iter=20,nu=1,type="gentle")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting$chd
      MC <- table(Actual, prediccion)
      # Detección de los SI chd
      gentle <- gentle + MC[2, 2]
      
    }
    deteccion.discrete[i] <- discrete
    deteccion.real[i] <- real
    deteccion.gentle[i] <- gentle
  }

plot(deteccion.discrete, col = "magenta", type = "b",
     ylim = c(min(deteccion.discrete,
                  deteccion.real, deteccion.gentle),
              max(deteccion.discrete,
                  deteccion.real, deteccion.gentle) +20),
     main = "Deteccion del SI chd",
     xlab = "Numero de iteración",
     ylab = "Cantidad de SI")
points(deteccion.real, col = "blue", type = "b")
points(deteccion.gentle, col = "red", type = "b")
legend("topright", legend = c("Discrete", "Real", "Gentle"),
       col = c("magenta", "blue", "red"), lty = 1, lwd = 1)
#No se puede determinar cuál algoritmo es mejor, pues la cantidad de SI
#detectados oscila sin ninguna tendencia clara. 
sum(deteccion.discrete)
sum(deteccion.real)
sum(deteccion.gentle)

# 2) Comparando con el error global

v.error.discrete <- rep(0, n.promedio)
v.error.real <- rep(0, n.promedio)
v.error.gentle <- rep(0, n.promedio)
# Validación cruzada 10 veces
  set.seed(20)
  for (i in 1:n.promedio) {
    grupos <- createFolds(1:n, n.grupos)  # Crea los 6 grupos
    error.discrete <- 0
    error.real <- 0
    error.gentle <- 0
    # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 6
    # grupos (Folds)
    for (k in 1:n.grupos) {
      muestra <- grupos[[k]]  
      # Por ser una lista requiere de doble paréntesis
      ttesting <- datos[muestra, ]
      taprendizaje <- datos[-muestra, ]
      
      modelo<- ada(chd~.,data=taprendizaje,iter=20,nu=1,type="discrete")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting$chd
      MC <- table(Actual, prediccion)
      # Detección de los SI chd
      acierto.discrete <- sum(diag(MC))/sum(MC)
      error.discrete <- (1-acierto.discrete) + error.discrete
      
      modelo<- ada(chd~.,data=taprendizaje,iter=20,nu=1,type="real")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting$chd
      MC <- table(Actual, prediccion)
      # Detección de los SI chd
      acierto.real <- sum(diag(MC))/sum(MC)
      error.real <- (1-acierto.real) + error.real
      
      modelo<-ada(chd~.,data=taprendizaje,iter=20,nu=1,type="gentle")
      prediccion <- predict(modelo, ttesting)
      Actual <- ttesting$chd
      MC <- table(Actual, prediccion)
      # Detección de los SI chd
      acierto.gentle <- sum(diag(MC))/sum(MC)
      error.gentle <- (1-acierto.gentle) + error.gentle
      
    }
    v.error.discrete[i] <- error.discrete/n.grupos
    v.error.real[i] <- error.real/n.grupos
    v.error.gentle[i] <- error.gentle/n.grupos
  }
plot(v.error.discrete, col = "magenta", type = "b",
     ylim = c(min(v.error.discrete,
                  v.error.real, v.error.gentle),
              max(v.error.discrete,
                  v.error.real, v.error.gentle)+0.05),
     main = "Variación del Error",
     xlab = "Numero de iteración",
     ylab = "Estimación del error")
points(v.error.real, col = "blue", type = "b")
points(v.error.gentle, col = "red", type = "b")
legend("topright", legend = c("Discrete", "Real", "Gentle"),
       col = c("magenta", "blue", "red"), lty = 1, lwd = 1)

sum(v.error.discrete)
sum(v.error.real)
sum(v.error.gentle)

#En base a los resultados obtenidos, el método funciona mejor con los 
#algoritmos discrete y real, mirando los gráficos, y el que funciona peor
#es el algoritmo gentle. Esto se ve con el error acumulado en cada iteración
#que tiene cada algoritmo

# 3) Basándose en la suma acumulada de SI y en el error acumulado
#el mejor algoritmo es el real

# 4) Comparar SVM, KNN, Bayes, Árboles, Bosques, Potenciación y Redes

install.packages("kknn", dependencies = TRUE) # paquete para k vecinos
install.packages("class", dependencies = TRUE) # paquete para árbol
install.packages("randomForest", dependencies = TRUE) # paquete para bosque
install.packages('neuralnet', dependencies = TRUE) # paquete para redes
library(e1071) # librería para naiveBayes
library(kknn) # librería para k vecinos más cercanos
library(rpart) # librería para árbol
library(randomForest) # librería para bosque
library(ada) # librería para potenciación
library(neuralnet) #librería para redes

# Validación cruzada 10 veces
#vectores de deteccion de los SI
deteccion.svm <- rep(0,n.promedio)
deteccion.knn <- rep(0,n.promedio)
deteccion.bay <- rep(0,n.promedio)
deteccion.arb <- rep(0,n.promedio)
deteccion.bos <- rep(0,n.promedio)
deteccion.pot <- rep(0,n.promedio)
deteccion.rna <- rep(0,n.promedio)

 set.seed(20)
 for(i in 1:n.promedio){
   grupos <- createFolds(1:n, n.grupos) #crea los 6 grupos
   svm <- 0
   knn <- 0
   bay <- 0
   arb <- 0
   bos <- 0
   pot <- 0
   rna <- 0
   for(k in 1:n.grupos){
     muestra <- grupos[[k]] #Tomamos como muestra cada grupo
     ttesting <- datos[muestra,] #Testeamos con los datos de la muestra
     taprendizaje <- datos[-muestra,] #Entrenamos con el resto de los datos
     #Modelo Máquina de soporte vectorial
     modelo <- svm(chd~., data = taprendizaje, kernel = "linear")
     prediccion <- predict(modelo, ttesting)
     Actual <- ttesting$chd
     MC <- table(Actual, prediccion)
     svm <- svm + MC[2,2]
     ind[k] <- MC[2,2]
     #Modelo K vecinos más cercanos
     modelo <- train.kknn(chd~., data = taprendizaje, kmax = 7)
     prediccion <- predict(modelo, ttesting)
     Actual <- ttesting$chd
     MC <- table(Actual, prediccion)
     knn <- knn + MC[2,2]
     #Modelo Bayes
     modelo <- naiveBayes(chd~., data = taprendizaje)
     prediccion <- predict(modelo, ttesting)
     Actual <- ttesting$chd
     MC <- table(Actual, prediccion)
     bay <- bay + MC[2,2]
     #Modelo Árbol
     modelo <- rpart(chd~., data = taprendizaje)
     prediccion <- predict(modelo, ttesting, type = "class")
     Actual <- ttesting$chd
     MC <- table(Actual, prediccion)
     arb <- arb + MC[2,2]
     #Modelo Bosques
     modelo <- randomForest(chd~., data = taprendizaje, ntree = 400, mtry = 3)
     prediccion <- predict(modelo, ttesting)
     Actual <- ttesting$chd
     MC <- table(Actual, prediccion)
     bos <- bos + MC[2,2]
     #Modelo Potenciación
     modelo<- ada(chd~., data = taprendizaje, iter=20, type="real")
     prediccion <- predict(modelo, ttesting)
     Actual <- ttesting$chd
     MC <- table(Actual, prediccion)
     pot <- pot + MC[2,2]
     #Modelo Red neuronal
     var.ind <- colnames(datos)[1:9]
     var.dep <- colnames(datos)[10]
     ind <- paste(var.ind, collapse = ' + ')
     form <- paste(var.dep, "~ ", ind)
     form <- as.formula(form)
     datosred <- datos
     datosred[,-c(5,10)] <- scale(datosred[,-c(5,10)])
     binfamhist <- model.matrix(~famhist, data = datosred)
     datosred$famhist <- binfamhist[,2]
     datosred[,-c(10)] <- scale(datosred[,-c(10)])
     binchd <- model.matrix(~chd, data = datosred)
     datosred$chd <- binchd[,2]
     ttestred <- datosred[muestra,]
     taprendred <- datosred[-muestra,]
     modelo <- neuralnet(form, data = taprendred, 
                         hidden = 60, linear.output = FALSE)
     prediccion <- compute(modelo, ttestred[,-10])
     prediccion$net.result <- apply(prediccion$net.result, 2, round)
     Actual <- ttestred$chd
     MC <- table(Actual, prediccion$net.result)
     rna <- rna + MC[2,2]
   }
   deteccion.svm[i] <- svm
   deteccion.knn[i] <- knn
   deteccion.bay[i] <- bay
   deteccion.arb[i] <- arb
   deteccion.bos[i] <- bos
   deteccion.pot[i] <- pot
   deteccion.rna[i] <- rna
 }
# Gráfico
 plot(deteccion.svm, col = "magenta", type = "b",
      ylim = c(min(deteccion.svm, deteccion.knn, deteccion.bay,
                   deteccion.arb, deteccion.bos, deteccion.pot),
               max(deteccion.svm, deteccion.knn, deteccion.bay,
                   deteccion.bos, deteccion.pot, deteccion.rna) +40),
      main = "Deteccion del SI chd",
      xlab = "Numero de iteración",
      ylab = "Cantidad de SI")
 points(deteccion.knn, col = "blue", type = "b")
 points(deteccion.bay, col = "red", type = "b")
 points(deteccion.arb, col = "green", type = "b")
 
 points(deteccion.bos, col = "black", type = "b")
 points(deteccion.pot, col = "orange", type = "b")
 points(deteccion.rna, col = "brown", type = "b")
 legend("topright", legend = c("SVM", "KKNN", "Bayes",
                               "Árbol", "Bosque", 
                               "Potenciación", "Red neuronal"),
        col = c("magenta", "blue", "red", "green", "black",
                "orange", "brown"), lty = 1, lwd = 1,
        cex = 0.8)

