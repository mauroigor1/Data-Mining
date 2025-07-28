
library(rattle)
library(ggplot2) 

help(read.csv)
Y<-read.csv("CompraBicicletas.csv",header=TRUE,sep=";", dec = ",")

##Instalacion y carga de las librerias necesarias
install.if.required <- function(x){
  if(!require(x, character.only=TRUE)){
    install.packages(x, dependencies = TRUE, character.only=TRUE)
    library(class, character.only=TRUE)
  }
}

lapply(c("class", "e1071", "ROCR","rpart"), install.if.required)

 if(!require(class)){
  install.packages("class", dependencies = TRUE)
   library(class)
 }
 if(!require(e1071)){
   install.packages("e1071", dependencies = TRUE)
   library(e1071)
 }
 if(!require(ROCR)){#   install.packages("ROCR", dependencies = TRUE)
   library(ROCR)
}

##Fijamos el directorio de trabajo y cargamos la tabla de datos
Datos<-read.table('Comprabicicletas.csv', sep=";", dec=".", header=T) 
head(Datos)

##Fijamos la semilla de modo de poder comparar los resultados
set.seed(30)

#Generamos una muestra donde el 75% de los datos ser??n utilizados como tabla de aprendizaje y el25%como tabla de prueba (testing):
muestra <-sample(1:nrow(Datos),nrow(Datos)%/%(100/25)) #25 PORCIENTE
ttesting<-Datos[muestra,]
taprendizaje<- Datos[-muestra,]

#Generamos el modelo utilizando la funci??n kernel lineal:
modelosvm<-svm(PurchasedBike~.,data=taprendizaje, kernel="linear") #PUSHACE ES LO Q QEREMOS PREDECIR

#Realizamos las predicciones sobre la tabla de prueba:
prediccionsvm<-predict(modelosvm, ttesting) 

#Calculamos la matriz de confusi??n y la precisi??n global:
MCsvm<-table(ttesting[,ncol(Datos)],prediccionsvm) 
MCsvm
PGsvm<-(sum(diag(MCsvm)))/sum(MCsvm)
PGsvm

#Comparativa con ??rboles de decisi??n
library(rpart)  #LO MISMO PERO CON ARBOL
modelorpart<-rpart(PurchasedBike~.,data=taprendizaje)
prediccionrpart<-predict(modelorpart, ttesting, type="class")
MCrpart<-table(ttesting[,ncol(Datos)],prediccionrpart) 
MCrpart
PGrpart<-(sum(diag(MCrpart)))/sum(MCrpart)
PGrpart

#El siguiente c??digo permite construir la curva ROC sin la necesidad de recurrir a rattle:
modelosvm<-svm(PurchasedBike~.,data=taprendizaje, kernel="linear", probability=TRUE)
modelorpart<-rpart(PurchasedBike~.,data=taprendizaje)

prediccionsvm<-predict(modelosvm, ttesting, probability=TRUE)
prediccionrpart<-predict(modelorpart, ttesting, probability=TRUE)

prediccionsvm.rocr <- prediction(attr(prediccionsvm, "probabilities")[,2], ttesting$PurchasedBike)
prediccionrpart.rocr <- prediction(prediccionrpart[,1], ttesting$PurchasedBike)

prediccionsvm.perf <- performance(prediccionsvm.rocr, "tpr", "fpr")
prediccionrpart.perf <- performance(prediccionrpart.rocr, "fpr", "tpr" )

plot(prediccionsvm.perf, main="Curva ROC", col="blue")
plot(prediccionrpart.perf, col="red", add=TRUE)
lines(c(0,1), c(0,1), col="black")

#El ??rea bajo la curva, AUC, est?? dada por:
AUCsvm<-as.numeric(slot(performance(prediccionsvm.rocr,"auc"), "y.values"))
AUCsvm

AUCrpart<-1- as.numeric(slot(performance(prediccionrpart.rocr,"auc"), "y.values"))
AUCrpart

legend("bottomright", c(paste("SVM", "AUC:",round(AUCsvm, 3)), paste("rpart", "AUC:", round(AUCrpart,3))), lty = c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))
