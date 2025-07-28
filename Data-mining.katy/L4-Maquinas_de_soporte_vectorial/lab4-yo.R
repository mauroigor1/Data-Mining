##Instalacion y carga de las librerias necesarias
install.if.required <- function(x){
  if(!require(x, character.only=TRUE)){
    install.packages(x, dependencies = TRUE, character.only=TRUE)
    library(class, character.only=TRUE)
  }
}

lapply(c("rpart","class", "e1071", "ROCR"), install.if.required)

if(!require(class)){
  install.packages("class", dependencies = TRUE)
  library(class)
}
if(!require(e1071)){
  install.packages("e1071", dependencies = TRUE)
  library(e1071)
}
if(!require(ROCR)){
  install.packages("ROCR", dependencies = TRUE)
  library(ROCR)
}

#Fijamos el directorio de trabajo
setwd("C:/Users/Katerin/Google Drive/2017/DATA MINING/L4-Maquinas_de_soporte_vectorial")

#Cargamos la tabla de entrenamiento y de testeo
train <- read.csv("ZipDataTrainCod.csv", sep = ";", dec = ".", header = T)
head(train)
test <- read.csv("ZipDataTestCod.csv", sep = ";", dec = ".", header = T)
head(test)

#Generamos el modelo SVM con núcleo linear
modelosvm <- svm(Numero~., data = train, kernel = "linear")

#Realizamos las predicciones sobre la tabla de testeo
prediccionsvm <- predict(modelosvm, test)

#Calculamos la matriz de confusion y la precision global
MCsvm <- table(test[,1], prediccionsvm)
MCsvm
PGsvm <- (sum(diag(MCsvm)))/(sum(MCsvm))
PGsvm

#Precisiones positivas

x <- rep(0,10)
y <- c("cero","cinco","cuatro","dos","nueve","ocho","seis","siete","tres","uno")
PrecPossvm <- data.frame(y,x)
PrecPossvm <- as.matrix(PrecPossvm)

for (i in 1:10){
  PrecPossvm[i,2] <- round(((MCsvm[i,i])/(sum(MCsvm[i,])))*100)
  
}
PrecPossvm

#Comparativa con árbol de decisión
modelorpart <- rpart(Numero~., data = train)
prediccionrpart <- predict(modelorpart, test, type = "class")
MCrpart <- table(test[,1], prediccionrpart)
MCrpart
PGrpart <- (sum(diag(MCrpart)))/(sum(MCrpart))
PGrpart

PrecPosrpart <- data.frame(y,x)
PrecPosrpart <- as.matrix(PrecPosrpart)

for (i in 1:10){
  PrecPosrpart[i,2] <- round(((MCrpart[i,i])/(sum(MCrpart[i,])))*100)
  
}
PrecPosrpart




