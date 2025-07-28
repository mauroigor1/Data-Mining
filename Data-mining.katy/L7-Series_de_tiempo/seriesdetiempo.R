#Directorio de trabajo
setwd("C:/Users/alumno/Downloads/Laboratorio")

######### Pregunta 1 #############

#Cargamos los archivos del cajero desde el 108 al 116

Caj108<-read.csv("cajero108.csv",header=F,dec=".",sep=";") 
Caj109<-read.csv("cajero109.csv",header=F,dec=".",sep=";") 
Caj110<-read.csv("cajero110.csv",header=F,dec=".",sep=";") 
Caj111<-read.csv("cajero111.csv",header=F,dec=".",sep=";") 
Caj112<-read.csv("cajero112.csv",header=F,dec=".",sep=";") 
Caj113<-read.csv("cajero113.csv",header=F,dec=".",sep=";") 
Caj114<-read.csv("cajero114.csv",header=F,dec=".",sep=";") 
Caj115<-read.csv("cajero115.csv",header=F,dec=".",sep=";") 
Caj116<-read.csv("cajero116.csv",header=F,dec=".",sep=";") 

#Dimensiones 
dim(Caj108)
dim(Caj109)
dim(Caj110)
dim(Caj111)
dim(Caj112)
dim(Caj113)
dim(Caj114)
dim(Caj115)
dim(Caj116)
 
#Como todas las dimensiones no son iguales debemos cambiarlas
Caj108<-t(Caj108)
Caj109<-t(Caj109)
Caj110<-t(Caj110)
Caj111<-t(Caj111)
Caj112<-t(Caj112)

#Ahora transformamos estos 'vectores' en series de tiempo
Caj108<-ts(Caj108[,1],start=c(2008,1),freq=365)
Caj109<-ts(Caj109[,1],start=c(2008,1),freq=365)
Caj110<-ts(Caj110[,1],start=c(2008,1),freq=365)
Caj111<-ts(Caj111[,1],start=c(2008,1),freq=365)
Caj112<-ts(Caj112[,1],start=c(2008,1),freq=365)
Caj113<-ts(Caj113[,1],start=c(2008,1),freq=365)
Caj114<-ts(Caj114[,1],start=c(2008,1),freq=365)
Caj115<-ts(Caj115[,1],start=c(2008,1),freq=365)
Caj116<-ts(Caj116[,1],start=c(2008,1),freq=365)

#Vemos que cada una corresponde a una serie de tiempo
str(Caj108)
str(Caj109)
str(Caj110)
str(Caj111)
str(Caj112)
str(Caj113)
str(Caj114)
str(Caj115)
str(Caj116)

######## Pregunta 2 #########

install.packages("itsmr", dependencies = TRUE)
library(itsmr)

#Cargamos los datos
str(dowj)
str(strikes)
str(Sunspots)
str(wine)

#Transformamos a series de tiempo y graficamos
tdowj<-ts(dowj, start=c(1972, 241), freq=365) # Buscar lo de start y end
plot(tdowj)
tstrikes<- ts(strikes, start=1951, end=1980, freq=1)
plot(tstrikes)
tSunspots<- ts(Sunspots, start=1870, end=1979, freq=1)
plot(tSunspots)
twine<- ts(wine, start=c(1980,1), end=c(1991,10), freq=12)
plot(twine)

########### Problema 3 ##############

DJTable<-read.csv("DJTable.csv",header=T,dec=".",sep=";") 
tail(DJTable) #ve las últimas filas
head(DJTable) #ve las primeras filas

#notamos que la tabla tiene las fechar en orden descendente
#por tanto, hay que invertir la tabla
CSCO<-rev(DJTable$CSCO)
CVX<-rev(DJTable$CVX)
DD<-rev(DJTable$DD)

#Ahora transformamos esta variable en serie de tiempo
#Verificamos que sean series de tiempo
#Graficamos
CSCO<-ts(CSCO, start=c(2010, 1), freq=365)
CVX<-ts(CVX, start=c(2010, 1), freq=365)
DD<-ts(DD, start=c(2010, 1), freq=365)
str(CSCO)
str(CVX)
str(DD)
plot(CSCO)
plot(CVX)
plot(DD)

########## Problema 4 ##########

Caj103 <- read.csv("Cajero103.csv", sep = ";", dec = ".")
dim(Caj103)

#Como Caj103 es un dataframe, debemos cambiarlo a un vector de columna
Caj103<-as.matrix(Caj103)  
Caj103<-as.vector(Caj103)
length(Caj103)

#Ahora, lo convertimos en una serie de tiempo
Caj103<-ts(Caj103,start=c(1998,1),freq=365) 
plot(Caj103)

#Para verificar la normalidad de los residuos graficamos el histrograma
#de diferenciar la serie
hist(diff(Caj103),prob=T,ylim=c(0,2.5e-07),col="red")
#Probabilidad = True ????
lines(density(diff(Caj103)),lwd=2) 
mu<-mean(diff(Caj103)) 
sigma<-sd(diff(Caj103)) 
x<-seq(-1e+07,1e+07,length=1000) 
y<-dnorm(x,mu,sigma) 
lines(x,y,lwd=2,col="blue")
#vemos que se ajusta a una normal

############# Problema 5 ###########

#Vamos a hacer un suavizado lineal con a=4, a=6 y a=10
plot(Caj103, type="l")
st.1<- filter(Caj103,filter=rep(1/9,9)) #qué es filter???
st.2<-filter(Caj103,filter=rep(1/13,13)) 
st.3<-filter(Caj103,filter=rep(1/21,21)) 
lines(st.1,col="red") 
lines(st.2,col="purple") 
lines(st.3,col="blue")
#con el suavizado reducimos la variabilidad

############ Problema 6 ############

#Descomponemos
#Tendencia, ciclo, estacionalidad y aleatoriedad
DescCaj103<-stl(Caj103,s.window="periodic") 
head(DescCaj103$time.series)
plot(DescCaj103)

########## Problema 7 ###########

#El período es 12 pues se espera que afecte la época del año
suppressMessages(library(itsmr))

str(deaths)
Deaths<-ts(deaths, start=c(1973,1), freq=12)
res<-spec.pgram(Deaths, log = "no") #????
res.ord<-order(res$spec,res$freq,decreasing=TRUE) #????
res.ord[1:4]
#Vemos que los máximos se encuentran en las posiciones 6,12y306,12y30.

max1<-res$freq[6]
period1<-12/max1
max2<-res$freq[12]
period2<-12/max2
max3<-res$freq[30]
period3<-12/max3
c(period1,period2,period3)
res<-spec.pgram(Deaths, log = "no")
abline(v=max1, lty="dotted",col="red")
abline(v=max2, lty="dotted",col="blue")
abline(v=max3, lty="dotted",col="magenta")

#Modelo ajustado
fit<-arima(Deaths,order=c(1,2,1),seasonal=list(order=c(2,1,2),period=12))

#Predicciones
LH.pred<-predict(fit,n.ahead=12) #modelo arima (fit), predicción a 12 meses

layout(matrix(c(1,1,2,3),2,2,byrow=TRUE)) 
plot(Deaths,xlim=c(1973,1980),ylim=c(7000,12000),type="o") 
lines(LH.pred$pred,col="green",type="o") 
lines(LH.pred$pred+2*LH.pred$se,col="green",lty=3,type="o") 
lines(LH.pred$pred-2*LH.pred$se,col="green",lty=3,type="o")
acf(Deaths,main="Autocorrelación Simple",col="black",ylim=c(-1,1)) 
pacf(Deaths,main="Autocorrelaciónn Parcial",col="black",ylim=c(-1,1))
#Banda de confianza demasiado ancha
#En el tiempo, cerca de un año, la predicción se vuelve mala,
#pues toma cualquier valor posible entre el rango de valores
#que ha tomado la variable. Lo ideal es usar la predicción hecha en el
#tiempo inmediato, e incluir este valor para hacer la próxima predicción
#En la autocorrelacion parcial se ve la periciodad, el período 11 afecta el 1


############## Problema 8 ##############

Caj101<-read.csv("Cajero101.csv",header=F,dec=".",sep=";") 
Caj101<-t(Caj101)#Matriz transpuesta 
Caj101<-ts(Caj101[,1],start=c(2008,1),freq=365) #Serie de tiempo
plot(Caj101,type="o",col="blue") #Grafica serie de tiempo

res<-spec.pgram(Caj101, log = "no") #Periodiograma
res.ord<-order(res$spec,res$freq,decreasing=TRUE) #ordenarla de manera decreciente
#respecto de la frecuencia
res.ord[1:4] #extraemos las 4 mayores
#Y ahora calculamos el período
max1<-res$freq[101]
period1<-365/max1
max2<-res$freq[439]
period2<-365/max2
max3<-res$freq[220]
period3<-365/max3
c(period1,period2,period3)
res<-spec.pgram(Caj101, log = "no")
abline(v=max1, lty="dotted",col="red")
abline(v=max2, lty="dotted",col="blue")
abline(v=max3, lty="dotted",col="magenta")
#15 no debería estar en 15 en la gráfica

#Ahora aplicamos SARIMA con periodo 15

fit<-arima(Caj101,order=c(1,1,2),seasonal=list(order=c(2,1,2),period=15))

#Predicciones
LH.pred<-predict(fit,n.ahead=365) #modelo arima (fit), predicción a 12 meses

layout(matrix(c(1,1,2,3),2,2,byrow=TRUE)) 
plot(Caj101,xlim=c(2008,2013),ylim=c(min(Caj101)-3500000,max(Caj101)+1000000),type="o") 
lines(LH.pred$pred,col="green",type="o") 
lines(LH.pred$pred+2*LH.pred$se,col="green",lty=3,type="o") 
lines(LH.pred$pred-2*LH.pred$se,col="green",lty=3,type="o")
acf(Caj101,main="Autocorrelación Simple",col="black",ylim=c(-1,1)) 
pacf(Caj101,main="Autocorrelaciónn Parcial",col="black",ylim=c(-1,1))
#Cuando vemos los lag, el del primer período está influido por el del segundo
#y el del segundo está influido por su tiempo anterior
#pero esto no implica que este último afecta al primero
#es sólo por la dependencia de los lag

########### Problema 9 ###########

install.packages("ecm", dependencies = TRUE)
install.packages("forecast", dependencies = TRUE)
library(ecm)
suppressMessages(library(forecast))

ER <- function(Pron,Real) {
  return(sum(abs(Pron-Real))/abs(sum(Real)))
}

# mean squared error (MSE)
ECM<-function(Pred,Real) {
  N<-length(Real)
  ss<-sum((Real-Pred)^2)
  return((1/N)*ss)
}

PFA <- function(Pron,Real) {
  Total<-0
  N<-length(Pron)
  for(i in 1:N) {
    if(Pron[i]>=Real[i])
      Total<-Total+1      
  }
  return(Total/N)
}

PTFA <- function(Pron,Real) {
  Total<-0
  SReal<-0
  N<-length(Pron)
  for(i in 1:N) {
    if(Pron[i]>=Real[i]) {
      Total<-Total+(Pron[i]-Real[i])
      SReal<-SReal+abs(Real[i])
    }
  }
  return(Total/SReal)
}
calibrar<-function(serie.aprendizaje,serie.testing, pot.incremento) {
  incremento<-10^{pot.incremento}
  error.c<-Inf
  alpha.i<-incremento  # alpha no puede ser cero
  while(alpha.i<=1) {
    beta.i<-0
    while(beta.i<=1) {
      gamma.i<-0
      while(gamma.i<=1) {
        mod.i<-HoltWinters(serie.aprendizaje,alpha=alpha.i,beta=beta.i,gamma=gamma.i)
        res.i<-predict(mod.i,n.ahead=length(serie.testing))
        error.i<-sqrt(ECM(res.i,serie.testing))
        if(error.i<error.c) {
          error.c<-error.i
          mod.c<-mod.i         
        }
        gamma.i<-gamma.i+incremento
      }
      beta.i<-beta.i+incremento
    }
    alpha.i<-alpha.i+incremento
  }  
  return(mod.c)
} 
suppressMessages(library(itsmr))
str(deaths)
Deaths<-ts(deaths, start=c(1973,1), freq=12)
Deaths.train<-Deaths[1:62]
Deaths.train<-ts(Deaths.train, start=c(1973,1), freq=12)
Deaths.test<-Deaths[63:72]
Deaths.test<-ts(Deaths.test, start=c(1978,3), freq=12)
modelo.HW<-calibrar(Deaths.train,Deaths.test, -1)
modelo.HW
suppressMessages(library(forecast))
auto.arima(Deaths.train)
modelo.A<-arima(Deaths.train,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12)) 
predict.HW<-predict(modelo.HW,n.ahead=10)
plot(predict.HW,xlim=c(1973,1979), ylim=c(6800, 11500), main = "Predicciones mediante modelo de Holt-Winters", col=2)
lines(Deaths.train,col=1)
predict.A<-predict(modelo.A,n.ahead=10)
plot(predict.A$pred,xlim=c(1973,1979), ylim=c(6800, 11500), main = "Predicciones mediante modelo SARIMA", col = 2)
lines(Deaths.train,col=1)
ECM.HW<-sqrt(ECM(predict.HW,Deaths.test))
ECM.A<-sqrt(ECM(predict.A$pred,Deaths.test))
Errores.ECM<-c(ECM.HW, ECM.A)
names(Errores.ECM)<-c("Holt-Winters", "SARIMA")
Errores.ECM
ER.HW<-ER(predict.HW,Deaths.test)
ER.A<-ER(predict.A$pred,Deaths.test)
Errores.R<-c(ER.HW, ER.A)
names(Errores.R)<-c("Holt-Winters", "SARIMA")
Errores.R

############## Problema 10 ########
#Hacer lo mismo sin componente estacional


######## #########

#omparar las series de tiempo por individuos
#si se parecen sus series de tiempo se parecen


#Las observaciones son un atributo, así que hacemos ACP
#podemos ver que días se parecen
#o que variables economicas se parecen




