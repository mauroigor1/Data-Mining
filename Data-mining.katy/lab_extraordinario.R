############ Laboratorio Extraordinario ###############
#### Problema 1 #####


#Fijar directorio 
setwd("C:/Users/alumno/Desktop")

#Cargar los datos
controldata <- read.table("synthetic_control.data.txt", dec = ".")
dim(controldata)

### Parte 1 ### 
#Transformamos el vector en serie de tiempo

controldata.ts <- ts(t(controldata[600,]), frequency = 21)
str(controldata.ts)
controldata.ts
plot(controldata.ts)
hist(diff(controldata.ts),prob=T,col="red")
desccontroldata.ts <- stl(controldata.ts[,1], s.window = 7)
head(desccontroldata.ts)
plot(desccontroldata.ts)

#Grafico de autocorrelacion simple y parcial
acf(controldata.ts, main = "Autocorrelación simple", col = "black")
pacf(controldata.ts, main = "Autocorrelación parcial", col = "black")


### Parte 2 ###
controldata2 <- t(controldata)
write.table("controldata2.txt", dec =".")

