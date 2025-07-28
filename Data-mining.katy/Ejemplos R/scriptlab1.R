#Páctica 1, Data Mining

###Primera sesión### (10/03/2017)
rnorm(5) # 5 números aleatorios de una normal (mean= 0, sd = 1)
## "#" indica el principio de un comentario
 ## Los números se calculan y se muestran (print)

# asignamos unos números a un objeto (un vector) x
summary(x) ## muestra un resumen de x (un summary "inteligente")
 ## o también:
w <- summary(x)
w
print(w) # escribir w y print(w) produce el mismo resultado
## summary(x) TAMBIÉN es un objeto.

###Algunos cálculos elementales###
2+2
sqrt(10)
2*3*4*5
# Intereses sobre 1000 euros
1000*(1+0.075)^5 - 1000
# R conoce pi
pi
# Circunferencia de la Tierra en el Ecuador en km
2*pi*6378
# Convertimos ángulos a radianes y luego calculamos el seno
sin(c(0,30,45,60,90)*pi/180)

###Ayuda###
?sin
?if #No funciona 
help("if")

###Instalar paquetes###
install.packages("nnet", dependencies=TRUE) #dependencies =TRUE instala todas 
#las dependencias de ese paquete, todo lo que necesita este paquete 
#para funcionar
library(nnet) #carga el paquete en la sesión

###Ejemplo muestra exponencial###
alpha <- 0.01; n <- 50; m <- 1000
> datos <- matrix(rexp(n * m, alpha), ncol=n)
> fz <- function(x) n*min(x)
> z <- apply(datos,1,fz) #aplica fz por filas o columnas a la matriz datos
> mean(z) # debe ser 1/alpha=100
> hist(z,freq=F)
> points(dexp(0:600,alpha),type="l")
> ks.test(z,"pexp",alpha)

###Visualizar y grabar objetos###
ls() #describe en la consola todos los objetos de trabajo
save(datos,file="nombre.RData") #con RStudio es directo guardar la sesión
attach("misdatos.RData") #carga la sesión guardada en el fichero en la actual
ls(pos=2) # segunda posición en la ''search list''

###Tipos de objetos###
x <- c(1, 2, 3); y <- c("a", "b", "c") #vector
z <- c(TRUE, TRUE, FALSE)

dades <- data.frame(ID=c("gen0", "genB", "genZ"),
                     subj1 = c(10, 25, 33), subj2 = c(NA, 34, 15),
                     oncogen = c(TRUE, TRUE, FALSE),
                     loc = c(1,30, 125))
#cuando las columnas son de distinta naturaleza

xx <- c(1, 2, 2, 1, 1, 2, 1, 2, 1)
x <- factor(c(1, 2, 2, 1, 1, 2, 1, 2, 1))
#referencia a categorías en función de la información

una.lista <- c(un.vector = 1:10,
               una.palabra = "hola",
               una.matriz = matrix(rnorm(20), ncol = 5),
               lista2 = c(a = 5,
                            b = factor(c("a", "b"))))
#puede ser lo que sea

###Atributos de objetos###
x <- 1:15; length(x)
y <- matrix(5, nrow = 3, ncol = 4); dim(y)
is.vector(x); is.vector(y); is.array(x) #pregunta por el tipo de variable
x1 <- 1:5; x2 <- c(1, 2, 3, 4, 5); x3 <- "patata"
typeof(x1); typeof(x2); typeof(x3) #entrega el tipo de variable
mode(x); mode(y); z <- c(TRUE, FALSE); mode(z)
attributes(y)
w <- list(a = 1:3, b = 5); attributes(w)
y <- as.data.frame(y); attributes(y)
f1 <- function(x) {return(2 * x)}
attributes(f1); is.function(f1)

###Operaciones matemáticas###
data(presidents)
help(presidents)
range(presidents, na.rm = TRUE)
which.min(presidents)# 28
which.max(presidents)# 2

###Operadores lógicos###
x <- 5; x < 5; x >= 5; x == 6; x != 5
y <- c(TRUE, FALSE); !y; z <- c(TRUE, TRUE)
xor(y, z)
y & z; y | z
0 + y; as.numeric(y)#cambia "y"a valores numéricos
mode(y) <- "numeric"

###Ejemplo###
peso <- c(19,14,15,17,20,23,30,19,25)
peso < 20
peso < 20 | peso > 25
peso[peso<20]
wich(peso<20) #"wich" indica las componentes con esa condición
peso[peso<20 & peso!=15]
trat <- c(rep("A",3),rep("B",3),rep("C",3)) #"rep" repite algo un número de veces
peso[trat=="A"]
peso[trat=="A"|trat=="B"]
split(peso,trat) #'split' es un date.frame y separa los pesos con respecto a trat
#sirve para categorizar información
split(peso,trat)$A

###Discretizar datos###
split(peso,trat)
split(peso,trat)$A

vv <- rnorm(100)
cut1 <- cut(vv, 5) #???
table(cut1)
cut2 <- cut(vv, quantile(vv, c(0, 1/4, 1/2, 3/4, 1)))
summary(cut2)
class(cut2)

###Operaciones con conjuntos###
x <- 1:5; y <- c(1, 3, 7:10) #concatena estos dos 'vectores'
union(x, y) # une (y borra los repetidos)
intersect(x, y)
setdiff(y, x)
v <- c("bcA1", "bcA2", "blX1")
w <- c("bcA2", "xA3")
union(v, w)
intersect(v, w)
setdiff(w, v)
setdiff(v, w)

###Generación de secuencias###
x <- c(1, 2, 3, 4, 5)
x <- 1:10; y <- -5:3
1:4+1; 1:(4+1)
x <- seq(from = 2, to = 18, by = 2) #'seq' secuencia de números equidastentes
x <- seq(from = 2, to = 18, length = 30)
y <- seq(along = x) #???
z2 <- c(1:5, 7:10, seq(from=-7,to=5,by=2))

rep(1, 5)
x <- 1:3; rep(x, 2)
y <- rep(5, 3); rep(x, y)
rep(1:3, rep(5, 3))
rep(x, x)
rep(x, length = 8)
gl(3, 5) # como rep(1:3, rep(5, 3))
gl(4, 1, length = 20) # !Alerta! gl genera factores ?????
gl(3, 4, label = c("Sano", "Enfermo", "Muerto"))
expand.grid(edad = c(10, 18, 25),sexo = c("Macho", "Hembra"), loc = 1:3)
#Esto es útil para llenar matrices con estructuras especiales

###Secuencias aleatorias###
sample(5)
sample(5, 3) #aleatoria del 1 al 5, con muestra 3
x <- 1:10
sample(x)
sample(x, replace = TRUE) #muestra con reemplazo
sample(x, 2* length(x), replace = TRUE)
probs <- x/sum(x)
sample(x, prob = probs)
#Números aleatorios rDistribución(n,parámetros)
rnorm(10) # rnorm(10, mean = 0,sd = 1)
runif(8,min=2,max=10)

###Selección elementos de un vector###
x <- 1:5; x[1]; x[3]; x[c(1,3)]
x[x > 3] 
x > 3
y <- x > 3
x[y]
x[-c(1, 4)] #retira elementos del vector x
y <- c(1, 2, 5); x[y]
names(x) <- c("a", "b", "c", "d", "papa") #poner nombre a las columnas
x[c("b", "papa")] #extraerlas por su nombre

###Valores perdidos###
#NA es el c´odigo de "Not available".
v <- c(1,6,9,NA) #NA es sólo una etiqueta, no existe
is.na(v); which(is.na(v))
w <- v[!is.na(v)] # sin los valores perdidos
v == NA # !No funciona! ¿Por qué?

#Sustituir NA por, p.ej., 0:
v[is.na(v)] <- 0

#El infinito y NaN ("not a number") son diferentes de NA.
5/0; -5/0; 0/0
is.infinite(-5/0); is.nan(0/0); is.na(5/0)

#Con algunas funciones
xna <- c(1, 2, 3, NA, 4); mean(xna) #??????
mean(xna, na.rm = TRUE)

###Omitir valores perdidos###
XNA <- matrix(c(1,2,NA,3,NA,4), nrow = 3)
XNA
X.no.na <- na.omit(XNA)

###Ordenación vectores###
x1 <- c(5, 1, 8, 3)
order(x1) #ranquea en orden decreciente
sort(x1) #lor ordena de forma creciente
rev(x1) #vector reverso
rank(x1) #ranquea en orden creciente
x1[order(x1)] #??
x2 <- c(1, 2, 2, 3, 3, 4); rank(x2)
min(x1); which.min(x1); which(x1 == min(x1)) #los dos últimos hacen lo mismo
y <- c(1, 1, 2, 2); order(y, x1)

###Vectores de caracteres###
codigos <- paste(c("A", "B"), 2:3, sep = "")
codigos <- paste(c("A", "B"), 2:3, sep = ".")
juntar <-paste(c("una", "frase", "simple"), collapse =" ")
columna.a <- LETTERS[1:5]; columna.b <- 10:15
juntar <- paste(columna.a, columna.b, sep ="")
substr("abcdef",2,4)
x <- paste(LETTERS[1:5], collapse="")
substr(x, 3, 5) <- c("uv")

###Factores###
#son variables categóricas y no numéricas aunque sean números, no se puede operar con ella
codigo.postal <- c(28430, 28016, 28034);
mode(codigo.postal)
codigo.postal <- factor(codigo.postal) # mejor
y <- rnorm(10); x <- rep(letters[1:5], 2)
aov(y ~ x) # !error!
aov(y ~ factor(x)) # funciona

x <- c(34, 89, 1000); y <- factor(x); y
as.numeric(y) # mal
# los valores han sido recodificados
as.numeric(as.character(y)) # bien

ftr1 <- factor(c("alto", "bajo", "medio"))
ftr1
ftr1 <- factor(c("alto", "bajo", "medio"),
                 levels = c("bajo", "medio", "alto"))

###Arreglos y matrices###
a <- 1:24; dim(a) <- c(3,4,2)
a1 <- array(9, dim = c(5,4))
a2 <- matrix(1:20, nrow = 5)# como en FORTRAN #siempre llena en columnas
a3 <- matrix(1:20, nrow = 5, byrow = TRUE) #ahora llena en filas
a4 <- 1:20; dim(a4) <- c(5, 4)
a[1,1,1]; a[1,1,2]; a[3,4,2]
a[2, , ] # es un array de dimensión c(4,2)
a4[1, ]; a4[, 2]; a4[c(1, 3), c(2, 4)] #????
im <- matrix(c(1, 3, 2, 4), nrow = 2)
im
a4[im]

#Ejemplo 1# (15/0372017)
x <- c(190,8,22,191,4,1.7,223,80,2,210,50,3) #vector
datos <- matrix(x,nrow=4,byrow=T); dim(datos) #matriz con 4 filas c
ciudades <- c("Barna","Tarraco","Lleida","Gi") #vector con nombre de ciudades
dimnames(datos) <- list(ciudades,NULL) #filas nombre de ciudades, columnas nada
variables <- c("A","B","C")
dimnames(datos) <- list(ciudades,variables) #filas nombre ciudades, columnas variables
datos
dimnames(datos)???
datos["Barna", ]
datos[ ,"C"]

#Ejemplo 2# #??
a4 <- 1:20; dim(a4) <- c(5, 4)
attributes(a4)
colnames(a4) <- paste("v", 1:4, sep = "")
rownames(a4) <- paste("id", 1:5, sep = ".")
a4[, c("v1", "v3")]
attributes(a4)

matriz <- matrix(rnorm(20),ncol=4)
o.matriz <- matriz[order(matriz[, 1]), ] #order, ordena ascendente con la etiqueta de la
#fila
#o.matriz permuta la fila entera ordenando la primera columna

###Operaciones matriciales###
data(longley) #cargamos datos que ya existen en R
Cl <- cor(longley)
## Representación gráfica de la correlación:
symnum(Cl) # Altamente correlacionadas
## rho de Spearman 
cor(apply(longley, 2, rank))
cor(longley, method = "spearman") # mejor alternativa

##función outer
x <- 1:9; names(x) <- x
x %o% x
y <- 2:8; names(y) <- paste(y,":",sep="")
outer(y, x, "^") #crea tablas con la operación indicada

###Combinación de arreglos###
x1 <- 1:10; x2 <- 11:20
a6 <- diag(6) # matriz identidad
a7 <- cbind(x1, x2); # despliega x1 y x2 como columnas
a8 <- rbind(x1, x2); #despliega x1 y x2 como filas
a24 <- cbind(a2, a4) #pega a2 y a4 por columnas
cbind(a4, a6) # no funciona #dimensiones no calzan
rbind(a4, a6) # no funciona #idem
a9 <- matrix(rnorm(30), nrow = 5)
cbind(a4, a9)
rbind(a4, a9) # no funciona

###Data Frames###
##Sin data frame
x3 <- letters[1:10] #vector con las primeras 10 letras
a9 <- cbind(x1, x2, x3)
##Con data frame
a10 <- data.frame(x1, x2, x3) #no puedo hacer matriz porque todo debe ser numérico
#dataframe considera todo como caracteres
prcomp(a10[, c(1,2)])# componentes principales
prcomp(a10[, c("x1", "x2")])
prcomp(a10[, -3]) #todas las columnas menos la 3, con -c(3,5) quito más de una columna

playa <- c("si","si","no","no")
datos.df <- data.frame(datos,playa)
datos.df$playa

set.seed(1) # fija la semilla del generador de números aleatorios
#fijar la semilla es como fijar los datos "aleatorios"
d1 <- data.frame(g1 = runif(10), g2 = rnorm(10))
d1$edad <- c(rep(20, 5), rep(40, 5))
set.seed(1)
d2 <- cbind(g1 = runif(10), g2 = rnorm(10))
d2[, 3] <- c(rep(20, 5), rep(40, 5)) # error
d2 <- cbind(d2, edad = c(rep(20, 5), rep(40, 5)))

attributes(a10) # cuando no están definidos

###Función apply###
ax <- matrix(rnorm(20), ncol = 5)
medias.por.fila <- apply(ax, 1, mean) #mean calcula el promedio, en este caso por fila
por.si.na <- apply(ax, 1, mean, na.rm = TRUE)
mi.f1 <- function(x) { return(2*x - 25)} #paréntesis es la definición de la función
#return sirve para devolver el valor de la función
#R por defecto no entrega valores
mi.f1.por.fila <- apply(ax, 1, mi.f1) #aplica la función f1, por fila a x
mas.simple <- apply(ax, 1, function(x){return(2*x -25)})
medias.por.columna <- apply(ax, 2, mean)#aquí lo calcula por columna
sample.rows <- apply(ax, 1, sample) #sample muestrea y entrega el resultado por columnas
dos.cosas <- function(y){return(c(mean(y), var(y)))}
apply(ax, 1, dos.cosas)
t(apply(ax, 1, dos.cosas))

##sapply, lapply
parameters <- cbind(mean = -5:5, sd = 2:12)
z.data <- matrix(rnorm(1000 * 11), nrow = 11)
data <- (z.data * parameters[,2]) + parameters[,1]
apply(data, 1, mean); apply(data, 1, sd)

data(airquality) #carga una base de datos de R
sapply(airquality, function(x)sum(is.na(x))) #no es necesario especificar fila o columna

x <- c(19,14,15,17,20,23,19,19,21,18)
trat <- c(rep("A",5),rep("B",5))
x.media <- tapply(x,trat,mean) #va a etiquetar x de acuerdo a trat y calcula la media
x.media
##Algunas funciones que internamente utilizan apply
x1 <- 1:10
m1 <- matrix(1:20, ncol = 5)
d1 <- as.data.frame(m1)
mean(x1); mean(d1); sd(x1); sd(d1); median(m1); median(d1)

###Tablas###
table(sexo,nivel) #permite crear tablas de contingencia
resultado <- cbind(expand.grid(calif=c("mejor","peor","igual"),tratam=c("A","B")))
frec <- c(21,34,5,7,12,14)
tabla <- table(resultado)*frec
tabla

##Ejemplo##
d3 <- data.frame(g1=runif(10),g2=rnorm(10),
                 id1 = c(rep("a", 3), rep("b", 2),
                         rep("c", 2), rep("d", 3)))
my.fun <- function(x) {
    las.medias <- mean(x[, 1:2]) #??
    las.vars <- var(x[, -3])
    max.total <- max(x[, -3])
    tabla.clases <- table(x[, 3])
    return(list(row.means = las.medias,
          row.vars = las.vars, maximum = max.total,
          factor.classes = tabla.clases))
  }
my.fun(d3)

###Muchos objetos - Listas###
una.lista <- my.fun(d3); una.lista
attributes(una.lista); names(una.lista)
length(una.lista)
una.lista[[4]] #aparecen los valores del objeto de la tabla
una.lista[4] # aparece sólo el objeto de esa table
una.lista$factor.classes
una.lista[[3]] <- list(NULL); una.lista
una.lista[[3]] <- NULL
una.lista # hemos eliminado el "slot" maximum
unlist(una.lista)
otra.lista <- list(cucu = 25, una.lista)
unlist(otra.lista)
unlist(otra.lista)
una.lista <- c(una.lista, otro.elemento = "una frase")

###Objetos disponibles###
ls() #muestra los objetos disponibles
objects() #muestra lo mismo
objects(pattern="a*") #objetos que tengan "a"

search() #mostrar los paquetes que están disponible
library(MASS) #cargamos la librería
search() #antes no aparecía, ahora aparece MASS

str(datos.df) # es un data.frame, y describe sus elementos
A # error
attach(datos.df) #atach toma las variables de este arreglo, y la vuelve global el nombre
A # ahora sí
plot(A,B) # en lugar de plot(datos.df$A,datos.df$B)

datos.df$D <- 1:4 # una nueva columna
datos.df # aquí está
D # pero aquí no

#Para desconectar
detach(datos.df)

#Para borrar objetos concretos
rm(datos.df)

#Para borrar todos los objetos del entorno de trabajo:
rm(list = ls())

load("nombre.RData")

##Cuidado!!##
alpha <- 0.01; n <- 50; m <- 1000
datos <- matrix(rexp(n * m, alpha), ncol=n)
playa <- c("si","si","no","no")
datos.df <- data.frame(datos,playa)
attach(datos.df)
playa <- 1
playa # usa la última
search() # el search path
detach(datos.df)
attach(datos.df)
D
A # cuidado

###Guardar y leer datos###
x <- runif(20)
y <- list(a = 1, b = TRUE, c = "patata")
save(x, y, file = "xy.RData") ##Guarda
load("xy.RData") ##lee

##Guardar todos los objetos disponibles
save.image() # guardado como ".RData"
save.image(file = "objetosdisponibles.RData")
rm(list = ls())
load("objetosdisponibles.RData")

##Datos disponibles en paquetes
data() # muestra todos los archivos
data(iris)
data(iris, package = "base") # equivalente
?iris

###Exportar datos###
#setwd fija donde guardad los datos
#importante revisar bien qué es lo que hace cada uno
write(t(x), file = "c:/dir/data.txt",
        ncolumns = n,
        append = FALSE)#como matriz

write.table(my.data.frame,
              file = "mi.output.txt",
              sep = "",row.names = FALSE,
              col.names = TRUE) #Como data frame

write.table(x, file = "foo.csv", sep = ",",
              + col.names = NA)#importable dsede Excel

###plot###
x <- runif(50, 0, 4); y <- runif(50, 0, 4)
plot(x, y, main = "Título principal",
       sub = "Subtítulo", xlab = "eje x", ylab = "eje y",
       xlim = c(-5,5),ylim = c (-5,5))

##Variantes
z <- cbind(x,y)
plot(z)
plot(y ~ x)
plot(log(y + 1) ~ x) # transformaci´on de y
plot(x, y, type = "p") #puntos
plot(x, y, type = "l") #líneas
plot(x, y, type = "b") #puntos y líneas
plot(c(1,5), c(1,5))
legend(1, 4, c("uno", "dos", "tres"), lty = 1:3,
         col = c("red", "blue", "green"),
         pch = 15:17, cex = 2)

#Caracteres de texto
sexo <- c(rep("v", 20), rep("m", 30))
plot(x, y, type = "n")
text(x, y, labels = sexo)

##Tipos de puntos y líneas
points(x, y, pch = 3, col = "red") #pch = point character

plot(c(1, 10), c(1, 3), type = "n", axes = FALSE,
       xlab = "", ylab="") #no tiene ejes ni etiquetas
points(1:10, rep(1, 10), pch = 1:10, cex = 2, col = "blue") #cex= tamaño
points(1:10, rep(2, 10), pch = 11:20, cex = 2, col = "red")
points(1:10, rep(3, 10), pch = 21:30, cex = 2,
         col = "blue", bg = "yellow") #col=color borde, bg=relleno

plot(c(0, 10), c(0, 10), type = "n", xlab ="",
       ylab ="")
for(i in 1:10) #??
  abline(0, i/5, lty = i, lwd = 2) #lty=tipo de línea, lwd=ancho 

###Identificación interactiva de datos###
x <- 1:10
y <- sample(1:10)
nombres <- paste("punto", x, ".", y, sep ="")
plot(x, y)
identify(x, y, labels = nombres)

plot(x, y)
locator() #dice las coordenadas
text(locator(1), "el marcado", adj = 0)

###Múltiples gráficos por ventana###
par(mfrow = c(2, 2)) #para plotear varios gráficos
plot(rnorm(10))
plot(runif(5), rnorm(5))
plot(runif(10))
plot(rnorm(10), rnorm(10))

###Datos Multivariados###
X <- matrix(rnorm(1000), ncol = 5)
colnames(X) <- c("a", "id", "edad", "loc",
                   "weight")
pairs(X) #entrega todos los gráficos posibles

##con coplot
Y <- as.data.frame(X) #? as?
Y$sexo <- as.factor(c(rep("Macho", 80),
                        rep("Hembra", 120)))
coplot(weight ~ edad | sexo, data = Y) #filtra por sexo
coplot(weight ~ edad | loc, data = Y) #filtra por loc?
coplot(weight ~ edad | loc * sexo, data = Y) #filtra por loc y sexo

###Boxplot### ?????
attach(Y)
boxplot(Y$weight)
plot(Y$sexo, Y$weight)
detach()

boxplot(weight ~ sexo, data = Y,
          col = c("red", "blue"))

##Ruido
dc1 <- sample(1:5, 500, replace = TRUE)
dc2 <- dc1 + sample(-2:2, 500, replace = TRUE,
                      prob = c(1, 2, 3, 2, 1)/9)
plot(dc1, dc2)
plot(jitter(dc1), jitter(dc2)) #jitter genera ruido

##Añadir rectas
x <- rnorm(50)
y <- rnorm(50)
plot(x, y)
lines(lowess(x, y), lty = 2) #lines agrega línea
plot(x, y)
abline(lm(y ~ x), lty = 3) #???

###Guardar gráficos###
pdf(file = "f1.pdf", width = 8, height = 10) #le damos las dimensiones
plot(rnorm(10))
dev.off()
#también se puede hacer desde plot

##copiar a un fichero
plot(runif(50))
dev.copy2eps()

###Funciones###
##Ejemplo
suma <- function(a1,d,n){
  an <- a1+(n-1)*d;
  ((a1+an)*n)/2}

###Argumentos###
una.f <- function(a,b,c = 4,d = FALSE){x1<-a*2
return(x1)}
una.f(4, 5)
una.f(b = 5, a = 4)

##PAsando argumentos
f3 <- function(x, y, label = "la x", ...){
  plot(x, y, xlab = label, ...)}
f3(1:5, 1:5) #da los argumentos a f x=1:5 e y=1:5
f3(1:5, 1:5, col = "red") #color lo manda a ... = etc
#esto sirve para dar flexibilidad al gráfico

##Usando outer
f <- function(x,y){cos(y)/(x^2-3)}
z <- outer(x,y,f) #para aplicar función a vectores

###Condiciones lógicas###
f4 <- function(x) {
  if(x > 5) print("x > 5")
  else {
   y <- runif(1)
   print(paste("y is ", y)) #pega la palabra con y
   } #print imprime
  }

odd.even <- function(x) {
  ifelse(x %% 2 == 1, "Odd", "Even") #analizar sintáxis
  } #odd=impar, even=par

f5 <- function(x){
  ifelse(x > 5, "x>5", paste("y is", runif(1)))
}

mtf <- matrix(c(TRUE, FALSE, TRUE, TRUE),
                nrow = 2)
ifelse(mtf, 0, 1) #cambia por 0 los verdaderos y por 1 los falsos

##For
for(i in 1:10) {cat("el valor de i es", i, "\n") #\n es un separador
continue.loop <- TRUE
x <- 0
while(continue.loop) {
  x <- x + 1
  print(x)
  if( x > 10) continue.loop <- FALSE
  }
}
#recomendación, siempre poner mensajes para conocer el estado