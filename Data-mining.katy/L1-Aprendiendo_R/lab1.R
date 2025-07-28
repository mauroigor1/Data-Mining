
############################ Ejercicio 9
#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("gridExtra")
library(ggplot2) #carga los paquetes de la librería ggplot2
library(reshape) #carga los paquetes de la librería reshape

head(tips) ##muestra las primeras filas del data frame tips
dim(tips)  ##muestra las dimensiones del data frame  tips 
           ##en un vector de la forma (filas columnas)

##grafica tip (propina) vs total_bill (cuenta total) usando puntos
qplot(tip,total_bill,geom="point", data=tips)

##grafica tip vs total_bill usando puntos 
##y diferenciando si smoker= yes o smoker=no (si es fumador o no)
qplot(tip,total_bill,geom="point", data=tips, colour=smoker) 

##misma gráfica, pero cambiando nombre del eje x e y, además de agregar un título
qplot(tip,total_bill,geom="point", data=tips, colour=smoker, 
      xlab="Tip (in dollars)", ylab="Total Bill (in dollars)",
      main="Scatterplot of Tip by Total Bill,
      Colored by Smoking Status")

##crea un vector con el cuociente entre tip y total_bill en cada componente   
##y lo agrega como una nueva columna, de título rate, a tips 
##(a qué porcentaje de la cuenta equivale la propina)
tips$rate <- tips$tip/tips$total_bill 

##hace un histograma con los "rates" (los agrupa por diferencias de 0.02)
qplot(rate,geom="histogram", data=tips) 

##hace un histograma con los "rates", pero agrupándolos por diferencias de 0.05
qplot(rate,geom="histogram", data=tips, binwidth=.05) 

##lo mismo anterior, pero con bordes negros y color interior celeste
ggplot(tips, aes(x=rate)) + geom_histogram(binwidth=.05,
                                           colour="black", fill="lightblue") 

##Hace un diagrama de cajas sobre los "rates", separando por sexo
qplot(sex, rate, geom="boxplot", data=tips,fill = sex) 

##Mismo diagrama de cajas, agregando título, cambiando nombre al eje x y al eje y, 
##y agregando puntos que muestran cada "rate".
qplot(sex, rate, geom="boxplot", data=tips,
      xlab="Gender", ylab="Tipping Rate",
      main="Boxplots of tipping rate by gender",fill = sex) + geom_jitter() 


############################ Ejercicio 10
DJ<-read.csv("DJTable.csv",header=TRUE,sep=";",dec=".",fill=TRUE)
DJ2<-DJ
for (i in 1:dim(DJ)[1]){
  DJ2[i,]=DJ[dim(DJ)[1]+1-i,] #Para reordenar los datos, ya que parte desde el final
}
DJ<-DJ2
plot(c(0,380), c(0,150), type = "n", axes = TRUE,
     xlab = "Tiempo", ylab="Valores de las acciones",main="Valores de las acciones en el año 2010")
lines(1:dim(DJ)[1], DJ[,"CSCO"], col = "blue",lwd = 1)
lines(1:dim(DJ)[1], DJ[,"IBM"], col = "red",lwd = 1)
lines(1:dim(DJ)[1], DJ[,"INTC"], col = "green",lwd = 1)
lines(1:dim(DJ)[1], DJ[,"MSFT"], col = "black",lwd = 1)
legend("bottomright", c("CSCO", "IBM", "INTC","MSFT"), col = c("blue", "red", "green","black"),lwd=1)
############################ Ejercicio 11

ggplot(DJ, aes(x=Tiempo,y=Valores_de_las_acciones))+
  ggtitle("Valores de las acciones en el año 2010")+
  geom_line(data=DJ,aes(1:dim(DJ)[1], DJ[,"CSCO"], col = "CSCO"),lwd = 1)+
  geom_line(data=DJ,aes(1:dim(DJ)[1], DJ[,"IBM"], col = "IBM"),lwd = 1)+
  geom_line(data=DJ,aes(1:dim(DJ)[1], DJ[,"INTC"], col = "INTC"),lwd = 1)+
  geom_line(data=DJ,aes(1:dim(DJ)[1], DJ[,"MSFT"], col = "MSFT"),lwd = 1)+
  scale_colour_manual(values = c("CSCO"="blue", "IBM"="red", "INTC"="green", "MSFT"="black"))


############################ Ejercicio 12
SA<-read.csv("SAheart.csv",header=TRUE,sep=";",dec=".",fill=TRUE)
library(gridExtra)

SA$tipo <- "joven"

for (i in 1:dim(SA)[1]){
if (SA$age[i]<22){
SA$tipo[i] <- "joven"
} 
else if (50>SA$age[i]&SA$age[i]>21){
  SA$tipo[i] <- "adulto"
} 
else if (SA$age[i]>49){
  SA$tipo[i] <- "mayor"
  }
}
#qplot(tipo, typea, geom="boxplot",colour=famhist, data=SA,xlab="Edad", ylab="Tipo A",
  #    main="Diagrama de Caja relacionando el Tipo A 
  #    con la presencia de enfermedad Cardiaca
  #    en diferentes edades",fill = chd)+
  #scale_fill_manual(values=c("yellow", "red"))+
  #labs(colour="Historial familiar",fill="Enfermedad cardiaca")

#qplot(tipo, sbp, geom="boxplot", data=SA,fill = chd,colour=famhist)+
 # scale_fill_manual(values=c("yellow", "red"))
qplot(tipo, ldl, geom="boxplot", data=SA,xlab="Edad", ylab="Colesterol LDL",
      main="Diagrama de Caja relacionando el colesterol LDL
      con la presencia de enfermedad Cardiaca
      en diferentes edades",fill = chd,colour=famhist)+
  scale_fill_manual(values=c("yellow", "red"))+
  labs(colour="Historial familiar",fill="Enfermedad cardiaca")

q1<-ggplot(SA, aes(x=sbp,fill=chd)) + geom_histogram(binwidth=10, position="dodge")+
  labs(x = "Presión Sanguínea Sistólica",fill="Enfermedad
       cardiaca")
q2<-ggplot(SA, aes(x=typea,fill=chd)) + geom_histogram(binwidth=5, position="dodge")+
  labs(x = "Tipo A",fill="Enfermedad
       cardiaca")
q3<-ggplot(SA, aes(x=ldl,fill=chd)) + geom_histogram(binwidth=1, position="dodge")+
  labs(x = "Colesterol LDL",fill="Enfermedad
       cardiaca")
q4<-ggplot(SA, aes(x=adiposity,fill=chd)) + geom_histogram(binwidth=5, position="dodge")+
  labs(x = "Adiposidad",fill="Enfermedad
       cardiaca")
grid.arrange(q1,q2,q3,q4, ncol=2, nrow =2)
#ggplot(SA, aes(x=obesity,fill=chd)) + geom_histogram(binwidth=5, position="dodge")
#ggplot(SA, aes(x=tobacco,fill=chd)) + geom_histogram(binwidth=2, position="dodge")
#ggplot(SA, aes(x=alcohol,fill=chd)) + geom_histogram(binwidth=5, position="dodge")
#qplot(adiposity,sbp,geom="point", data=SA, colour=chd)


SA1<-SA[which(SA$famhist=="Absent"),]
SA2<-SA[which(SA$famhist=="Present"),]
p1<-qplot(obesity,adiposity,xlim = c(0,50),ylim = c (0,50),
          xlab="Obesidad", ylab="Adiposidad",main="Sin enfermedades cardiacas
          en su historial familiar",
          geom="point", data=SA1, colour=chd)+theme(legend.position="bottom")+
  labs(colour="Enfermedad cardiaca")
p2<-qplot(obesity,adiposity,xlim = c(0,50),ylim = c (0,50),
          xlab="Obesidad", ylab="Adiposidad",main="Con enfermedades cardiacas
          en su historial familiar",
          geom="point", data=SA2, colour=chd)+theme(legend.position="bottom")+
  labs(colour="Enfermedad cardiaca")
grid.arrange(p1,p2, ncol=2, nrow =1)
