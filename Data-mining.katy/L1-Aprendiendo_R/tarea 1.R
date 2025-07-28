# Probema5
matrix(1:6,nrow=2) 
matrix(1:6,nrow=4)
matrix(1:6,nrow=4,ncol=4)

#Problema 6
nombre <- c("Ana","Luis","Pedro","Juan","Eva","Jorge")
nombre
edad <- c(23,24,22,24,21,22)
edad
sex <- c("M","H","H","H","M","H")
sex
sexo <- as.factor(sex)
sexo
amigos <- data.frame(nombre,edad,sexo)
amigos

#Problema 7
windows(width = 7, height = 5)
par(mfrow=c(1,2))
x <- seq(0,2*pi,length=100)
plot(cos(x))
plot(x,cos(x),col="red")

#Problema 8
windows(width = 6, height = 7)
par(mfrow=c(2,2))
#(Gráfico 1)
x1 <- seq(0,2*pi, length = 100)

plot(x1, cos(x1), type = "l", col = "red", lwd = 2, xlab = "[0,2pi]", 
     ylab = "y1", cex.axis = 0.9, cex.lab = 0.9)
lines(x1, sin(x1), lty = 2, col = "green", lwd = 2)
title(main = "Seno y Coseno")
legend(0, -0.5, legend = c("Coseno","Seno"), col = c("red","green"), 
       lty = c(1,2), lwd = c(2,2), cex = 0.55, text.width = 0.7)
#(Gráfico 2)

plot(c(1,2,3,4,5), c(1,2,3,4,5), type = "n", xlab = "", ylab = "", 
     main = expression(italic("Título en cursiva")), cex.axis = 0.9)
lines(c(1,2,3,4,5), rep(1,5), col = "green", lwd = 5)
lines(rep(5,5), c(1,2,3,4,5), col = "green", lwd = 4)
lines(c(1,2,3,4,5), rep(5,5), col = "green", lwd = 4)
lines(rep(1,4), c(2,3,4,5), col = "green", lwd = 5)
lines(c(1,2,3,4), rep(2,4), col = "green", lwd = 5)
lines(rep(4,3), c(2,3,4), col = "green", lwd = 4)
lines(c(2,3,4), rep(4,3), col = "green", lwd = 4)
lines(c(2,2), c(3,4), col = "green", lwd = 5)
lines(c(2,3), c(3,3), col ="green", lwd = 4)
mtext("Eje x de color azul", 1, line = 3, col = "blue", cex = 0.7)
mtext("Eje y de color azul", 2, line = 3, col = "blue", cex = 0.7)
text(3, 1.5, expression(bold("Texto en negrita")), cex = 0.7)
#(Gráfico 3)
x2 <- seq(1, 10, length = 100)
plot(x2, log(x2), type = "l", col = "green", xlab = "Coordenada x", 
     ylab = "Coordenada y", main = "Función logaritmo", lwd = 3, cex.axis = 0.9,
     cex.lab = 0.7)
points(6, 1, pch = 17, col = "blue", cex = 1.2)
text(7.4, 1.01, "Punto (6,1)", cex = 0.7)
legend(7, 0.5, legend = "f(x)=log(x)", lty = 1, col = "green", lwd = 3, 
       cex = 0.55, text.width = 0.9)
#(Gráfico 4)
require(plotrix)
x3 <- seq(-4, 4, length = 3)
plot(x3, x3, type = "n", xlab = "", ylab = "", 
     main = "Circunferencias concéntricas", cex.lab = 0.7)
points(0, 0, pch = 16, col = "green")
draw.circle(0, 0, 1, nv = 100, border = "cyan", lty = 1, lwd = 2)
draw.circle(0, 0, 2, border = "yellow", lty = 2, lwd = 2)
draw.circle(0, 0, 3, border = "magenta", lty = 3, lwd = 1)
legend(2, -2.5, legend = c("Radio=1","Radio=2","Radio=3"),
       col = c("green","cyan","magenta"), lty = c(1,2,3), lwd = c(2,2,1),
       cex = 0.55, text.width = 0.8)
mtext("Eje x = [-4,4]", 1, line = 3, cex = 0.7)
mtext("Eje y = [-4,4]", 2, line = 3, cex = 0.7)




