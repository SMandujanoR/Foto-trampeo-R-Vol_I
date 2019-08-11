
#Instalar librería:
library(BiodiversityR)

#Cargar la matriz de datos:
mamiferos<-read.csv("mamiferos.csv", header=T, row.names=1)
View(mamiferos) #para visualizar el contenido. Cada columna son los registros independientes de 13 especies en 9 cámaras-trampa  

#Se generarán tres tipos de curvas de acumulación mediante la función `accumresult` 

# 1. Método `collector`
mamiferos1 <- accumresult(mamiferos, method="collector")
plot(mamiferos1, las= 1, col= "black") # Muestra el gráfico

# 2. Método `exact`
mamiferos2 <- accumresult(mamiferos, method="exact")
plot(mamiferos2, las= 1, col= "black")

# 3. Método `coleman`
mamiferos3 <- accumresult(mamiferos, method="coleman")
plot(mamiferos3, las= 1, col= "black")

#Es posible modificar el estilo en que se presentan los intervalos de confianza:
mamiferos_2 <- accumresult(mamiferos, method="exact") 
plot(mamiferos_2, las= 1, col= "black") 
plot(mamiferos_2, ci.type = "line", ci.lty = 2)
plot(mamiferos_2, ci.type = "poly", col= "blue", lwd=2, ci.lty = 0, ci.col="grey60")

# La edición y calidad de las gráficas puede mejorarse, y exportarse en formato jpg:

jpeg(filename = "Curvas_acumulacion.jpg", # asignar nombre al archivo jpg 
     width=8000, height=7000, units="px", res=1200)  # Asignar valores al archivo

#Gráfica con la edición deseada 

plot(mamiferos2, ci.type="polygon", ylim=c(0,15), xlim=c(1,9), 
     lwd=2, ci.lty=0, ci.col="gray80", last=1,frame.plot=F, 
     pch = 16, cex.lab = 1, cex.axis = 1,col="black", las=1,
     main = "Mamíferos", xlab = "No. de cámaras", ylab = "Especies")

dev.off()  #Para guardar



