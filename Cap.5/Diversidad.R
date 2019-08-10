
#Cargar la librería
library(vegan)

#Cargar la matriz de datos 

diversidad <- read.csv("mamiferos.csv", row.names=1) #Las especies deben de estar acomodadas en las columnas y en las filas las cámaras-trampa (sitios)
View(diversidad)

# Calcular los índices:

N0 <- rowSums(diversidad >0) # Riqueza específica (N0) 
N0

H<-diversity(diversidad, index="shannon") # Índice de Shannon-Wiener
H

J <- H/log(N0)     # Equidad de Pielou
J

simp <- diversity(diversidad, "simpson") #Índice de Simpson
simp

invsimp <- diversity(diversidad, "inv") # Inverso de Simpson
invsimp

N1 <- exp(H)   # Serie de números de Hill
N1

# Promedio y desviación estándar de la riqueza específica y la serie de números de Hill para todas las cámaras-trampa

mean(N0)  #Riqueza específica (N0)
sd(N0)

mean(N1) # Serie de números de Hill
sd(N1)

#Creamos una tabla que agrupe los resultados:
diversidad_mamiferos <- round(data.frame(N0, H, J, simp, invsimp, N1),2)
View(diversidad_mamiferos)

#Exportamos la tabla en formato .csv
write.csv (diversidad_mamiferos, "diversidad_mamiferos.csv")

