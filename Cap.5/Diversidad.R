
library(vegan)

diversidad <- read.csv("mamiferos.csv", row.names=1) #Las especies deben de estar acomodadas en las columnas y en las filas las cámaras-trampa (sitios)
View(diversidad)

# Calcular los índices:

N0 <- rowSums(diversidad >0) 
N0

H<-diversity(diversidad, index="shannon") 
H

J <- H/log(N0)    
J

simp <- diversity(diversidad, "simpson") 
simp

invsimp <- diversity(diversidad, "inv") 
invsimp

N1 <- exp(H)  
N1

# Estadísticos

mean(N0)  #Riqueza específica (N0)
sd(N0)

mean(N1) # Serie de números de Hill
sd(N1)

#Creamos una tabla que agrupe los resultados (Fig. 5.2):
diversidad_mamiferos <- round(data.frame(N0, H, J, simp, invsimp, N1),2)
View(diversidad_mamiferos)

#Exportamos la tabla en formato .csv
write.csv (diversidad_mamiferos, "diversidad_mamiferos.csv")

