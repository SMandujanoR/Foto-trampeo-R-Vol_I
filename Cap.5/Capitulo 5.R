################################################################
# Mandujano, S. y L.A. Pérez-Solano. (Eds.). 2019. Fototrampeo en R: organización y análisis de datos. Volumen I. Instituto de Ecología A.C., Xalapa, Ver., México. 248 pp. ISBN: 978-607-7579-90-8
########################################################
# CAPÍTULO 5
# Estimación de riqueza y diversidad: vegan
# Luz A. Pérez-Solano
########################################################

library(vegan)

diversidad <- read.csv("Cap.5/mamiferos.csv", row.names=1) #Las especies deben de estar acomodadas en las columnas y en las filas las cámaras-trampa (sitios)
View(diversidad)

# Calcular los índices para cada cámara:

(N0 <- rowSums(diversidad > 0)) 

(H <- diversity(diversidad, index="shannon"))

(J <- H/log(N0))

(simp <- diversity(diversidad, "simpson"))

(invsimp <- diversity(diversidad, "inv"))

(N1 <- exp(H))

# Estadísticos:
mean(N0)  #Riqueza específica (N0)
sd(N0)

mean(N1) # Serie de números de Hill
sd(N1)

#Creamos una tabla que agrupe los resultados (Fig. 5.2):
diversidad_mamiferos <- round(data.frame(N0, H, J, simp, invsimp, N1),2)
View(diversidad_mamiferos)

#Exportamos la tabla en formato .csv
write.csv (diversidad_mamiferos, "Cap.5/diversidad_mamiferos.csv")

########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 

