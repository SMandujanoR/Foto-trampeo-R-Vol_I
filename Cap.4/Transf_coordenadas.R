################################################################
# Mandujano, S. y L.A. Pérez-Solano. (Eds.). 2019. Fototrampeo en R: organización y análisis de datos. Volumen I. Instituto de Ecología A.C., Xalapa, Ver., México. 248 pp. ISBN: 978-607-7579-90-8
################################################################
# UTILIDAD PARA TRANSFORMAR COORDENADAS
################################################################

library(rgdal)

datos <- read.csv("camarasRBTC.csv", header = T)

# se prepara matriz con datos:
utmcoor <- SpatialPoints(cbind(datos$X, datos$Y), 
                         proj4string=CRS("+proj=utm +zone=14"))

# se transforman:
longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))

coordenadas <- as.data.frame(longlatcoor)
coordenadas <- data.frame(coordenadas$coords.x2, 
                          coordenadas$coords.x1)
colnames(coordenadas) <- c("Latitude", "Longitude")
write.csv(coordenadas, "coordenadas_camaras.csv")

