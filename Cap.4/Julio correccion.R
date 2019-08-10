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

