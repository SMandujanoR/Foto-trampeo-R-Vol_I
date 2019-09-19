########################################################
# CAPÍTULO 8
# Índice de abundancia relativa: RAI
########################################################

# Cargar el paquete: 
source("Cap.8/pkgRAI_1.R")

# Leer datos
wildlife.data <- read.csv("Cap.8/mamiferos.csv", header = T)
View(wildlife.data)

habitat.data <- read.csv("Cap.8/habitat.csv", header = T)
View(habitat.data)

# Poner aquí las especies para graficar fotos por cámara
especie <- c("Odo_vir", "Uro_cin", "Can_lat", "Lyn_ruf") 

# Ejecutar cálculos
RAI() # única función para generar todo el análisis

########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 

