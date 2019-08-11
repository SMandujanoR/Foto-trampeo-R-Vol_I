######################################################
# Estimación de la abundancia relativa fototrampeo
# Aplicación del paquete RAI
# Salvador Mandujano Rodríguez
Sys.Date()
######################################################

source("nuevo.R")

(fotos <- read.csv("datos.csv", header = T))

RAIgral(fotos) # calcula RAI modelo clásico

RAIalt(fotos, 10) # calcula RAI modelo alternativo

Dist_spp(fotos, c("Odo_vir", "Uro_cin", "Can_lat", "Pro_lot", "Nas_nar")) # grafica el número fotos por cámara para especies

RAIglm(fotos) # RAI modelo GLM tipo Poisson

RAIglmCov("Can_lat") # RAI modelo GLM tipo Poisson con covariables

#RAIinterp(fotos) # gráfico de interpolación entre occupación naive vs RAI

#RAIfinal(fotos) # genera tabla de resultados finales

# ------------------
# FIN SCRIPT

rm(list = ls()) # elimina datos
dev.off() # elimina graficos

