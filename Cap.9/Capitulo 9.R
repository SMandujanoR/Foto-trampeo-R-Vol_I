########################################################
# CAPÍTULO 9
# Análisis actividad y traslape: overlap
########################################################

setwd("Cap.9/")

library(overlap)

actividad_RBTC <- read.csv("tabla registros.csv", header = T)

summary(actividad_RBTC$Species)

hora_radianes_RBTC <- actividad_RBTC$Time_2 * 2 * pi

conejo_RBTC <- hora_radianes_RBTC[actividad_RBTC$Species == "Conejo"]

lince_RBTC <- hora_radianes_RBTC[actividad_RBTC$Species == "Lince"]

zorra_RBTC <- hora_radianes_RBTC[actividad_RBTC$Species == "Zorra"]

venado_RBTC <- hora_radianes_RBTC[actividad_RBTC$Species == "Venado"]

venado_RBTC_2012 <- hora_radianes_RBTC[actividad_RBTC$Species == "Venado" & actividad_RBTC == 2012]

venado_RBTC_2013 <- hora_radianes_RBTC[actividad_RBTC$Species == "Venado" & actividad_RBTC == 2013]

#fig. 9.3
par(mfcol = c(2,1), mar = c(3,3,1,1))
densityPlot(conejo_RBTC, xscale = 24, rug = T, extend = "lightgray", main = "Conejo", xlab = "Hora", ylab = "Densidad", bty = "L")

densityPlot(lince_RBTC, xscale = 24, rug = T, extend = "lightgray", main = "Lince", xlab = "Hora", ylab= "Densidad", bty = "L") 

#Todas las especies en la misma gráfica
par(mfcol = c(1,1), mar = c(5,5,5,5))
densityPlot(conejo_RBTC, xscale = 24, extend = NULL, main = " ", xlab = "Hora", ylab = "Densidad", bty = "L", col = "black", lty = 1)
densityPlot(lince_RBTC, add = TRUE, col = "royalblue4", lty = 2) 
densityPlot(zorra_RBTC, add = TRUE, col = "darkgreen", lty = 3) 
densityPlot(venado_RBTC, add = TRUE, col = "red2", lty = 4) 
legend("topright", inset= c(0,0), title = "Especies", c("Conejo", "Lince", "Zorra", "Venado"), lty = 1:4, col = c("black", "royalblue4", "darkgreen", "red2"), bty = "n")

min(length(conejo_RBTC), length(zorra_RBTC))

(conejo_zorra_delta <- overlapEst(conejo_RBTC, zorra_RBTC))

#Fig.9.4
overlapPlot(conejo_RBTC, zorra_RBTC, main = " ", linecol = c("black", "darkgreen"), lty = c(1,3), bty = "L", xlab = "Hora", ylab = "Densidad")
legend("topright", inset = c(0,0), title = "Especies", c("Conejo", "Zorra"), lty= c(1,3), col = c("gray25", "seagreen"), bty = "n")

conejo_remuestreo <- resample(conejo_RBTC, 1000)
zorra_remuestreo <- resample(zorra_RBTC, 1000)

conejo_zorra_delta_remuestreo <- bootEst(conejo_remuestreo, zorra_remuestreo, adjust = c(NA, 1, NA))

(conejo_zorra_delta_promedio <- colMeans(conejo_zorra_delta_remuestreo))

conejo_zorra_delta_remuestreo2 <- conejo_zorra_delta_remuestreo[, 2]
bootCI(conejo_zorra_delta [2], conejo_zorra_delta_remuestreo2)

########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 





