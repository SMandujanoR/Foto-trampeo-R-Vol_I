################################################################
# CAPÍTULO 13
# Simulación de movimientos y efectos en la detección: sim_JK
################################################################

# se lee el paquete:
source("Cap.13/sim_JW.R")

# se leen datos de la grilla de tramapas:
grid1 <- read.csv("Cap.13/grid1.csv", header = T)

grid2 <- read.csv("Cap.13/grid2.csv", header = T)

par(mfcol = c(1,1))
plot(grid1, xlim = c(19,71), ylim = c(19,71), frame.plot = F, main = "Retícula 1 espaciadas 100 m", col = "red", cex = 0.1)
plot(grid2, xlim = c(19,71), ylim = c(19,71), frame.plot = F, main = "Retícula 2 espaciadas 500 m", col = "red")

# Simulaciones variando la abundancia (N) y el home-range (HR)
N <- 10; HR <- 100; reticula <- grid1
muestreo(reticula); JW(x, y, HR)

N <- 1; HR <- 100; reticula <- grid1
muestreo(reticula); JW(x, y, HR)

N <- 10; HR <- 1000; reticula <- grid1
muestreo(reticula); JW(x, y, HR)

N <- 1; HR <- 1000; reticula <- grid1
muestreo(reticula); JW(x, y, HR)

N <- 10; HR <- 1000; reticula <- grid2
muestreo(reticula); JW(x, y, HR)

N <- 1; HR <- 100; reticula <- grid2
muestreo(reticula); JW(x, y, HR)

N <- 10; HR <- 100; reticula <- grid2
muestreo(reticula); JW(x, y, HR)

N <- 1; HR <- 1000; reticula <- grid2
muestreo(reticula); JW(x, y, HR)

N <- 10; HR <- 1000; reticula <- grid2
muestreo(reticula)
JW(x, y, HR)

########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 












