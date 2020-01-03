################################################################
# Mandujano, S. y L.A. Pérez-Solano. (Eds.). 2019. Fototrampeo en R: organización y análisis de datos. Volumen I. Instituto de Ecología A.C., Xalapa, Ver., México. 248 pp. ISBN: 978-607-7579-90-8
########################################################
# CAPÍTULO 7
# Curvas de acumulación: BiodiversityR
# Luz A. Pérez-Solano
########################################################

library(BiodiversityR)

mamiferos <- read.csv("Cap.7/mamiferos.csv", header = T, row.names = 1)
names(mamiferos)
View(mamiferos)

mamiferos1 <- accumresult(mamiferos, method = "collector")
mamiferos2 <- accumresult(mamiferos, method = "exact")
mamiferos3 <- accumresult(mamiferos, method = "coleman")

par(mfcol = c(2,2))
plot(mamiferos1, las = 1, lwd = 2, col = "red", frame.plot = F, main = "a) Collector")
plot(mamiferos2, las = 1, lwd = 2, col = "red", frame.plot = F, main = "b) Exact")
plot(mamiferos3, las = 1, lwd = 2, col = "red", frame.plot = F, main = "c) Coleman")

mamiferos_2 <- accumresult(mamiferos, method = "exact")
plot(mamiferos2, ci.type = "polygon", ylim = c(0,15),  xlim = c(1,9), lwd = 2, ci.lty = 0, ci.col = "gray80", last = 1, frame.plot = F, pch = 16, cex.lab = 1, cex.axis = 1, col = "red", las = 1, main = "Mamíferos", xlab = "No. de cámaras", ylab = "Especies")

#Si se quiere exportar la figura:
#jpeg(filename = "Cap.7/Fig7_3.jpg", width = 8000,  height = 7000, units = "px", res = 1200)
#dev.off()

########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 
