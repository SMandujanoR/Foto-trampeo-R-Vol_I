base <- read.csv("BASE_ACTIVIDAD.csv", header = T)
head(base)

library(circular)

coy_circular <- circular(coyote$Hora_Decimal, units = 
                           "hour", template = "clock24", rotation = "clock",
                         zero = pi/2)
head(coy_circular, 5)

con_circular <- circular(conejo$Hora_Decimal, units = 
                           "hour", template = "clock24", rotation = "clock", 
                         zero = pi/2)
head(con_circular, 5)

#Fig. 10.3
par(mfrow = c(1,2), mar = c(1,1,1,1))
plot.circular(coy_circular, shrink = 1.2)
text(x = 1, y = 1, labels = "a")
plot.circular(coy_circular, shrink = 1.2, stack = T)
text(x = 1, y = 1, labels = "b")

#Fig. 4:
par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
#Figura 4a
rose.diag(coy_circular, bins = 24, ticks = T, 
          prop = 2, pch = 21, border = "blue", 
          col = "gray", shrink = 1.2) 
text(x = 1, y = 1, labels = "a")
rose.diag(coy_circular, bins = 24, ticks = T, 
          prop = 2, pch = 21, border = "blue", 
          col = "gray", shrink = 1.2) 
#Figura 4b
points.circular(coy_circular, stack = T)  
text(x = 1, y = 1, labels = "b")
plot(density.circular(coy_circular, bw=40), lwd=2, 
     lty=1, main = "", shrink = 1.2) 
#Figura 4c
text(x = 1, y = 1, labels = "c")
plot(density.circular(coy_circular, bw=40), 
     lwd=2, lty=1, main = "", shrink = 1.2) 
#Figura 4d
text(x = 1, y = 1, labels = "d")
points.circular(coy_circular, stack = T) 
rose.diag(coy_circular, bins = 24, ticks = T, 
          prop = 2, pch = 21, border = "blue", 
          col = "gray", add = T) 


mean.circular(coy_circular) 
median.circular(coy_circular) 

rho.circular(coy_circular)

angular.deviation(coy_circular) 

sd.circular(coy_circular) 

mle.vonmises.bootstrap.ci(coy_circular, 
                          alpha = 0.05, reps = 1000)


#Fig. 10.5
plot.circular(coy_circular, shrink = 1.2, stack = T)
#Graficando la media y longitud de la hora promedio
arrows.circular(mean.circular(coy_circular),
                rho.circular(coy_circular), x0 = 0, 
                col = "blue", lwd = 1.9, lty = 8) 
#Graficando la mediana
arrows.circular(median.circular(coy_circular), 
                x0 = 0, col = "red", 
                lwd = 1.9, lty = 6) 
#Obtención de los intervalos de confianza
coy_ci <- mle.vonmises.bootstrap.ci(coy_circular, 
                                    alpha = 0.05, reps = 1000)
#Graficando los intervalos de confianza
arrows.circular(coy_ci$mu.ci, x0 = 0, col = "black", 
                lwd = 1.9, lty = 6)


par(mfrow = c (1, 2))
unif <- rcircularuniform(100)
plot(unif, template = "clock24")
text(x = 1, y = 1, labels = "a")
von <- rvonmises(100, circular(1), 10)
plot(von, template = "clock24")
text(x = 1, y = 1, labels = "b")


options(scipen = 999)

rayleigh.test(coy_circular) 

rao.spacing.test(coy_circular, alpha = 0.05) 

kuiper.test(coy_circular, alpha = 0.05) 

watson.test(coy_circular, alpha = 0.05) 


watson.williams.test(list(coy_circular, con_circular), 
                     alpha = 0.05)


watson.two.test(coy_circular, con_circular, 
                alpha = 0.05) 

watson.wheeler.test(list(coy_circular, con_circular), 
                    alpha = 0.05) 


base_sd <- base[!duplicated(base$Hora_Decimal), ]


coyote_sd <- subset(base_sd, 
                    base$Especies == "Canis latrans")
conejo_sd <- subset(base_sd, 
                    base$Especies == "Sylvilagus floridanus")
coy_circular_sd <- circular(coyote_sd$Hora_Decimal, 
                            units = "hour", template = "clock24",
                            rotation = "clock", zero = pi/2)
con_circular_sd <- circular(conejo_sd$Hora_Decimal, 
                            units = "hour", template = "clock24",
                            rotation = "clock", zero = pi/2)


watson.wheeler.test(list(coy_circular_sd, 
                         con_circular_sd), alpha = 0.05)

coyote95 <- modal.region(coy_circular, q = 0.95, bw = 5)
coyote95$zeros

coyote50 <- modal.region(coy_circular, q = 0.50, bw = 5)
coyote50$zeros


par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(coyote95, main = "Intervalo de actividad (95%)", 
     ylab = "Densidad", xlab = "Horas", cex.main = 0.8)
plot(coyote50, main = "Intervalo de actividad 
     núcleo (50%)", 
     ylab = "Densidad", xlab = "Horas", cex.main = 0.8)


coy_cone95 <- totalvariation.circular(coy_circular, 
                                      con_circular, q = 0.95, bw = 5)
coy_cone50 <- totalvariation.circular(coy_circular, 
                                      con_circular, q = 0.50, bw = 5)












