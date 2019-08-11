#########################################
# Paquete RAI 
# Elaborado por: Salvador Mandujano Rodríguez
# Última versión: Octubre 7, 2018
#########################################

# Modelo RAI general 
RAIgral <- function(new.mat) {
  #new.mat <- merge(x = wildlife.data, y = habitat.data, by = "Camera", all = TRUE)
  Tot_cameras <- with(new.mat, length(unique(Camera))) 
  cameras <- with(new.mat, tapply(Camera, Species, length)) 
  days <- with(new.mat, tapply(Effort, Species, sum))   
  n <- with(new.mat, tapply(Events, Species, sum))   
  
  occ <- subset(new.mat, new.mat$Events > 0)
  OccNaive <- as.data.frame(round(table(occ$Species)/cameras,2))
  
  RAIgral <- round(n/days*100, 2) 
  
  table1 <- cbind(cameras, days, n, RAIgral, OccNaive = OccNaive[,2])
  table1 <- table1[order(RAIgral),]
  View(table1)
  write.csv(table1, "Resultados/table1.csv") 
  
  #jpeg(filename= "Resultados/Dist_Abun.jpg", width= 7000, height= 5000, units= "px", res=1200)
  plot(OccNaive[,2], RAIgral, xlab= "Distribución (OccNaive)", ylab = "Abundancia (RAI)", frame.plot = F, las = 1, pch = 16, col = "skyblue", cex = 2)
  abline(lm(RAIgral ~ OccNaive[,2]), col = "red")
  r2 <- round(cor(RAIgral, OccNaive[,2]),2)
  text(0.9, 2, as.expression(substitute(r == r2, list(r2 = r2))))
 # dev.off()
}

# ------------------------------------------
# Distribución de especies
Dist_spp <- function(new.mat, especie) {
  library(stringr)
  par(mfcol = c(round(length(especie)/2,0), round(length(especie)/2,0)))
  for (i in 1:length(especie)) {
    sp  <- subset(new.mat, Species == especie[i])
    #jpeg(filename= "Resultados/Dist_Spp.jpg", width= 7000, height= 5000, units= "px", res=1200)
    plot(sp$X, sp$Y, xlab="UTM", ylab="UTM", frame.plot = T, cex.axis = 1,  main = unique(sp$Species), type = "n", labels = T) 
    points(sp$X, sp$Y, pch = 16, col = "skyblue", cex = 4)
    text(sp$X, sp$Y, sp$Events) 
    #dev.off()
  }
}

# ------------------------------------------
# Modelo RAI alternativo
RAIalt <- function(new.mat, max) {
  RAIalt <- with(new.mat, round((Events/Effort)*100, 2))
  table2 <- cbind(new.mat, RAIalt)
  View(table2)
  write.csv(table2, "Resultados/table2.csv") 
  par(mfcol = c(1,1), mar = c(5,5,1,1))
  #jpeg(filename= "Resultados/RAIalt.jpg", width= 7000, height= 5000, units= "px", res=1200)
  boxplot(RAIalt ~ Species, data = new.mat, ylab = "RAI", varwidth = F, outline = F, cex = 3.4, las = 2, frame.plot = FALSE, cex.axis = .7, col = "skyblue")
 # dev.off()

# Comparación estadística
  RAIaov <- aov(RAIalt ~ Species-1, data = table2)
  cat("----- \n RAI comparación entre especies \n")
  print(summary(RAIaov))
  
  # Dependiendo valor de P se hacen o no pruebas posteriori 
  P <- as.numeric(unlist(summary.aov(RAIaov)[[1]][5]))[1]

ifelse(P < 0.05, {
    # prueba Tukey
  par(mfcol = c(1,1))
 # jpeg(filename= "Resultados/Tukey.jpg", width= 7000, height= 10000, units= "px", res=1200)
  plot(TukeyHSD(RAIaov), cex.axis = 0.5, las = 1)
  #dev.off()
  
    # prueba HSD
  library(agricolae)
  out_1 <- HSD.test(RAIaov, "Species")
  out2 <- out_1$groups
  par(mfcol = c(1,1))
  #jpeg(filename= "Resultados/HSD.jpg", width= 7000, height= 5000, units= "px", res=1200)
  bar.group(out_1$groups , horiz = F, las = 2, cex.main = 2, font = 3, cex.axis = 0.9, plot = T, col = "lightblue", ylim = c(0, max), names.arg = out_1$trt, ylab = "RAI")
  #dev.off()
    }, NA) # si no es significativo, no ejecuta pruebas posterior
}

# GLM Poisson con "offsets"
RAIglm <- function(new.mat) {
  RAIglm <- glm(Events ~ Species-1, data = new.mat, offset = log(Effort), family = quasipoisson)
  cat("----- \n GLM regresión Poisson: \n")
  print(summary(RAIglm))
  cat("----- \n Estimación por especie: \n")
  RAIpoisson <- cbind(RAI = round(exp(RAIglm$coefficients)*100, 2))
  print(RAIpoisson)
}

# GLM Poisson con covariables
RAIglmCov <- function(especie) {
  
  # seleccionar especie:
  new.mat <- read.csv("table2.csv", header = T)
  especie_sel <- new.mat[new.mat$Species== especie,]
  especie_sel <- especie_sel[-10:-13,]
  
  # Selección de covariables
  RAIglm2 <- glm(Events ~ Veg_type-1, data = especie_sel, offset = log(Effort), family = quasipoisson) 
  summary(RAIglm2)
  cat("----- \n estimación RAI a partir de un GLM regresión Poisson con covariables: \n")
  print(summary(RAIglm2))
  RAIglm2 <- exp(RAIglm2$coefficients)*100
  print(RAIglm2)
  
  par(mfcol = c(1,1), mar = c(5,5,1,1))
  plot(RAIalt ~ Veg_type, data = especie_sel, frame.plot = F, varwidth = T, col = "skyblue", main = unique(especie_sel$Species))
  
  RAIglm3 <- glm(Events ~ scrub + grass, data = especie_sel, offset = log(Effort), family = quasipoisson) 
  summary(RAIglm3)
  par(mfcol = c(1,2), mar = c(5,5,1,1))
  plot(RAIalt ~ scrub, data = especie_sel, frame.plot = F, col = "skyblue", las = 1, pch = 16, cex = 2)
  plot(RAIalt ~ grass, data = especie_sel, frame.plot = F, col = "skyblue", las = 1, pch = 16, cex = 2) 

  RAIglm4 <- glm(Events ~ Road_dist + Loc_dist, data = especie_sel, offset = log(Effort), family = quasipoisson) 
  summary(RAIglm4)
  par(mfcol = c(1,2), mar = c(5,5,1,1))
  plot(RAIalt ~ Road_dist, data = especie_sel, frame.plot = F, col = "skyblue", las = 1, pch = 16, cex = 2)
  plot(RAIalt ~ Loc_dist, data = especie_sel, frame.plot = F, col = "skyblue", las = 1, pch = 16, cex = 2)
}

# RAI correlaciones
RAIcorr <- function(){
  RAImean <- round(tapply(RAIalt, Species, mean), 2)
  RAIsd <- round(tapply(RAIalt, Species, sd), 2)
  r1 <- round(cor(RAIgral, RAImean),2)
  
  par(mfcol = c(1,2), mar = c(5,5,5,1))
  plot(RAIgral, RAImean, frame.plot = F, las = 1, pch = 16, col = "skyblue", ylab = "RAI alternativo", xlab = "RAI general", cex = 2, cex.main = 0.7)
  abline(lm(RAImean ~ RAIgral), col = "red")
  text(2,7, as.expression(substitute(r == r1, list(r1 = r1))))
  
  plot(RAImean, as.numeric(OccNaive), ylab= "Occupation naive", xlab = "RAI alternativo", frame.plot = F, las = 1, pch = 16, col = "skyblue", cex = 2)
  abline(lm(OccNaive ~ RAImean), col = "red")
  text(2,0.9, as.expression(substitute(r == r2, list(r2 = r2))))
}

RAIinterp <- function(new.mat) {
  library(akima)
  library (MASS)
  library(RColorBrewer)
  par(mfrow = c(1,1))
  g <- interp(OccNaive, RAImean,n, duplicate = T)
  image(g, col= topo.colors(12), cex.lab = 1.0, frame = F, ylab= "Abundancia (RAI)", xlab = "Distribución (OccNaive)", ylim = c(0, max(RAImean + 2)), xlim = c(0, 1.5))
  contour(g, add = T)
  text(OccNaive, jitter(RAImean,3), labels = unique(sort(new.mat$Species)), cex = 1, pos = 4, col = "red")
}

# Tabla final de resultados
RAIfinal <- function(new.mat) {
  t1 <- read.csv("table.1", header = T)
  t2 <- read.csv("table.2", header = T)
  attach(t1)
  attach(t2)
  RAImean <-  with(t2, round(tapply(RAIalt, Species, mean), 2))
  RAIsd <- with(t2, round(tapply(RAIalt, Species, sd), 2))
  
  table.3 <- cbind(cameras, days, n, RAIgral, RAImean, RAIsd,  OccNaive)
  #table.3 <- table.3[order(RAIgral),]
  #View(table.3)
  #save(table.3)
}
