###########################################
# Paquete RAI: versión varias funciones
# Salvador Mandujano
# Abril 17, 2018
##########################################

new.mat <- merge(x = wildlife.data, y = habitat.data, by = "Camera", all = TRUE)
attach(new.mat)

# RAI general model

RAIgral <- function() {
  Tot_cameras <- with(new.mat, length(unique(Camera))) 
  cameras <- with(new.mat, tapply(Camera, Species, length)) 
  days <- with(new.mat, tapply(Effort, Species, sum))   
  n <- with(new.mat, tapply(Events, Species, sum))   
  
  RAIgral <- round(n/days*100, 2) 
  table.1 <- cbind(cameras, days, n, RAIgral)
  table.1 <- table.1[order(RAIgral),]
  View(table.1)
}

# RAI alternative model
RAIalt <- function() {
  RAIalt <- with(new.mat, round((Events/Effort)*100, 2))
  table.2 <- cbind(new.mat, RAIalt)
  View(table.2)
  attach(table.2)
  
  par(mfcol = c(1,1), mar = c(5,5,1,1))
  boxplot(RAIalt ~ Species, data = new.mat, ylab = "RAI", varwidth = TRUE, outline = F, cex = 3.4, las = 2, frame.plot = FALSE, cex.axis = .7, col = rainbow(12))
}

# ---------------------------------
# Distribution of selected species  

Dist_spp <- function() {
  par(mfrow=c(2,2), mar= c(5,5,5,5))
  for (i in 1:length(especie)) {
    sp  <- subset(new.mat, Species == especie[i])
    plot(sp$X, sp$Y, xlab="UTM", ylab="UTM", frame.plot = F, cex.axis = 0.8, las= 1, main = unique(sp$Species), type = "n")
    points(sp$X, sp$Y, pch = 16, col = "lightblue", cex = 4)
    text(sp$X, sp$Y, sp$Events)
  }
}
  
# RAI statistical comparision
RAIaov <- function() {
  RAIaov <- aov(RAIalt ~ Species-1)
  cat("---------------------------------- \n RAI comparison among species \n")
  print(summary(RAIaov))
  }

# Post hoc Tukey test
RAItukey <- function() {
  par(mfcol = c(1,1), mar = c(2,9,0,1))
  plot(TukeyHSD(RAIaov), cex.axis = 0.5, las = 1)
  cat("---------------------------------- \n")
  print(with(new.mat, pairwise.t.test(RAIalt, Species, p.adjust.method = "none")))
  }

# Post hoc HSD test
RAIhsd <- function() {
  library(agricolae)
  out_1 <- HSD.test(RAIaov, "Species")
  out2 <- out_1$groups
  par(mfcol = c(1,1), mar = c(5,5,1,1))
  bar.group(out_1$groups , horiz = F, las = 2, cex.main = 2, font = 3, cex.axis = 0.9, plot = T, col = "lightblue", ylim = c(0, max(RAIgral + 2)), names.arg = out_1$trt, ylab = "RAI")
    }

# Poisson GLM with offsets
RAIglm <- function() {
  RAIglm <- glm(Events ~ Species-1, offset = log(Effort), family = quasipoisson)
  cat("---------------------------------- \n RAI estimation from GLM Poisson regression: \n")
  print(summary(RAIglm))
  par(mfcol = c(2,2))
  plot(RAIglm)
  RAIpoisson <- round(exp(RAIglm$coefficients)*100, 4) 
  print(RAIpoisson)
}

# Poisson GLM covariables
RAIglmCov <- function() {
  RAIglmCov <- glm(Events ~ Species-1 + Loc_dist + Veg_type, offset = log(Effort), family = quasipoisson)
  cat("---------------------------------- \n RAI estimation from GLM Poisson regression with habitat covariables: \n")
  print(summary(RAIglmCov))
  RAIpoisson2 <- round(exp(RAIglmCov$coefficients), 4)
  print(RAIpoisson2)
}

# RAI correlations
RAIcorr <- function(){
  RAImean <- round(tapply(RAIalt, Species, mean), 2)
  RAIsd <- round(tapply(RAIalt, Species, sd), 2)
  occ <- subset(table.2, table.2$Events > 0)
  OccNaive <- round(table(occ$Species)/cameras,2)
  
  par(mfcol = c(1,2), mar = c(5,5,5,1))
  plot(RAIgral, RAImean, frame.plot = F, las = 1, pch = 16, col = "black", ylab = "RAI mean", xlab = "RAI general", cex = 2)
  abline(lm(RAImean ~ RAIgral), col = "red")
  cor(RAIgral, RAImean)
  
  plot(RAImean, as.numeric(OccNaive), ylab= "Occupation naive", xlab = "RAI mean", frame.plot = F, las = 1, pch = 16, col = "black", cex = 2)
  abline(lm(OccNaive ~ RAImean), col = "red")
  cor(RAIgral, OccNaive)
}

RAIinterp <- function() {
  library(akima)
  library (MASS)
  library(RColorBrewer)
  par(mfrow = c(1,1))
  g <- interp(RAImean, OccNaive, n, duplicate = T)
  image(g, col= topo.colors(12), cex.lab = 1.0, frame = F, xlab= "RAI mean", ylab = "Occupation naive", xlim = c(0, max(RAImean + 2)), ylim = c(0, 1.1))
  contour(g, add = T)
  text(RAImean, jitter(OccNaive,3), labels = unique(sort(new.mat$Species)), cex = 1, pos = 4, col = "red")
}

# Final results
RAIfinal <- function() {
  table.3 <- cbind(cameras, days, n, RAIgral, RAImean, RAIsd, RAIpoisson, OccNaive)
  table.3 <- table.3[order(RAIgral),]
  View(table.3)
  }

# -----------------------------
# FIN DEL PAQUETE
