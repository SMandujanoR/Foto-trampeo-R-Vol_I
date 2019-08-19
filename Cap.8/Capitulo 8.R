source("pkgRAI_1.R")

wildlife.data <- read.csv("mamiferos.csv", header = T)

habitat.data <- read.csv("habitat.csv", header = T)

new.mat <- merge(x = wildlife.data, y = habitat.data, by = "Camera", all = TRUE)

Tot_cameras <- with(new.mat, length(unique(Camera))) 
cameras <- with(new.mat, tapply(Camera, Species, length)) 
days <- with(new.mat, tapply(Effort, Species, sum))   
n <- with(new.mat, tapply(Events, Species, sum))   

occ <- subset(new.mat, new.mat$Events > 0)
OccNaive <- as.data.frame(round(table(occ$Species)/cameras,2))

RAIgral <- round(n/days*100, 2) 
table.1 <- cbind(cameras, days, n, RAIgral, OccNaive = OccNaive[,2])
table.1 <- table.1[order(RAIgral),]


especie <- c("Uro_cin", "Can_lat") 
#Fig. 8.2
par(mfcol = c(1,2)) 
for (i in 1:length(especie)) {
  sp  <- subset(new.mat, Species == especie[i])
  plot(sp$X, sp$Y, xlab="UTM", ylab="UTM", frame.plot = F, cex.axis = 0.5,  main = unique(sp$Species), type = "n", labels = T, ylim = c(min(sp$Y - 1000), max(sp$Y + 1000)))
  points(sp$X, sp$Y, pch = 16, col = "skyblue", cex = 4)
  text(sp$X, sp$Y, sp$Events) 
}


RAIalt <- with(new.mat, round((Events/Effort)*100, 2))
table.2 <- cbind(new.mat, RAIalt)
#Fig. 8.3
par(mfcol = c(1,1), mar = c(5,5,1,1))
boxplot(RAIalt ~ Species, data = new.mat, ylab = "RAI", varwidth = F, outline = F, cex = 3.4, las = 2, frame.plot = FALSE, cex.axis = .7, col = "skyblue")


RAIaov <- aov(RAIalt ~ Species-1, data = new.mat)
(mod <- summary(RAIaov))


library(agricolae)
out_1 <- HSD.test(RAIaov, "Species")
out2 <- out_1$groups
par(mfcol = c(1,1), mar = c(5,5,1,1)) #Fig. 8.4
bar.group(out_1$groups , horiz = F, las = 2, cex.main = 2, font = 3, cex.axis = 0.9, plot = T, col = "lightblue", ylim = c(0, max(RAIgral + 2)), names.arg = out_1$trt, ylab = "RAI")


RAImean <- with(new.mat, round(tapply(RAIalt, Species, mean), 2))
RAIsd <- with(new.mat, round(tapply(RAIalt, Species, sd), 2))
occ <- subset(table.2, table.2$Events > 0)
OccNaive <- round(table(occ$Species)/cameras,2)

table.3 <- cbind(cameras, days, n, RAIgral, RAImean, RAIsd, OccNaive)
table.3 <- table.3[order(RAIgral),]











