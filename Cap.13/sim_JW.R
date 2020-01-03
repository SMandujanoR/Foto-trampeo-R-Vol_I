################################################################
# Mandujano, S. y L.A. Pérez-Solano. (Eds.). 2019. Fototrampeo en R: organización y análisis de datos. Volumen I. Instituto de Ecología A.C., Xalapa, Ver., México. 248 pp. ISBN: 978-607-7579-90-8
################################################################
# Funciones para el capítulo
################################################################

JW <- function(x, y, HR) {
  for (j in 1:N) {
    x <- runif(1,1,99)
    y <- runif(1,1,99)
    points(x,y,pch=16,col="black",cex=1.2)
    for (i in 1:HR) {
      xi <- sample(c(1,0,-1),1) 
      yi <- sample(c(1,0,-1),1) 
      points(c(x, x + xi), c(y, y + yi), pch = 16, 
             cex = 0.3, col = "blue") 
      x <- x + xi
      y <- y + yi
      if (x > 100 | x < 0 | y > 100 | y < 0) break  
    }
  } 
}

muestreo <- function(reticula) {
  par(mfrow = c(1,1), mar = c(3,5,3,5))
  plot(0:100, 0:100, type = "n", 
       xlab = "UTM", ylab = "UTM") 
  points(reticula, pch = 3, col = "red", cex = 0.1) 
}
