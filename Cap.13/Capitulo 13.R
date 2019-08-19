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


grid1 <- read.csv("grid1.csv", header = T)
grid2 <- read.csv("grid2.csv", header = T)

par(mfcol = c(2,1))
plot(grid1, xlim = c(19,71), ylim = c(19,71), frame.plot = F, main = "Retícula 1 espaciadas 100 m", col = "red", cex = 0.1)
plot(grid2, xlim = c(19,71), ylim = c(19,71), frame.plot = F, main = "Retícula 2 espaciadas 500 m", col = "red")


N <- 10; HR <- 100; reticula <- grid1
muestreo(reticula); JW(x, y, HR)


N <- 1; HR <- 100; reticula <- grid1
muestreo(reticula); JW(x, y, HR)

N <- 10; HR <- 100; reticula <- grid1
muestreo(reticula); JW(x, y, HR)

N <- 1; HR <- 1000; reticula <- grid1
muestreo(reticula); JW(x, y, HR)

N <- 10; HR <- 1000; reticula <- grid1
muestreo(reticula); JW(x, y, HR)

N <- 1; HR <- 100; reticula <- grid2
muestreo(reticula); JW(x, y, HR)

N <- 10; HR <- 100; reticula <- grid2
muestreo(reticula); JW(x, y, HR)

N <- 1; HR <- 1000; reticula <- grid2
muestreo(reticula); JW(x, y, HR)

N <- 10; HR <- 1000; reticula <- grid2
muestreo(reticula); JW(x, y, HR)














