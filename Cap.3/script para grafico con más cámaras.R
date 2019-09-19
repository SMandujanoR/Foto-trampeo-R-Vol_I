
sitio_2 <- read.csv("Cap.3/actividad de camaras sitio_2.csv")

cam_activas_sitio_2 <- cameraOperation(CTtable = sitio_2,
                                       stationCol = "Station",
                                       setupCol = "Fecha_colo",
                                       retrievalCol = "Fecha_reti",
                                       hasProblems = T,
                                       dateFormat = "%d/%m/%Y")

camopPlot <- function(camOp){
  
  which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
  label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
  at.tmp <- which.tmp / ncol(camOp)
  
  image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = c("red", "grey70"))
  axis(1, at = at.tmp, labels = label.tmp)
  axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
  abline(v = at.tmp, col = rgb(0,0,0, 0.2))
  box()
}

jpeg(filename = "Figura_17.jpg", width = 2400, height = 1980, units = "px", res = 300)

camopPlot(camOp = cam_activas_sitio_2)

dev.off()
