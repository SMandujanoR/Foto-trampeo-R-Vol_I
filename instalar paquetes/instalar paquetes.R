################################################################
# Mandujano, S. y L.A. Pérez-Solano. (Eds.). 2019. Fototrampeo en R: organización y análisis de datos. Volumen I. Instituto de Ecología A.C., Xalapa, Ver., México. 248 pp. ISBN: 978-607-7579-90-8
################################################################
# INSTALACIÓN DE PAQUETES
################################################################
getwd()

# Se puede instalar uno por uno cada paquete desde RStudio/Packages/Install. Se sugiere instalar simultáneamente todas las dependencias asociadas a cada paquete.

# Alternativamente, para instalar los paquetes que se emplean en este libro, se sugiere ejectuar este código:
  
FototrampeoR_instalar <- function() {
  paquetes <- c("imager", "vegan", "unmarked", "secr", 
                "agricolae", "wiqid", "SPACECAP", "jagsUI", "iNEXT", 
                "BiodiversityR", "overlap", "circular", "camtrapR", 
                "rgdal", "ggplot2", "PerformanceAnalytics", 
                "RColorBrewer", "MASS", "MuMin",   "bibtex", "dplyr",
                "ade4", "adehabitatHR", "adehabitatLT", "agricolae", 
                "akima", "boot", "chron", "dunn.test", "ggExtra", 
                "ggplot2", "gtools", "jagsUI", "KernSmooth", 
                "maptools", "MASS", "mgcv", "overlap", "plotrix", 
                "raster", "RColorBrewer", "reshape", "rgdal", 
                "rgeos", "rmarkdown", "scrbook", "secr", "sp", 
                "survival", "survminer", "tree", "unmarked", 
                "vegan", "wiqid", "maps", "GIStools", "WDI", 
                "Scales", "shapefiles", "RgoogleMaps", "tidyverse",
                "data.table", "reshape2", "effects")
    
  pkgs_miss <- paquetes[!(paquetes %in% 
                          installed.packages())]
      
  if(length(pkgs_miss) > 0L)
  install.packages(pkgs_miss, repos = "https://cloud.r-project.org/", dependencies = TRUE)
}


########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 

