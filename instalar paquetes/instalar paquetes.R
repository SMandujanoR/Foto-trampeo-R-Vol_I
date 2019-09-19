# Para instalar los paquetes que se emplean en este libro, se sugiere ejectuar este código:
  
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

# Alternativamente se pueden instalar uno por uno cada paquete desde RStudio/Packages/Install. Se sugiere instalar simultáneamente todas las dependencias asociadas a cada paquete.

##############################################
getwd()

library(knitr)
opts_knit$set(root.dir = "/Users/SMandujanoR/Documents/GitHub/x/Foto-trampeo-R-Vol_I")
