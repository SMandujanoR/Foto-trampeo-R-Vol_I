################################################################
# Mandujano, S. y L.A. Pérez-Solano. (Eds.). 2019. Fototrampeo en R: organización y análisis de datos. Volumen I. Instituto de Ecología A.C., Xalapa, Ver., México. 248 pp. ISBN: 978-607-7579-90-8
########################################################
# CAPÍTULO 6
# Inter / extrapolación de la diversidad: iNEXT
# Julio C. Hernández-Hernández
########################################################

library(iNEXT)
library(ggplot2)

#iNEXT(x, q, datatype, size, endpoint, knots, se, conf = 0.95, nboot = 50)

curva_abundancia <- read.table(file = "Cap.6/datos_abundancia.csv", header = TRUE, sep = ",")

curva_abundancia <- (curva_abundancia[,2:3])

t <- seq (1,600)

iNEXT(curva_abundancia, q = 0, datatype = "abundance", size = t)

curva_abundance <- iNEXT(curva_abundancia, q = 0, datatype = "abundance", size = t)

ggiNEXT(curva_abundance, type = 1, se = TRUE, color.var = "site",grey = FALSE) +  theme_classic(base_size = 12) +  theme(legend.position = "bottom", text = element_text(size = 12), legend.title = element_blank()) + labs (x = "Número de individuos", y = "Diversidad de especies")

ggiNEXT(curva_abundance, type = 2, se = TRUE, color.var = "site", grey = FALSE) + theme_classic(base_size = 12) + theme(legend.position = "bottom", text = element_text(size = 12), legend.title = element_blank()) +  labs (x = "Número de individuos", y = "Cobertura de la muestra")

#Grafico de interpolación (Fig. 6.3)
ggiNEXT(curva_abundance, type = 3, se = TRUE, color.var = "site", grey = FALSE) +  theme_classic(base_size = 12) + theme(legend.position = "bottom", text = element_text(size = 12),  legend.title = element_blank()) +  labs (x = "Cobertura de la muestra", y = "Diversidad de especies")

#Curva de incidencia
curva_incidencia <- read.table(file = "Cap.6/datos_incidencia.csv", header = TRUE, sep = ",") 

curva_incidencia <- (curva_incidencia[,2:3])
t <- seq (1,700)
iNEXT(curva_incidencia, q = 0, datatype = "incidence_freq", size = t)
curva_incidence <- iNEXT(curva_incidencia, q = 0, datatype = "incidence_freq", size = t)

#Figura 6.4
ggiNEXT(curva_incidence, type = 1, se = TRUE, color.var = "site", grey = FALSE) + theme_classic(base_size = 12) + theme(legend.position = "bottom", text = element_text(size = 12), legend.title = element_blank()) + labs (x = "Unidades de muestreo", y = "Diversidad de especies")

ggiNEXT(curva_incidence, type = 2, se = TRUE, color.var = "site", grey = FALSE) + theme_classic(base_size = 12) + theme(legend.position = "bottom", text = element_text(size = 12), legend.title = element_blank()) + labs (x = "Unidades de muestreo", y = "Diversidad de especies")

#Figura 6.5
ggiNEXT(curva_incidence, type = 3, se = TRUE, color.var = "site", grey = FALSE) + theme_classic(base_size = 12) + theme(legend.position = "bottom", text = element_text(size = 12), legend.title = element_blank()) + labs (x = "Unidades de muestreo", y = "Diversidad de especies")

#Figura 6.6
ggiNEXT(curva_incidence, type = 3, se = TRUE, color.var = "site", grey = FALSE) + theme_classic(base_size = 12) + theme(legend.position = "bottom", text = element_text(size = 12), legend.title = element_blank()) + labs (x = "Unidades de muestreo", y = "Diversidad de especies")

########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 




