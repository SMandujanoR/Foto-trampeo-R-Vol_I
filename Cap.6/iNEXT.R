library(iNEXT)
library(ggplot2)

iNEXT(x, q, datatype, size, endpoint, knots, se, conf = 0.95, nboot = 50)


curva_abundancia <- read.table(file = "datos_abundancia.csv", header = TRUE, sep = ",")

curva_abundancia <- (curva_abundancia[,2:3])

t <- seq (1,600)

iNEXT(curva_abundancia, q = 0, datatype = "abundance", size = t)

curva_abundance <- iNEXT(curva_abundancia, q = 0, datatype = "abundance", size = t)

ggiNEXT(curva_abundance, type = 1, se = TRUE, color.var = "site",grey = FALSE) +  theme_classic(base_size = 12) +  theme(legend.position = "bottom", text = element_text(size = 12), legend.title = element_blank()) + labs (x = "Número de individuos", y = "Diversidad de especies")


ggiNEXT(curva_abundance, type = 2, se = TRUE, color.var = "site", grey = FALSE) + theme_classic(base_size = 12) + theme(legend.position = "bottom", text = element_text(size = 12), legend.title = element_blank()) +  labs (x = "Número de individuos", y = "Cobertura de la muestra")

#Grafico de interpolación (Fig. 6.3)
ggiNEXT(curva_abundance, type = 3, se = TRUE, color.var = "site", grey = FALSE) +  theme_classic(base_size = 12) + theme(legend.position = "bottom", text = element_text(size = 12),  legend.title = element_blank()) +  labs (x = "Cobertura de la muestra", y = "Diversidad de especies")

#Curva de incidencia
curva_incidencia <- read.table(file = "datos_incidencia.csv", header = TRUE, sep = ",") base_2 <- read.csv(file = "base_2.csv", header = TRUE)



curva_incidencia <- (curva_incidencia[,2:3])
t <- seq (1,700)
iNEXT(curva_incidencia, q = 0, 
      datatype = "incidence_freq", size = t)
curva_incidence <- iNEXT(curva_incidencia, q = 0, 
                         datatype = "incidence_freq", size = t)

#Figura 6.4
ggiNEXT(curva_incidence, type = 1, se = TRUE, 
        color.var = "site", grey = FALSE) + 
  theme_classic(base_size = 12) + 
  theme(legend.position = "bottom", 
        text = element_text(size = 12), 
        legend.title = element_blank()) + 
  labs (x = "Unidades de muestreo", 
        y = "Diversidad de especies")

ggiNEXT(curva_incidence, type = 2, se = TRUE, 
        color.var = "site", grey = FALSE) + 
  theme_classic(base_size = 12) + 
  theme(legend.position = "bottom", 
        text = element_text(size = 12), 
        legend.title = element_blank()) + 
  labs (x = "Unidades de muestreo", 
        y = "Diversidad de especies")

#Figura 6.5
ggiNEXT(curva_incidence, type = 3, se = TRUE, 
        color.var = "site", grey = FALSE) + 
  theme_classic(base_size = 12) + 
  theme(legend.position = "bottom", 
        text = element_text(size = 12), 
        legend.title = element_blank()) + 
  labs (x = "Unidades de muestreo", 
        y = "Diversidad de especies")

#Figura 6.6
ggiNEXT(curva_incidence, type = 3, se = TRUE, 
        color.var = "site", grey = FALSE) + 
  theme_classic(base_size = 12) + 
  theme(legend.position = "bottom", 
        text = element_text(size = 12), 
        legend.title = element_blank()) + 
  labs (x = "Unidades de muestreo", 
        y = "Diversidad de especies")





