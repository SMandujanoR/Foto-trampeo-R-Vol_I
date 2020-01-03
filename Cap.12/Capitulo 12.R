################################################################
# Mandujano, S. y L.A. Pérez-Solano. (Eds.). 2019. Fototrampeo en R: organización y análisis de datos. Volumen I. Instituto de Ecología A.C., Xalapa, Ver., México. 248 pp. ISBN: 978-607-7579-90-8
################################################################
# CAPÍTULO 12
# Interacciones fruto-mamíferos: data.table, tidyverse, reshape2, dplyr
# Angela A. Camargo-sanabria y Carlos M. Delgador-Martínez
################################################################

library(data.table)
library(tidyverse)
library(reshape2)
library(dplyr)

# además:
source("Cap.12/Funciones.R")

BASE <- read.csv("Cap.12/base_trabajo.csv", row.names = 1)
head (BASE)

BASE_10 <- subset_10_registers(BASE)
class (BASE_10)
length (BASE_10)

calculate_indep (BASE_10[[1]]) 

TiempoIndep <- c(350,248,85,300,592,475,178,31,42,325,1,362,342)
TiempoIndep <- cbind (BASE_10[[14]], TiempoIndep)
print (TiempoIndep)
#write.csv (TiempoIndep, "Cap.12/Species_times.csv")

BASE_ANALIZADA <- analyze_times("Cap.12/base_trabajo.csv", "Cap.12/Species_times.csv", "Cap.12/base_analizada.csv")
head (BASE_ANALIZADA, 10)

em_station <- read.csv ("Cap.12/EM_station.csv", header = T)

FC <- calculate_fc("Cap.12/base_analizada.csv", "Cap.12/EM_Station.csv", "Cap.12/FC_result.csv", melted = T)
head (FC, 10)

DURATION_VISIT <- calculate_visit ("Cap.12/base_analizada.csv", "Cap.12/Duration_result.csv")
head (DURATION_VISIT, 10)

MODE_INDIVIDUALS <- calculate_mode_indiv("Cap.12/base_analizada.csv", "Cap.12/Mode_indiv_result.csv")
head (MODE_INDIVIDUALS, 10)

total_data <- full_join(FC, DURATION_VISIT, by = c("StationID", "SpeciesID")) %>%
  full_join(MODE_INDIVIDUALS, by = c("StationID", "SpeciesID")) %>%
  full_join(em_station[,c("SpeciesTree", "StationID")], by="StationID")

total_data$ifi <- with (total_data, FC*mean_duration*mode_individuals)

IFI_mean <- with (total_data, round(tapply (ifi, list(SpeciesID, SpeciesTree), mean),2))

IFI_mean_stand <- round (IFI_mean/rep (apply (IFI_mean, 2, max), each = dim (IFI_mean)[1]),2)

########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 
