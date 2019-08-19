
library(data.table)
library(tidyverse)
library(reshape2)
library(dplyr)


BASE <- read.csv("base_trabajo.csv", row.names = 1)
head (BASE)

source("Funciones.R")

BASE_10 <- subset_10_registers(BASE)
class (BASE_10)
length (BASE_10)

calculate_indep (BASE_10[[1]]) 

TiempoIndep <- c(350,248,85,300,592,475,178,31,42,325,
                 1,362,342)
TiempoIndep <- cbind (BASE_10[[14]], TiempoIndep)
print (TiempoIndep)
write.csv (TiempoIndep, "Species_times.csv")


BASE_ANALIZADA <- analyze_times("base_trabajo.csv",
                                "Species_times.csv","base_analizada.csv")
head (BASE_ANALIZADA,10)

em_station <- read.csv ("EM_station.csv", header = T)

FC <- calculate_fc("base_analizada.csv",
                   "EM_Station.csv", "FC_result.csv", 
                   melted = T)
head (FC, 10)


DURATION_VISIT <- calculate_visit ("base_analizada.csv",
                                 "Duration_result.csv")
head (DURATION_VISIT, 10)


MODE_INDIVIDUALS <- calculate_mode_indiv(
  "base_analizada.csv", "Mode_indiv_result.csv")
head (MODE_INDIVIDUALS, 10)


library(dplyr)

total_data <- full_join(FC, DURATION_VISIT,
                        by = c("StationID", "SpeciesID")) %>%
  full_join(MODE_INDIVIDUALS, 
            by = c("StationID", "SpeciesID")) %>%
  full_join(em_station[,c("SpeciesTree",
                          "StationID")], by="StationID")

total_data$ifi <- with (total_data, 
                        FC*mean_duration*mode_individuals)


IFI_mean <- with (total_data, round(tapply (ifi, 
                                            list(SpeciesID, SpeciesTree), mean),2))

IFI_mean_stand <- round (IFI_mean/rep (apply (IFI_mean, 
                                              2, max), each = dim (IFI_mean)[1]),2)













