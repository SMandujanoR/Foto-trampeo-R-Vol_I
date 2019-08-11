###para la etapa 4

library(dplyr)
EM<-read.csv("EM_Station.csv",header = T)

FC<-read.csv("FC_result.csv")
DURATION_VISIT<-read.csv("Duration_result.csv")
MODE_INDIVIDUALS<-read.csv("Mode_indiv_result.csv")

names (FC); names (DURATION_VISIT); names (MODE_INDIVIDUALS)

total_data <- full_join(FC,DURATION_VISIT,by = c("StationID", "SpeciesID")) %>%
  full_join(MODE_INDIVIDUALS,by = c("StationID", "SpeciesID")) %>%
  full_join(EM[,c("SpeciesTree","StationID")],by="StationID")

#Calcular índice
total_data$ifi <- with (total_data, FC*mean_duration*mode_individuals)

#Obtener índice promedio por especie de mamífero y especie de árbol
IFI_mean <- with (total_data, round(tapply (ifi, list(SpeciesID, SpeciesTree), mean),2))

IFI_mean_stand <- round (IFI_mean/rep (apply (IFI_mean, 2, max), 
                                       each = dim (IFI_mean)[1]),2)


