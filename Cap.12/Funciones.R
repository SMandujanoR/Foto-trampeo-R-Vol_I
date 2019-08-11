# Funciones capitulo analisis de interacciones

## Función "subset_10_registers" -------------------------------------------

subset_10_registers<-function(bd){
  
  require(tidyverse)
  
  posicion <- dim(bd)[2]
  
  #convertir a formato de fecha y hora
  bd<-cbind (bd, (as.POSIXct(paste(bd$Date,bd$Time,sep=" "), 
                             format="%d/%m/%Y %H:%M:%S")))
  
  names (bd)[posicion+1]<-"Time2"
  
  spp <- unique(bd$SpeciesID)
  
  #calcular no.registros por mamífero y árbol
  registros_sp <- tapply (bd$CaptureID, list(bd$SpeciesTree, 
                                             bd$SpeciesID), length) 
  
  combinaciones <- reshape2::melt(registros_sp)
  
  registros_sp_10 <- combinaciones [which (combinaciones[,3]>10),]
  
  #lista que guardará los subconjuntos
  datosSP<-vector("list",dim(registros_sp_10)[1]+1)
  
  for (i in 1:dim(registros_sp_10)[1]) {
    datosSP[[i]]<-filter(bd, bd$SpeciesID == registros_sp_10$Var2[i] &
                           bd$SpeciesTree == registros_sp_10$Var1[i])
  }
  datosSP[[length(datosSP)]]<-registros_sp_10[,-3]
  
  names(datosSP)<-paste(registros_sp_10$Var2,
                        registros_sp_10$Var1)
  
  names(datosSP)[length(datosSP)]<-"Groups"
  
  return(datosSP)
}


## Función "calculate_indep" -----------------------------------------------

calculate_indep <- function (x) {
  
  require(tidyverse)
  
  require(data.table)
  
  Tiempo <- seq (1,1440,1) #Indica el tiempo de 1 hasta 1440 min (24 h)
  
  TRUEs <- numeric() #guarda el número de eventos independientes
  
  for (i in 1:1440){
    k <- 2 #marcador de posicion. 
    
    Indep <- logical () #guarda T/F
    Indep [1] <- T #el primer registro siempre es T (=independiente)
    
    while (k <= dim (x)[1]) {
      if (difftime (x$Time2[k], x$Time2[k-1], units = "mins") 
          > Tiempo[i]) { Indep[k]<-T } 
      else { Indep[k]<-F }
      
      k <- k+1 
    }
    TRUEs[i]<-sum (Indep)
  }
  
  #Crear función para calcular la moda, es decir, la meseta en la gráfica 
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  mode_1 <- getmode(TRUEs)
  mode_position <- grep(TRUE,(mode_1==TRUEs)) 
  meseta_1 <- Tiempo[first(mode_position)]
  meseta_2 <- Tiempo[last(mode_position)]
  
  #graficar
  plot (Tiempo, TRUEs, cex=0.3,cex.axis=1, ylab = "No. Registros", 
        xlab="Tiempo",las=1, main=paste(x$SpeciesID[1],x$SpeciesTree[1], sep = " - "))
  
  out<-list(minimo=meseta_1, records=max(TRUEs), grouped_records=mode_1)
  return(out)
}


## Función "analyze_times" -------------------------------------------------

analyze_times <- function(input_file, times_file, output_file) {
  
  BASE<-read.csv(input_file,row.names = 1)
  
  posicion <- dim(BASE)[2]
  
  #convertir a formato de fecha y hora
  BASE<-cbind (BASE, (as.POSIXct(paste(BASE$Date,BASE$Time,sep=" "), 
                                 format="%d/%m/%Y %H:%M:%S")))
  names (BASE)[posicion+1]<-"Time2"
  
  SPECIES_TIMES <- read.csv (times_file, header = F, row.names = 1, skip = 1)
  
  base_size <- dim(BASE)[1]
  species_times_size <- dim(SPECIES_TIMES)[1]
  previous_tree = ""
  previous_species = ""
  previous_time = 0
  
  BASE_ANALYZED = BASE
  base_n_cols <- dim(BASE_ANALYZED)[2]
  
  for(i in 1:base_size) {
    # La especie para este registro
    species = BASE$SpeciesID[i]
    # La hora del registro
    time = BASE$Time2[i]
    # arbol focal del registro
    tree = BASE$SpeciesTree[i]
    # Inicializar threshold
    threshold = 0
    
    # Leer tabla de especies vs. duracion de la muestra y extraer
    for(j in 1:species_times_size) {
      species_id = SPECIES_TIMES[j,2]
      tree_id= SPECIES_TIMES[j,1]
      if(species_id == species & tree_id == tree) {
        threshold = SPECIES_TIMES[j,3]} else 
        {threshold = min(SPECIES_TIMES[j,3])
        }
    }
    
    if(species != previous_species || tree != previous_tree) { 
      BASE_ANALYZED[i,base_n_cols+1] = TRUE
      previous_species = species
      previous_time = time
      previous_tree = tree
      next
    }
    
    if(difftime(time, previous_time, units = "mins") > threshold) {
      BASE_ANALYZED[i,base_n_cols+1] = TRUE
    } else {
      BASE_ANALYZED[i,base_n_cols+1] = FALSE
    }
    previous_species = species
    previous_time = time
    previous_tree = tree
    
  }
  names(BASE_ANALYZED)[base_n_cols+1] <- "Independ"
  write.csv(BASE_ANALYZED, output_file,row.names = F)
  
  return(BASE_ANALYZED)
}

## Función "calculate_fc" --------------------------------------------------

calculate_fc <- function(input_file, em_file, output_file, n=100, melted=TRUE){
  
  require(reshape2)
  
  EM <- read.csv (em_file, header = T)
  
  Trues <- read.csv (input_file, header = T)
  
  indep_records <- tapply (Trues$Independ,
                           list(Trues$StationID,Trues$SpeciesID), sum)
  
  StationID<-unique(Trues$StationID)
  SpeciesID<-unique(Trues$SpeciesID)
  
  FC<-indep_records
  
  
  for (i in 1:length(StationID)) {
    for (j in 1:length(SpeciesID)) {
      FC[as.character(StationID[i]),as.character(SpeciesID[j])]<-
        round((indep_records[as.character(StationID[i]),
                             as.character(SpeciesID[j])
                             ]/EM[EM$StationID ==
                                    as.character(StationID[i]),"Dias"])*n,1)
    }}
  
  FC[is.na(FC)]<-0
  
  if(melted==TRUE){
    FC<-reshape2::melt(FC)
    names (FC) <- c("StationID", "SpeciesID", "FC")
  }else{FC=FC}
  
  write.csv (FC, output_file)
  
  return(FC)
}

## Función "calculate_visit" -----------------------------------------------

calculate_visit <- function (input_file, output_file) {
  
  BASE <- read.csv(input_file)
  
  duration <- numeric() #guarda la duracion de la visita en min
  sp_mammal <- character() #guarda el nombre de la sp. de mamifero
  sp_tree <- character() #guarda el nombre de la sp. de arbol
  station_id <- character() #guarda el nombre del arbol focal
  date_ini <- character() #guarda la fecha y hora inicial
  date_end <- character() #guarda la fecha y hora final
  
  n<-1
  narbol<-1
  
  while (n <= dim (BASE)[1]){
    m = n
    
    if (n < dim (BASE)[1]){
      while (BASE$Independ[m+1] == F & m+1 < dim (BASE)[1]) {m <- m+1}
      if (m+1 == dim (BASE)[1] & BASE$Independ[m+1] == F) {m <- m+1}
    }
    if (m == n) {duration[narbol] <- 0}
    else {duration[narbol] <- round(difftime (BASE$Time2[m], BASE$Time2[n], units = "mins"),3)}
    sp_mammal[narbol] <- as.character(BASE$SpeciesID[n])
    sp_tree[narbol] <- as.character(BASE$SpeciesTree[n])
    station_id[narbol] <- as.character(BASE$StationID[n])
    date_ini[narbol] <- as.character(BASE$Time2[n]) 
    date_ini[narbol] <- as.character(BASE$Time2[m]) 
    
    narbol <- narbol + 1
    n = m+1
  }
  
  visits <- data.frame (sp_mammal, sp_tree, station_id, date_ini, date_ini, duration)
  colnames (visits) <- c("SpeciesID", "SpeciesTree","StationID", "DateI", "DateF", "Duration")
  
  MEAN <- round(tapply (visits$Duration, list(visits$StationID, visits$SpeciesID), mean),2)
  
  duration_visit <- reshape2::melt(MEAN)
  duration_visit[is.na(duration_visit)] <- 0
  names (duration_visit) <- c("StationID", "SpeciesID", "mean_duration")
  
  write.csv (duration_visit, output_file)
  
  return(duration_visit)  
}

## Función "calculate_mode_indiv" ------------------------------------------

calculate_mode_indiv <- function (input_file, output_file) {
  
  BASE <- read.csv(input_file)
  
  mode_indiv <- numeric() #guarda la moda en el num. de indiv. para c/evento
  sp_mammal <- character() #guarda el nombre de la sp. de mamifero
  sp_tree <- character() #guarda el nombre de la sp. de arbol
  station_id <- character() #guarda el nombre del arbol focal
  
  ###Funcion para encontrar la moda
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  n<-1
  narbol<-1
  
  while (n <= dim (BASE)[1]){
    m = n
    
    if (n < dim (BASE)[1]){
      while (BASE$Independ[m+1] == F & m+1 < dim (BASE)[1]) {m <- m+1}
      if (m+1 == dim (BASE)[1] & BASE$Independ[m+1] == F) {m <- m+1}
    }
    if (m == n) {mode_indiv[narbol] <- 0}
    else {mode_indiv[narbol] <- getmode(BASE$Individuals[n]:BASE$Individuals[m])}
    sp_mammal[narbol] <- as.character(BASE$SpeciesID[n])
    sp_tree[narbol] <- as.character(BASE$SpeciesTree[n]) 
    station_id[narbol] <- as.character(BASE$StationID[n])
    
    narbol <- narbol + 1
    n = m+1
  }
  
  individuals <- data.frame (sp_mammal, sp_tree, station_id, mode_indiv)
  colnames (individuals) <- c("SpeciesID", "SpeciesTree","StationID", "Mode")
  
  MEAN <- round(tapply (individuals$Mode, list(individuals$StationID, individuals$SpeciesID), mean),2)
  
  mode_individuals <- reshape2::melt(MEAN)
  
  mode_individuals[is.na(mode_individuals)] <- 0
  names (mode_individuals) <- c("StationID", "SpeciesID", "mode_individuals")
  
  write.csv (mode_individuals, output_file)
  
  return(mode_individuals)  
}