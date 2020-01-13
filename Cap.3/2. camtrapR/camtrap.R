#######################################################################
###                  "Taller de Foto-trampeo R"                     ###
###   Paquetería camtrapR para la organizacion y manejo de datos    ###           
###                                                                 ###
###      Luz A. Perez-Solano       adriana.perez.s@outlook.com      ###
####################################################################### 
##*Script sin acentos*


library (camtrapR)


Sitio1 <- read.csv ("Cap.3/2. camtrapR/Ejercicio.csv", header = TRUE, sep=",")
View(Sitio1)

#------------------------
## CREAR CARPETAS DE TRABAJO


directory_original <- file.path("Cap.3/2. camtrapR/IMAGENES_ORIGINALES") 


Sitio1_c <- createStationFolders (inDir = directory_original,
                                  stations = as.character (Sitio1$Station), 
                                  createinDir = TRUE)

# --> Arrastrar cada foto a la subcarpeta correspondiente 


directory_images_renamed <- file.path("Cap.3/2. camtrapR/RENAMED")

#------------------------
## RENOMBRAR LAS IMAGENES

renaming_table <- imageRename(inDir = directory_original,
                              outDir = directory_images_renamed, 
                              hasCameraFolders = FALSE, 
                              copyImages = TRUE) 

# ¡Ojo! outDir no puede ser un subdirectorio de inDir.

print (renaming_table)

#------------------------
## AGREGAR ETIQUETA COPYRIGHT A LAS IMAGENES

proyecto_institucion <- "Monitoreo_RBTC_INECOL"  

addCopyrightTag(inDir = directory_images_renamed, 
                copyrightTag = proyecto_institucion,
                askFirst = F, 
                keepJPG_original = F) 

exifTagNames(directory_images_renamed, 
             returnMetadata = T) 

#------------------------
## IDENTIFICACION DE IMAGENES

# --> Etiquetado: En digiKam etiquetamos por especies


data_table_c <- recordTable(inDir = directory_images_renamed, 
                            IDfrom = "metadata", 
                            metadataSpeciesTag = "Species") 

#Exploramos:
head (data_table_c) 
View(data_table_c)
write.csv(data_table_c, "data_table_c.csv") #Exportar la base
#NOTA: esta base contiene TODOS los registros de cada fotografia, no son registros unicos (independientes)

#------------------------
## FUNCIONES OPCIONALES

#Agregar nombre de las especies a los nombres de los archivos

print (directory_images_renamed)
species_names_append <- appendSpeciesNames(inDir = directory_images_renamed,
                                           IDfrom = "metadata", 
                                           hasCameraFolders = F,
                                           metadataSpeciesTag = "Species")

head (species_names_append)

#Si se quieren remover los nombres:
species_names_remove <- appendSpeciesNames(inDir = directory_images_renamed,
                                           IDfrom = "metadata",
                                           hasCameraFolders = F,
                                           metadataSpeciesTag = "Species",
                                           removeNames = T) 

head(species_names_remove)

#------------------------
## COLECTAR TODAS LAS IMAGENES DE UNA ESPECIE

species_to_copy <- "Odocoileus virginianus" # especificar especie a copiar

#Establecer directorio donde las imagenes seran guardadas
directory_images_species <- file.path ("Cap.3/2. camtrapR/RENAMED SPECIES")
print (directory_images_species)

species_images <- getSpeciesImages(species = species_to_copy, 
                                   inDir = directory_images_renamed,
                                   outDir = directory_images_species,
                                   IDfrom = "metadata",
                                   createStationSubfolders = F,
                                   metadataSpeciesTag = "Species")
print (species_images)

# --> Cambiar argumento *createStationSubfolders* = T por F (pero antes borrar la carpeta de la especie que se crea), correr de nuevo y ver que las imagenes ya no estan organizadas en carpetas de Stations.

#------------------------
## EXTRAER OTROS METADATOS DE LA IMAGEN

data_table_metadata1 <- recordTable(inDir = directory_images_renamed, 
                                    IDfrom = "metadata", 
                                    metadataSpeciesTag = "Species", 
                                    exclude = "empty", 
                                    additionalMetadataTags = c("EXIF:Model", "EXIF:Make"))
View(data_table_metadata1)

# --> En digiKam pueden ser agregados otros metadatos como numero de individuos, sexo, comportamiento, etc.

#------------------------
## EXCLUIR ESPECIES 

data_table_exclude <- recordTable(inDir = directory_images_renamed, 
                          IDfrom = "metadata",
                          metadataSpeciesTag = "Species", 
                          exclude = "Aves") 

#------------------------
## AGREGAR INDEPENDENCIA ENTRE EVENTOS FOTOGRAFICOS

data_table_independientes <- recordTable(inDir = directory_images_renamed, 
                           IDfrom = "metadata", 
                           metadataSpeciesTag = "Species", 
                           exclude = "empty", 
                           minDeltaTime = 60,  # 1440 por 24 h
                           deltaTimeComparedTo = "lastRecord",
                           timeZone = "UTC") 

View(data_table_independientes)

write.csv (data_table_independientes, "eventos_indep_60.csv")

#------------------------
## EXPLORACION Y VISUALIZACIÓN DE DATOS

##Numero de registros independientes por especie

#Subset para especie de interes
data_table_sp_interes <- data_table_independientes[data_table_independientes$Species == "Odocoileus virginianus",]


Map_test2 <- detectionMaps(CTtable = Sitio1, 
                           recordTable = data_table_sp_interes, 
                           Xcol = "utm_x", Ycol = "utm_y", stationCol = "Station",
                           speciesCol = "Species", printLabels = T,
                           richnessPlot = F, 
                           speciesPlots = T, 
                           speciesToShow = "Odocoileus virginianus", 
                           addLegend = F, smallPoints = 2)

##Estimar la actividad empleando la densidad kernel

conejo <- "Sylvilagus floridanus"  #Especie de interes

activityDensity(recordTable = data_table_independientes, species = conejo, xlab = "Hora", ylab = "Densidad")


##Comparar dos especies 

zorra <- "Urocyon cinereoargenteus"

windows()
activityOverlap(recordTable = data_table_independientes,
                speciesA = conejo,
                speciesB = zorra,
                writePNG = FALSE,
                plotR = TRUE,
                addLegend = TRUE,
                legendPosition = "topright",
                linecol = c("grey2", "black"),
                linewidth = c(3,3),
                add.rug = TRUE,
                xlab = "Hora",
                ylab = "Densidad",
                ylim = c(0,0.25))

##Grafica circular de actividad con barras:

activityRadial(recordTable = data_table_independientes, species = zorra, lwd = 3)

##Grafica circular de actividad con poligono

activityRadial(recordTable = data_table_independientes, species = zorra, speciesCol = "Species", plotR = TRUE, ldw = 3, rp.type = "p")

#####
#REPORTE

reporte_sitio_1 <- surveyReport(recordTable = data_table_independientes, 
CTtable = Sitio1, 
speciesCol = "Species", 
stationCol = "Station", 
setupCol = "Setup_date", 
retrievalCol = "Retrieval_date", 
CTDateFormat = "%d/%m/%Y", 
recordDateTimeCol = "DateTimeOriginal", 
recordDateTimeFormat = "%Y-%m-%d %H:%M: %S",
makezip = T, 
sinkpath = "Cap.3/2. camtrapR")

#Si se quiere generar un reporte para extraer informacion especifica:

#1- Opereacion de la camara, no. de días activa e inactiva:
reporte_sitio_1 [[1]] 

#2. Numero de especies por estacion 
reporte_sitio_1 [[2]] 

#3. Numero de registros independientes por especie y no. de estaciones en las que se registro:
reporte_sitio_1 [[3]] 

#4. No. de registros de cada especie por estacion
reporte_sitio_1 [[4]] 

#Ejemplo de como exportar:
Especies_estacion <- reporte_sitio_1 [[4]] 

write.csv(Especies_estacion, "Especies_estacion.csv")


