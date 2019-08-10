# Instalación y datos iniciales ####

#1. El primer paso es instalar el paquete camtrapR

install.packages("camtrapR")

#Nota: Después de instalar el paquete cada que se abre R nuevamente se debe cargar

library(camtrapR)

#---------------------------------------------------

#2. Se debe verificar la ruta de exiftool

Sys.getenv("PATH")
Sys.which("exiftool")

#------------------------------------------------------------

# Organización y manejo de las fotos ####

#3.Leer el archivo csv con la información de ubicación y actividad de las cámaras

sitio_1 <- read.csv ("actividad de estaciones_1_cam.csv", header = TRUE) #Dentro del parentesis se debe especificar la ruta donde se ubica el archivo

# Pedir a R que muestre la tabla
sitio_1

#------------------------------------------------------

#4. Creación de la carpeta principal para colocar las fotos

fotos_sitio_1 <- file.path("Sitio_1")#En esta función se indica la ruta y el nombre de la carpeta principal

fotos_sitio_1_carpeta <- createStationFolders (inDir = fotos_sitio_1,
stations = as.character  (sitio_1$Station),
createinDir = TRUE)

#-----------------------------------------------------

#5. Después de colocar las imagenes en su carpeta correspondiente, el siguiente paso es verificar los datos de fecha y hora. Si hay errores se corrigen con la función "timeshift":

#Primero se carga la carpeta que contiene las fotos a corregir

fotos_correccion_sito_1 <- file.path("corrección_sitio_1")

#Después se carga la tabla con la información

tabla_correccion_sitio_1 <- read.csv("correccion de hora.csv", header = TRUE)
View(tabla_correccion_sitio_1)

#Por último se corre la función

fotos_sitio_1_corregidas <- timeShiftImages(inDir = fotos_correccion_sitio_1,
timeShiftTable = tabla_correccion_sitio_1,
stationCol = "Station",
hasCameraFolders = FALSE,
timeShiftColumn = "timeshift",
timeShiftSignColumn = "sign",
undo = T)
#----------------------------------------------------------

#6. Renombrar fotos: para esto se utilizan las siguientes funciones

fotos_sitio_1_renombradas <- file.path("Sitio_1_R") #se especifica la dirección en donde estará la carpeta y el nombre

imageRename(inDir = fotos_sitio_1,
            outDir = fotos_sitio_1_renombradas,
            hasCameraFolders = FALSE,
            keepCameraSubfolders = FALSE,
            copyImages = TRUE)#las imagenes son copiadas en la nueva carpeta

#Nota: se recomienda guardar las fotos originales que fueron extraidas de las tarjetas SD

#------------------------------------------------------
#7. Una vez renombradas las fotos se puede agregar a los metadatos el nombre del proyecto e institución a la que pertenecen

proyecto_institucion <- "Proyecto_RBTC (INECOL)"

addCopyrightTag(inDir        = fotos_sitio_1_renombradas, 
                copyrightTag = proyecto_institucion, 
                askFirst     = FALSE,
                keepJPG_original = F)

#Para corroborar que se haya grabado la información en los metadatos se utiliza:

exifTagNames(fotos_sitio_1_renombradas, returnMetadata = TRUE)


#----------------------------------------------------
# Clasificación por especie ####


#8. Clasificación de fotos por especie

#Se crea un objeto con el nombre de cada especie o sus nombres comunes

especies_sitio_1 <- c("Conejo","Coyote", "Lince", "Pecarí", "Venado", "Zorra")

#Después de listar todas las especies, se crean las subcarpetas con la siguiente función

createSpeciesFolders(inDir = fotos_sitio_1_renombradas,
                     species = especies_sitio_1,
                     hasCameraFolders = FALSE,
                     removeFolders = FALSE)
#---------------------------------------------------------

#9. También se pueden crear carpetas que contengan las fotos de todas las cámaras de una sola especie:

#Se carga la dirección de la carpeta

fotos_venado_sitio_1 <- file.path("Venado")

#Se utiliza la ruta de la carpeta con las fotos renombradas

fotos_sitio_1_renombradas <- file.path("Sitio_1_R")

#Se crea un objeto con el nombre de la especie que se quiere extraer

venado <- "Venado"

#Se corre la siguiente función para crear la nueva carpeta

getSpeciesImages(species = "venado",
                 IDfrom = "directory",
                 inDir = fotos_sitio_1_renombradas,
                 outDir = fotos_venado_sitio_1,
                 createStationSubfolders = TRUE)# si se utiliza FALSE todas las fotos estarán en una sola carpeta

#La nueva carpeta "Venado" no puede ser guardada dentro de la carpeta de fotos reenombradas, por lo tanto inDir y outDir deben tener rutas diferentes

#-------------------------------------------------------

# Extracción de metadatos por especie y/o individuos ####

#10.Creación de tablas con información de especies y/o individuos

tabla_sitio_1 <- recordTable(inDir = fotos_sitio_1_renombradas,
IDfrom = "directory",
timeZone = "America/Mexico_City",
writecsv = FALSE)#crea una tabla con todas las fotos sin contemplar registros independientes

View(tabla_sitio_1)

write.csv(tabla_sitio_1, "tabla sitio 1.csv")

#--------------------------------------------------------

#11. Para obtener la tabla solo con los registros independientes se se deben agregar otros datos a la función:

tabla_sitio_1_independientes <- recordTable(inDir = fotos_sitio_1_renombradas,
IDfrom = "directory",
minDeltaTime = 60,
deltaTimeComparedTo = "lastRecord",
timeZone = "America/Mexico_City")

write.csv(tabla_sitio_1_independientes, "tabla sitio 1 independientes.csv")

View(tabla_sitio_1_independientes)

#Posteriormente, las tablas pueden ser leídas en camtrapR con la siguiente funcion


tabla_sitio_1 <- read.csv("tabla sitio 1.csv", header = T)

tabla_sitio_1_independientes <- read.csv("tabla sitio 1 independientes.csv", header = T)

#---------------------------------------------------------

#12. Para crear las tablas con la información de cada individuo, se utiliza el objeto creado anteriormente con la dirección de la carpeta "Venado"


fotos_venado_sitio_1 <- file.path("Venado")

#Después se utiliza la siguiente función

tabla_venado_individuos <- recordTableIndividual(inDir = fotos_venado_sitio_1,
IDfrom = "directory",
minDeltaTime = 60,
deltaTimeComparedTo = "lastIndependentRecord",
metadataIDTag = "individuo", 
hasStationFolders = TRUE,
timeZone = "America/Mexico_City")

write.csv(tabla_venado_individuos, "tabla venado.csv")

#En IDfrom se utiliza "directory" si las fotos estan en subcarpetas y "metadata" si tienen etiquetas.

#En metadataIDTag se debe poner el nombre que identifica a la carpeta de cada individuo. 

#----------------------------------------

#13. Riqueza de especies por estación:

sitio_1 <- read.csv("actividad de estaciones_1_cam.csv", header = T)

jpeg(filename = "Figura_7.jpg", width = 2400, height = 1980, units = "px", res = 300)

mapa_riqueza_estación <- detectionMaps(CTtable     = sitio_1,
                recordTable  = tabla_sitio_1_independientes,
                Xcol         = "X",
                Ycol         = "Y",
                stationCol   = "Station",
                speciesCol   = "Species",
                printLabels  = TRUE,
                richnessPlot = TRUE,    
                speciesPlots = FALSE,
                addLegend    = TRUE)
dev.off()


#Objeto "Sitio_1" contiene la tabla con las UTM y el lapso de operación de cada cámara. El objeto "tabla_sitio_1_independientes" es el que contiene los metadatos de los registros independientes.

#-------------------------------------------------
#14. Número de registros de una especie por estación

jpeg(filename = "Figura_8.jpg", width = 2400, height = 1980, units = "px", res = 300)

mapa_lince <- detectionMaps(CTtable      = sitio_1,
              recordTable   = tabla_sitio_1_independientes,
              Xcol          = "X",
              Ycol          = "Y",
              stationCol    = "Station",
              speciesCol    = "Species",
              speciesToShow = "Lince",    
              printLabels   = TRUE,
              richnessPlot  = FALSE,     
              speciesPlots  = TRUE,      
              addLegend     = TRUE)

dev.off()
#-----------------------------------------------------

#15. Graficos de actividad por especie:  

#Densidad kernel

zorra <- "Zorra" #se crea un objeto con el nombre de la especie que se va utilizar


jpeg(filename = "Figura_9.jpg", width = 2400, height = 1980, units = "px", res = 300)

activityDensity(recordTable = tabla_sitio_1_independientes, 
species = zorra,
extend = NULL, 
xlab = "Hora", 
ylab = "Densidad",
bty = "L")

dev.off()


#Histogramas

jpeg(filename = "Figura_10.jpg", width = 2400, height = 1980, units = "px", res = 300)

activityHistogram(recordTable = tabla_sitio_1_independientes,
species = zorra)

dev.off()

#Grafica circular con barras

venado <- "Venado"

jpeg(filename = "Figura_11.jpg", width = 2400, height = 1980, units = "px", res = 300)

activityRadial(recordTable = tabla_sitio_1_independientes, 
               species = venado, 
               lwd = 3)
dev.off()

#Gráfica circular con polígono

jpeg(filename = "Figura_12.jpg", width = 2400, height = 1980, units = "px", res = 300)


activityRadial(recordTable = tabla_sitio_1_independientes, species = venado, 
speciesCol = "Species", 
plotR = TRUE, 
lwd = 3, 
rp.type = "p")


dev.off()

#Comparar la actividad de dos especies con densidad kernel

conejo <- "Conejo" #se crea otro objeto con el nombre de la especie con la que se quiere comparar la actividad de la zorra

jpeg(filename = "Figura_13b.jpg", width = 2400, height = 1980, units = "px", res = 300)

conejo_zorra <- activityOverlap(recordTable = tabla_sitio_1_independientes, 
                speciesA = zorra, 
                speciesB = conejo, 
                writePNG = FALSE, 
                plotR = TRUE, 
                addLegend = TRUE, 
                legendPosition = "topright", 
                linecol = c("grey", "black"), 
                linewidth = c(3,3), 
                add.rug = TRUE, 
                xlab = "Hora", 
                ylab = "Densidad", 
                ylim = c(0,0.40))

dev.off

summary(conejo_zorra)
str(conejo_zorra)
#-----------------------------------------------------

#16. Reporte de datos obtenidos:

reporte_sitio_1 <- surveyReport (recordTable = tabla_sitio_1_independientes,
CTtable = sitio_1,
speciesCol = "Species",
stationCol = "Station",
setupCol = "Fecha_colocación",
retrievalCol = "Fecha_retiro",
CTDateFormat = "%d/%m/%Y", 
recordDateTimeCol = "DateTimeOriginal",
recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
CTHasProblems = T)


#Operación de la cámara, número de días activa e inactiva

reporte_sitio_1 [[1]]

#Número de especies por estación

reporte_sitio_1 [[2]]

#Número de registros independientes totales por especie y número de estaciones en las que se registraron

reporte_sitio_1 [[3]]

#Número de registros de cada especie por estación

reporte_sitio_1 [[4]]

#--------------------------------------------------------

#17. Para crear las tablas de historias de captura por especie, primero se necesita crear un objeto que contenga la tabla con la información de cuantos días estuvo activa cada cámara 


cam_activas_sitio_1 <- cameraOperation(CTtable = sitio_1,
stationCol = "Station",
setupCol = "Fecha_colocación",
retrievalCol = "Fecha_retiro",
hasProblems = T,
dateFormat = "%d/%m/%Y")

View(cam_activas_sitio_1)

#También se puede obtener la gráfica del periodo de operación de las cámaras:

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

jpeg(filename = "Figura_16.jpg", width = 2400, height = 1980, units = "px", res = 300)

camopPlot(camOp = cam_activas_sitio_1)

dev.off()


#Después se utiliza la siguiente función para crear la tabla

hist_captura_venado <- detectionHistory(recordTable = tabla_sitio_1_independientes, 
camOp = cam_activas_sitio_1,
stationCo = "Station", 
speciesCol = "Species", 
recordDateTimeCol = "DateTimeOriginal", 
recordDateTimeFormat = "%Y-%m-%d", 
species = "Venado", 
occasionLength = 1, 
day1 = "station", 
includeEffort = FALSE, 
timeZone = "America/Mexico_City")

View(hist_captura_venado)


#En occasionLength se debe especificar el número de días de cada ocasión de detección, es decir "1" dará la detección del venado en cada día de actividad de la cámara y "10" agrupará las detecciones de venado cada diez días


#Se puede guardar la tabla creada

write.csv(hist_captura_venado, "historia captura venado.csv")

#----------------------------------------------------

#Fin del script
