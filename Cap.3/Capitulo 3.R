
install.packages("camtrapR")



library(camtrapR)

#---------------------------------------------------
Sys.getenv("PATH")
Sys.which("exiftool")

#------------------------------------------------------------
sitio_1 <- read.csv ("actividad de estaciones_1_cam.csv", header = TRUE) 
sitio_1

#------------------------------------------------------
fotos_sitio_1 <- file.path("Sitio_1")

fotos_sitio_1_carpeta <- createStationFolders (inDir = fotos_sitio_1,
stations = as.character  (sitio_1$Station),
createinDir = TRUE)

#Transferir las fotos de la carpeta "Set_sitio_1" a la carpeta "Sitio_1" para poder ejecutar el script
#-----------------------------------------------------
fotos_correccion_sitio_1 <- file.path("correccion_sitio_1")

tabla_correccion_sitio_1 <- read.csv("correccion de hora.csv", header = TRUE)
View(tabla_correccion_sitio_1)


fotos_sitio_1_corregidas <- timeShiftImages(inDir = fotos_correccion_sitio_1,
timeShiftTable = tabla_correccion_sitio_1,
stationCol = "Station",
hasCameraFolders = FALSE,
timeShiftColumn = "timeshift",
timeShiftSignColumn = "sign",
undo = F)

#En caso de querer revertir el cambio de fecha y hora en "undo" se debe sustituir F por T
#----------------------------------------------------------

fotos_sitio_1_renombradas <- file.path("Sitio_1_R") 

imageRename(inDir = fotos_sitio_1,
            outDir = fotos_sitio_1_renombradas,
            hasCameraFolders = FALSE,
            keepCameraSubfolders = FALSE,
            copyImages = TRUE)

#------------------------------------------------------
proyecto_institucion <- "Proyecto_RBTC (INECOL)"

addCopyrightTag(inDir        = fotos_sitio_1_renombradas, 
                copyrightTag = proyecto_institucion, 
                askFirst     = FALSE,
                keepJPG_original = F)

exifTagNames(fotos_sitio_1_renombradas, returnMetadata = TRUE)

#----------------------------------------------------

especies_sitio_1 <- c("Conejo","Coyote", "Lince", "Pecarí", "Venado", "Zorra")

createSpeciesFolders(inDir = fotos_sitio_1_renombradas,
                     species = especies_sitio_1,
                     hasCameraFolders = FALSE,
                     removeFolders = FALSE)

#Una vez creadas las carpetas de las especies se deben arrastrar las fotos a su carpeta correspondiente 
#---------------------------------------------------------

fotos_venado_sitio_1 <- file.path("Venado")


fotos_sitio_1_renombradas <- file.path("Sitio_1_R")


venado <- "Venado"

getSpeciesImages(species = "venado",
                 IDfrom = "directory",
                 inDir = fotos_sitio_1_renombradas,
                 outDir = fotos_venado_sitio_1,
                 createStationSubfolders = TRUE)

#Sustituir el nombre de la carpeta "Set_venados" por "Venado" para poder crear la "tabla_venado_indivuos" que está más adelante

#-------------------------------------------------------

tabla_sitio_1 <- recordTable(inDir = fotos_sitio_1_renombradas,
IDfrom = "directory",
timeZone = "America/Mexico_City",
writecsv = FALSE)

View(tabla_sitio_1)

write.csv(tabla_sitio_1, "tabla sitio 1.csv")

#--------------------------------------------------------

tabla_sitio_1_independientes <- recordTable(inDir = fotos_sitio_1_renombradas,
IDfrom = "directory",
minDeltaTime = 60,
deltaTimeComparedTo = "lastRecord",
timeZone = "America/Mexico_City")

write.csv(tabla_sitio_1_independientes, "tabla sitio 1 independientes.csv")

View(tabla_sitio_1_independientes)

tabla_sitio_1 <- read.csv("tabla sitio 1.csv", header = T)

tabla_sitio_1_independientes <- read.csv("tabla sitio 1 independientes.csv", header = T)

#---------------------------------------------------------

fotos_venado_sitio_1 <- file.path("Venado")

tabla_venado_individuos <- recordTableIndividual(inDir = fotos_venado_sitio_1,
IDfrom = "directory",
minDeltaTime = 60,
deltaTimeComparedTo = "lastIndependentRecord",
metadataIDTag = "individuo", 
hasStationFolders = TRUE,
timeZone = "America/Mexico_City")

write.csv(tabla_venado_individuos, "tabla venado.csv")

#----------------------------------------

sitio_1 <- read.csv("actividad de estaciones_1_cam.csv", header = T)

jpeg(filename = "Figura_7.jpg", width = 2400, height = 1980, units = "px", res = 300)

mapa_riqueza_estacion <- detectionMaps(CTtable     = sitio_1,
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

#-------------------------------------------------

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

zorra <- "Zorra" 

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

conejo <- "Conejo" 

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



reporte_sitio_1 [[1]]

reporte_sitio_1 [[2]]

reporte_sitio_1 [[3]]

reporte_sitio_1 [[4]]

#--------------------------------------------------------

cam_activas_sitio_1 <- cameraOperation(CTtable = sitio_1,
stationCol = "Station",
setupCol = "Fecha_colocación",
retrievalCol = "Fecha_retiro",
hasProblems = T,
dateFormat = "%d/%m/%Y")

View(cam_activas_sitio_1)

# operación de las cámaras:

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


#crear la tabla

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


#En occasionLength se debe especificar el número de días de cada ocasión de detecciÃ³n, es decir "1" darÃ¡ la detecciÃ³n del venado en cada dÃ­a de actividad de la cÃ¡mara y "10" agruparÃ¡ las detecciones de venado cada diez dÃ­as

write.csv(hist_captura_venado, "historia captura venado.csv")

#----------------------------------------------------

