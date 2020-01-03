###############################################################
# Mandujano, S. y L.A. Pérez-Solano. (Eds.). 2019. Fototrampeo en R: organización y análisis de datos. Volumen I. Instituto de Ecología A.C., Xalapa, Ver., México. 248 pp. ISBN: 978-607-7579-90-8
###############################################################
# CAPÍTULO 3
# Organización de fotos: camtrapR
# Eva López-Tello
###############################################################

library(camtrapR)

sitio_1 <- read.csv ("actividad de estaciones_1_cam.csv", header = TRUE)
head(sitio_1)

(fotos_sitio_1 <- file.path("Sitio_1"))

fotos_sitio_1_carpeta <- createStationFolders(inDir = fotos_sitio_1, stations = as.character(sitio_1$Station), createinDir = TRUE)

fotos_correccion_sitio_1 <- file.path("Corrección_sitio_1")

(tabla_correccion_sitio_1 <- read.csv("correccion de hora.csv", header = TRUE))

#fotos_sitio_1_corregidas <- timeShiftImages(inDir = fotos_correccion_sitio_1, timeShiftTable = tabla_correccion_sitio_1, stationCol = "Station", hasCameraFolders = FALSE, timeShiftColumn = "timeshift", timeShiftSignColumn = "sign", undo = F)

fotos_sitio_1_renombradas <- file.path("Sitio_1_R")

#imageRename(inDir = fotos_sitio_1, outDir = fotos_sitio_1_renombradas, hasCameraFolders = FALSE, keepCameraSubfolders = FALSE, copyImages = TRUE)

proyecto_institucion <- "Proyecto_"

addCopyrightTag(inDir = fotos_sitio_1_renombradas, copyrightTag = proyecto_institucion, askFirst = FALSE, keepJPG_original = F)

copyright <- exifTagNames(fotos_sitio_1_renombradas, returnMetadata = TRUE)

especies_sitio_1 <- c("Conejo", "Coyote", "Lince", "Pecarí", "Venado", "Zorra")

createSpeciesFolders(inDir = fotos_sitio_1_renombradas, species = especies_sitio_1, hasCameraFolders = FALSE, removeFolders = FALSE)

fotos_venado_sitio_1 <- file.path("Venado")
venado <- "Venado"

# getSpeciesImages(species = "venado", IDfrom = "directory", inDir = fotos_sitio_1_renombradas, outDir = fotos_venado_sitio_1, createStationSubfolders = TRUE)

fotos_sitio_1_renombradas <- file.path("Sitio_1_R")

tabla_sitio_1 <- recordTable(inDir = fotos_sitio_1_renombradas, IDfrom = "directory", timeZone = "America/Mexico_City", writecsv = FALSE)

tabla_sitio_1_independientes <- recordTable(inDir = fotos_sitio_1_renombradas, IDfrom = "directory", minDeltaTime = 60, deltaTimeComparedTo = "lastRecord", timeZone = "America/Mexico_City")

#write.csv(tabla_sitio_1, "tabla sitio 1.csv")
#write.csv(tabla_sitio_1_independientes, "tabla sitio 1 independientes.csv")
#read.csv("tabla sitio 1.csv", header = T)
#read.csv("tabla sitio 1 independientes.csv", header = T)

fotos_venado_sitio_1 <- file.path("Venado")

tabla_venado_individuos <- recordTableIndividual(inDir = fotos_venado_sitio_1, IDfrom = "directory", minDeltaTime = 60, deltaTimeComparedTo = "lastIndependentRecord", metadataIDTag = "individuo", hasStationFolders = TRUE, timeZone = "America/Mexico_City")

# write.csv(tabla_venado_individuos, "tabla venado.csv")

mapa_riqueza_estacion <- detectionMaps(CTtable = sitio_1,            recordTable = tabla_sitio_1_independientes, Xcol = "X", Ycol = "Y", stationCol= "Station", speciesCol = "Species", printLabels = TRUE, richnessPlot = TRUE, speciesPlots = FALSE, addLegend = TRUE)

mapa_lince <- detectionMaps(CTtable = sitio_1, recordTable = tabla_sitio_1_independientes, Xcol = "X", Ycol = "Y", stationCol = "Station",        speciesCol = "Species", speciesToShow = "Lince", printLabels = TRUE, richnessPlot = FALSE,  speciesPlots = TRUE, addLegend = TRUE)

zorra <- "Zorra"
activityDensity(recordTable = tabla_sitio_1_independientes, species = zorra, extend = NULL, xlab = "Hora", ylab = "Densidad", bty = "L")

activityHistogram(recordTable = tabla_sitio_1_independientes, species = zorra)

venado <- "Venado"
activityRadial(recordTable = tabla_sitio_1_independientes, species = venado, lwd = 3)

activityRadial(recordTable = tabla_sitio_1_independientes, species = venado, speciesCol = "Species", plotR = TRUE, lwd = 3, rp.type = "p")

zorra <- "Zorra"
conejo <- "Conejo"
activityOverlap(recordTable =tabla_sitio_1_independientes, speciesA = zorra, speciesB = conejo, writePNG = FALSE, plotR = TRUE, addLegend = TRUE, legendPosition = "topright", linecol = c("grey", "black"), linewidth = c(3,3), add.rug = TRUE, xlab = "Hora", ylab = "Densidad") 

reporte_sitio_1 <- surveyReport(recordTable = tabla_sitio_1_independientes, CTtable = sitio_1, speciesCol = "Species", stationCol = "Station", setupCol = "Fecha_colocación", retrievalCol = "Fecha_retiro", CTDateFormat = "%d/%m/%Y", recordDateTimeCol = "DateTimeOriginal", recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", CTHasProblems = T)

reporte_sitio_1 [[1]]

reporte_sitio_1 [[2]]

reporte_sitio_1 [[3]]

reporte_sitio_1 [[4]]

cam_activas_sitio_1 <- cameraOperation(CTtable = sitio_1, stationCol = "Station", setupCol = "Fecha_colocación", retrievalCol = "Fecha_retiro", hasProblems = T, dateFormat = "%d/%m/%Y")

camopPlot <- function(camOp){which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
at.tmp <- which.tmp / ncol(camOp)
image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = c("red", "grey70"))
axis(1, at = at.tmp, labels = label.tmp)
axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
abline(v = at.tmp, col = rgb(0,0,0, 0.2))
box()
}

camopPlot(camOp = cam_activas_sitio_1)

hist_captura_venado <- detectionHistory(recordTable = tabla_sitio_1_independientes, camOp = cam_activas_sitio_1, stationCo = "Station", speciesCol = "Species", recordDateTimeCol = "DateTimeOriginal", recordDateTimeFormat = "%Y-%m-%d", species = "Venado", occasionLength = 1, day1 = "station", includeEffort = FALSE, timeZone = "America/Mexico_City")
head(hist_captura_venado)

#write.csv(hist_captura_venado, "historia captura venado.csv")

########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 
