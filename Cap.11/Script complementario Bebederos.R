#############################################################
# Mandujano, S. y L.A. Pérez-Solano. (Eds.). 2019. Fototrampeo en R: organización y análisis de datos. Volumen I. Instituto de Ecología A.C., Xalapa, Ver., México. 248 pp. ISBN: 978-607-7579-90-8
########################################################
# Script complementario al capítulo
# Carlos Hernández-Gómez
########################################################

#Se cargan las librerias necesarias#
library(ggplot2) #Para los gráficos
library(agricolae)#Para la prueba HSD Tukey
library(effects)#Para los efectos de los modelos

#Se cargan los datos para calcular el índice#
datos <-read.csv("datos.csv", header=TRUE)
View(datos)#Para ver todos los datos
head(datos); tail(datos) #Para ver solo los primeros y últimos 6 filas

#Cálculo de la tasa de visita#
Tasa <- with(datos,round((Eventos/Esfuerzo)*100, 2))
tabla <- cbind(datos, Tasa)
write.csv(tabla, "datos_2.csv") #En caso de querer guardar un .csv con la tasa

#Para visualizar la tasa
#Todos lo escrito entre comillas se puede modificar, para cambiar los colores empleados, la posición del título, nombres de los ejes. También se puede cambiar el tamaño de los títulos y leyendas cambiando el argumento size.rel(), se puede modificar el eje y con el argumento ylim(), colocando el valor mínimo y máximo que tomará el eje, también se puede cambiar la posición de la etiqueta especificando las coordenadas dentro de legend.position =  c()#

#Se cargan los datos que ya incluyen la tasa de visita#
datos_2 <-read.csv("datos_2.csv", header=TRUE)

ggplot(datos_2, aes(x =Especies , y =Tasa , fill=factor(Año), color=factor(Año))) +
  geom_boxplot()+
  scale_fill_manual(values = c("green", "royalblue", "red"))+
  scale_color_manual(values=c("green4", "royalblue4", "red4"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position =  c(0.1, 0.8))+
  ggtitle("")+ #Para agregar un título al grágico
  theme(axis.text.x = element_text(angle = 90,  face="bold.italic", hjust = 1, size=rel(1.7)))+
  theme(plot.title = element_text(hjust = 0.5, size=rel(2.5)))+
  labs(x="", y="Visitas en 100 días")+
  ylim(0,60)+
  labs(fill="Año", color="Año")+
  theme(axis.title.x=element_text(size=rel(1.75)))+
  theme(axis.title.y=element_text(size=rel(1.75)))+
  theme(legend.title = element_text(size=rel(1.75)))+
  theme(legend.text = element_text(size=rel(1.5)))

#Para la gráfica por modelo de bebedero#
ggplot(datos_2, aes(x =Especies , y =Tasa , fill=factor(Modelo), color=factor(Modelo))) +
  geom_boxplot()+
  scale_fill_manual(values = c("royalblue", "red"))+
  scale_color_manual(values=c("royalblue4", "red4"))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position =  c(0.1, 0.8))+
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 90,  face="bold.italic", hjust = 1, size=rel(1.7)))+
  theme(plot.title = element_text(hjust = 0.5, size=rel(2.5)))+
  labs(x="", y="Visitas en 100 días")+
  ylim(0,50)+
  labs(fill="Modelo", color="Modelo")+
  theme(axis.title.x=element_text(size=rel(1.75)))+
  theme(axis.title.y=element_text(size=rel(1.75)))+
  theme(legend.title = element_text(size=rel(1.75)))+
  theme(legend.text = element_text(size=rel(1.5)))

#Para ver como se distribuyen las especies en cada bebedero#
ggplot(datos_2, aes(fill=Especies, y=Tasa, x=Camara))+
  geom_bar(stat="identity")+
  facet_wrap(~ Año)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position =  "right")+
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=rel(1.5)))+
  theme(plot.title = element_text(hjust = 0.5, size=rel(2.5)))+
  labs(x="Bebedero", y="Visitas")+
  theme(axis.title.x=element_text(size=rel(1.75)))+
  theme(axis.title.y=element_text(size=rel(1.75)))+
  theme(legend.title = element_text(size=rel(1.5)))+
  theme(legend.text = element_text(face="italic",size=rel(1)))

#Para los análisis estadísticos#

anova <-with(datos_2, aov(Tasa~Especies))#Prueba de ANOVA
summary(anova)

A_2018<-datos_2[datos_2$Año == "2018",] #Extraccion de datos para un año
anova_18 <-with(A_2018, aov(Tasa~Especies)) #Prueba de ANOVA usando solo ese año
summary(anova_18)

#Prueba HSD Tukey#
out <- HSD.test(anova_18, "Especies", alpha = 0.05)
out

#Para verlo de manera gráfica#
par(mfcol = c(1,1), mar = c(9,5,3,3))
bar.group(out$groups, horiz = F, las = 3, cex.main = 2, font = 3, cex.axis = 0.9, plot = T, col = "lightblue",ylim=c(0,60),names.arg = out$trt, ylab = "Tasa de Visitas", main="", cex.lab=2)

#Análisis estadísticos por modelo de bebedero#
anova_mod <-with(datos_2, aov(Tasa~Modelo))
summary(anova_mod)

anova_punto <-with(A_2018, aov(Tasa~Camara))#ANOVA para evaluar cada bebedero#
summary(anova_punto)

#Para realizar análisis para un especie en específico#
Lince<-datos_2[datos_2$Especies == "L. rufus",]#Extracción de datos de lince

anova_L.r1 <-with(Lince, aov(Tasa~Año))#Para buscar diferencias entre años
summary(anova_L.r1)

anova_L.r2 <-with(Lince, aov(Tasa~Modelo))#Para buscar diferencias entre modelos de bebedero
summary(anova_L.r2)

anova_L.r3 <-with(Lince, aov(Tasa~Camara))#Para buscar diferencias entre puntos
summary(anova_L.r3)

#Para extraer datos de año y especie#
S.a_14<-datos_2[datos_2$Especies == "S. angustifrons" & datos_2$Año == "2014",]


#Para los modelos lienales generalizados#

Venado <-read.csv("Datos_venado.csv", header=TRUE)#Se cargan los datos
cor(Venado[(7:10)])#Para buscar correlación entre las variables
#No se utilizan datos de distancia a bebedero más cercano por correlación con distancia a río#

#Estandarización de variables#
attach(Venado)
D_rio <-scale(Rio) #Distancia rio
T_max <-scale(T_max) #T_Max
HR <-scale(HR) #HR

#Primer modelo con todas las variables#
Modelo1 <- glm(Visitas ~  Modelo + Año + D_rio + T_max * HR, offset = log(Esfuerzo), family = poisson)
summary(Modelo1)

#Step backward#
MejorModelo <- step(Modelo1)
summary(MejorModelo)

#Grafica de efectos#
plot(allEffects(MejorModelo), main="")
plot(Effect(c("D_rio"), MejorModelo), main="")
