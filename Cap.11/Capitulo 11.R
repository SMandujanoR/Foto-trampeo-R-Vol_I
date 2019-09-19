########################################################
# CAPÍTULO 11
# Uso de bebederos: ggplot2, agricolae y effects
########################################################

library(ggplot2)
library(agricolae)
library(effects)

datos <- read.csv("Cap.11/datos.csv", header = TRUE)

Tasa_visitas <- with(datos, round((Eventos/Esfuerzo)*100, 2))
#write.csv(Tasa_visitas, "Cap.11/datos_2.csv")

datos_2 <- read.csv("Cap.11/datos_2.csv", header = TRUE)

#Fig. 11.2
ggplot(datos_2, aes(x = Especies , y = Tasa , fill = factor(Año), color = factor(Año))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("green", "royalblue", "red")) +
  scale_color_manual(values = c("green4", "royalblue4", "red4")) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(legend.position =  c(0.1, 0.8)) + 
  ggtitle("") + 
  theme(axis.text.x = element_text(angle = 90, face="bold.italic", hjust = 1, size=rel(1.7))) +
  theme(plot.title = element_text(hjust = 0.5, size = rel(2.5))) +
  labs(x = "", y = "Tasa de visitas") +
  labs(fill = "Año", color = "Año") +
  theme(axis.title.x = element_text(size = rel(1.75))) +
  theme(axis.title.y = element_text(size = rel(1.75))) +
  theme(legend.title = element_text(size = rel(1.75))) +
  theme(legend.text = element_text(size = rel(1.5)))


#Fig. 11.3
ggplot(datos_2, aes(fill = Especies, y = Tasa, x = Camara)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Año) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.position =  "right") +
  ggtitle("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=rel(1.5))) +
  theme(plot.title = element_text(hjust = 0.5, size=rel(2.5)))+
  labs(x = "Bebedero", y = "Tasa de visitas") +
  theme(axis.title.x = element_text(size = rel(1.75))) +
  theme(axis.title.y = element_text(size = rel(1.75))) +
  theme(legend.title = element_text(size = rel(1.5))) +
  theme(legend.text = element_text(face = "italic", 
                                   size = rel(1)))

anova <- with(datos_2, aov(Tasa ~ Especies))
summary(anova)

A_2018 <- datos_2[datos_2$Año == "2018",]
anova_18 <- with(A_2018, aov(Tasa ~ Especies))
summary(anova_18)

out <- HSD.test(anova_18, "Especies", alpha = 0.05)
out$groups
out <- HSD.test(anova_18, "Especies", alpha = 0.05)

#Fig. 11.4
par(mfcol = c(1,1), mar = c(9,5,3,3))
bar.group(out$groups, horiz = F, las = 3, cex.main = 2, font = 3, cex.axis = 0.9, plot = T, col = "lightblue", ylim = c(0,60), names.arg = out$trt, ylab = "Tasa de visitas",  main = "", cex.lab = 2)

Lince <- datos_2[datos_2$Especies == "L. rufus",] 
anova_L.r1 <- with(Lince, aov(Tasa ~ Año))
summary(anova_L.r1)

anova_L.r3 <- with(Lince, aov(Tasa ~ Camara)) 
summary(anova_L.r3)

S.a_14 <- datos_2[datos_2$Especies == "S. angustifrons" & datos_2$Año == "2014",]

Venado <-read.csv("Cap.11/Datos_venado.csv", header=TRUE)

cor(Venado[(7:10)])

D_rio <- with(Venado, scale(Rio)) 
T_max <- with(Venado, scale(T_max)) 
HR <- with(Venado, scale(HR))

Modelo1 <- with(Venado, glm(Visitas ~  Modelo + Año + Rio + T_max * HR, offset = log(Esfuerzo), family = poisson))
summary(Modelo1)

MejorModelo <- step(Modelo1)
summary(MejorModelo)

plot(allEffects(MejorModelo), main="")

########################################################
# FIN SCRIPT
rm(list = ls()) 
dev.off() 



