#######################################################################
# Objetivo: Revisar metricas estadisticas de los set de datos considerados
# MODIS, MAIAC, C6.0, C6.1, latam vs USA, todas las ventanas
# Hacer algunas pruebas de plots interesantes de como varian las
# metricas segun las ventanas
######################################################################

#Filtrar info de interes
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "latam",]
metricas <- metricas[metricas$temporal == "60",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
unique(metricas$metrica)
metrica_interes <- "r2"   
metrica_interes <- "r2 promedio espacial"   

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

#Plots varios
plot_1 <- ggplot(metricas_subset, aes(x = espacial , y = valor, color =estacion )) + 
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#3f007d")) +
  scale_shape_manual(values = c(15,16,17,18)) +  # Personaliza las formas (shapes)
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(x = "Spatial Window",
       y = expression(R^2),
       color = "Spatial window",
       shape = "Temporal window") + theme_classic()+  theme(legend.title = element_text(family = "Roboto",
                                                                                   
                                                                                   size = 14,
                                                                                   face = 2))+
  theme(legend.position = "none")  # Elimina la leyenda
  #theme(legend.text = element_text(size =12))
  #theme_minimal()

#guardar plot
ggsave("D:/Josefina/paper_git/paper_maiac/plot/V04/MAIAC-C61-Latam-R2_v03.png",a,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)




# Otro plot
# Ajustar el código para asignar los colores manualmente y reducir el ancho de los boxplots
plot_2 <- ggplot(metricas_subset, aes(x = estacion, y = valor, color = estacion)) +
  # Boxplot con reducción del ancho
  geom_boxplot(alpha = 0.6, outlier.shape = 16, outlier.colour = "black", width = 0.4) +  # 'width' controla el ancho del boxplot
  
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#3f007d")) +
  
  # Personalizacion de los ejes
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  
  # Etiquetas
  labs(
    x = "Station",
    y = expression(R^2),
    color = "Ventana espacial"
  ) +
  
  # Tema y personalización
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los numeros de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none"  # Elimina la leyenda
  )

# Mostrar grafico
print(a)
# Se guarda
ggsave("D:/Josefina/paper_git/paper_maiac/plot/V04/MAIAC-C61-Latam-R2_v02.png",a,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)

################################################################################
plot_3<- 
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(alpha=0.2,shape=c("o","v","s","P","*","h")) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(
    x = "Spatial window (km)",
    y = expression(R^2),
    color = "Station",
    size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(family = "Roboto",
                                     
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


################################################################################
plot_4 <- ggplot(metricas_subset, aes(y = valor, x = temporal, size = espacial, color = estacion)) +
  geom_point(shape = 20, alpha = 0.3, stroke = 0.5) +
  
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  labs(
    x = "Temporal window (min)",
    y = expression(R^2),
    color = "Station",
    size = "Spatial window"
  ) + 
  theme_classic() + theme(legend.position = "none")  # Elimina la leyenda

ggsave("D:/Josefina/paper_git/paper_maiac/plot/V03/MAIAC-C61-Latam-R2-TemporalWindow.png",
       f,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)

################################################################################
#Unir varios plots
plot_arrange <- grid.arrange(plot_1, plot_2,
                          plot_3, plot_4,ncol=2)

ggsave("./buff_R01_C61-USA.png",plot_arrange,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)



############################################################
##                     RMSE
############################################################
metrica_interes <- "rmse promedio espacial"  
metrica_interes <-"rmse" 

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]


plot_rmse_1<- ggplot(metricas_subset, aes(x =espacial , y = valor, color = estacion ))+
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#3f007d")) +
  scale_y_continuous(limits=c(0, 0.14),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14))+
  labs(
    x = "Spatial Windows",
    y = "RMSE",
    color = "Spatial window")+
  theme_classic()+  theme(legend.title = element_text(family = "Roboto",
                                                      size = 14,face = 2))+
  theme(legend.position = "none") 



ggsave("/MAIAC-C61-Latam-RMSE_V03.png",plot_rmse_1,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)


################################################################################

plot_rmse_2<- ggplot(metricas_subset, aes(x = estacion, y = valor, color = estacion))+
  geom_boxplot(alpha = 0.6, outlier.shape = 16, outlier.colour = "black", width = 0.4) +
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#3f007d")) +
# Probar escalas
  scale_y_continuous(limits=c(0, 0.14),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14))+
  # Etiquetas
  labs(
    x = "Station",
    y = "RMSE",
    color = "Ventana espacial"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),  # Agrandar los números de los ticks
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none"  # Elimina la leyenda
  )

# Mostrar gráfico
print(plot_rmse_2)

ggsave("/MAIAC-C61-Latam-RMSE_V02.png",plot_rmse_2,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)



################################################################################
plot_rmse_3 <- 
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 0.26),breaks = c(0,0.04,0.08,0.12,0.16,0.2,
                                                   0.24))+
  
  labs(title = "MAIAC C6.1 USA",
    x = "Spatial window (km)",
    y = "RMSE",
    color = "Station",
    size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


###############################################################################
plot_rmse_4 <- 
  ggplot(metricas_subset, aes(y = valor, x = temporal,  size =espacial,color=estacion )) +
  geom_point(shape=20,alpha=0.3, stroke = 0.2) +
  scale_y_continuous(limits=c(0, 0.14),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.14))+
  labs(
       x = "Temporal window (min)",
    y = "RMSE",
    color = "Station",
    size = "Spatial window") + theme_bw()+
  theme(legend.title = element_text(
                                    
                                    size = 14,
                                    face = 2))+
  theme_classic() + theme(legend.position = "none")

#####
plot_rmse_arrange <- grid.arrange(plot_rmse_1, plot_rmse_2,plot_rmse_3, ncol=2)
ggsave("MAIAC-C61-Latam-RMSE-TemporalWindow.png",plot_rmse_arrange,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)


############################################################
##                     BIAS
############################################################

metrica_interes <-"bias" 
metrica_interes <-"bias promedio espacial" 

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

plot_bias_1<- ggplot(metricas_subset, aes(x = espacial, y = valor,  color = estacion )) +
  geom_point(alpha=0.6, fill="black",size=3) +
  
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#3f007d")) +
  scale_shape_manual(values = c(15,16,17,18)) +  
  scale_y_continuous(limits=c(-0.07, 0.1))+
  
  labs(
    x = "Spatial Windows",
    y = "Bias",
    color = "Spatial window (km)",
    shape = "Temporal window") + theme_classic()+  theme(legend.title = element_text(
      size = 14,
      face = 2))+
  theme(legend.position = "none")

plot_bias_1

ggsave("MAIAC-C61-Latam-Bias_V03.png",plot_bias_1,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)

################################################################################
plot_bias_2<- ggplot(metricas_subset, aes(x = estacion, y = valor, color = estacion))+
  geom_boxplot(alpha = 0.6, outlier.shape = 16, outlier.colour = "black", width = 0.4) +
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#3f007d")) +

  scale_y_continuous(limits=c(-0.07, 0.1))+#
  
  # Etiquetas
  labs(
    x = "Station",
    y = "Bias",
    color = "Ventana espacial"
  ) +

  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11), 
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none" 
  )

# Mostrar gráfico
print(plot_bias_2)


ggsave("MAIAC-C61-Latam-Bias_V02.png",plot_bias_2,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)

################################################################################
plot_bias_3 <- 
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(-0.08, 0.18),breaks = c(-0.08,-0.04,0,0.04,0.08,0.12,0.16))+
  labs(title = "MAIAC C6.0 Latam",
       x = "Spatial window (km)",
    y = "Bias",
    color = "Station",
    size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))

###############################################################################
plot_bias_3<- #
  ggplot(metricas_subset, aes(y = valor, x = temporal,  size =espacial,color=estacion )) +
  geom_point(shape=20,alpha=0.3,stroke = 0.2) +
  scale_y_continuous(limits=c(-0.07, 0.1))+
  labs(
    x = "Temporal window (min)",
    y = "Bias",
    color = "Station",
    size = "Spatial window (km)") + 
  theme_classic()+
  theme(legend.text = element_text(size =12))+
  theme(
    legend.title = element_text(size = 14, face = 2),
    legend.text = element_text(size = 12)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5))  
  )

# Merge de plots
plot_bias_arrange <- grid.arrange(plot_bias_1, plot_bias_2,
                                  plot_bias_3, ncol=2)
ggsave("MAIAC-C61-Latam-Bias-TemporalWindow_Leyenda.png",plot_bias_arrange,
       width = 10,
       height = 12,
       units = "cm",
       dpi = 500)


############################################################
##                     N
############################################################

metrica_interes <-"n" 

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

plot_n_1<- ggplot(metricas_subset, aes(x = estacion, y = valor, shape = temporal, color = espacial)) +
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd","#756bb1")) + 
  scale_shape_manual(values = c(15,16,17,18)) +
  
  scale_y_continuous(limits=c(0,2000),breaks = c(0,400,800,1200,1600,2000))+
  labs(title = "Collection C61 USA",
    x = "Station",
    y = "Number",
    color = "Spatial window",
    shape = "Temporal window") + theme_bw()+  theme(legend.title = element_text(
      
      size = 14,
      face = 2))+
  theme(legend.text = element_text(size =12))
plot_n_1

ggsave("/buff_Num_C60.png",plot_n_1,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)
############################################################
##                     REU
############################################################

# Distintas variables
metrica_interes <-"reu" 
metrica_interes <-"reu promedio espacial" 
metrica_interes <-"reuMeanAOD promedio espacial" 
metrica_interes <-"reuMeanAOD" 

#Filtramos solo la metrica de interes
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

plot_reu_1<- ggplot(metricas_subset, aes(x = espacial, y = valor, color = estacion )) +
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#3f007d")) +
  scale_shape_manual(values = c(15,16,17,18)) + 
  scale_y_continuous(limits=c(0, 300),breaks = c(0,40,80,120,160,200,240,280))+
  labs(
       x = "Spatial Windows",
       y = "REU",
       color = "Spatial window",
       shape = "Temporal window") +theme_bw()+  theme(legend.title = element_text(
         size = 14,
         face = 2))+
  theme(legend.text = element_text(size =12))+
theme_classic()+theme(legend.position = "none")

plot_reu_1
ggsave("MAIAC-C61-Latam-REU.png",plot_reu_1,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)


##############################################################################
plot_reu_2<- ggplot(metricas_subset, aes(x = estacion, y = valor, color = estacion))+

  geom_boxplot(alpha = 0.6, outlier.shape = 16, outlier.colour = "black", width = 0.4) +  
  
  scale_color_manual(values = c("#005a32", "#fd8d3c","#99000d","#023858","#ce1256","#3f007d")) +
  scale_y_continuous(limits=c(0, 300),breaks = c(0,40,80,120,160,200,240,280))+
  labs(
    x = "Station",
    y = "REUx",
    color = "Ventana espacial"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11), 
    legend.title = element_text(family = "Roboto", size = 12, face = 2),
    legend.position = "none"  # Elimina la leyenda
  )


ggsave("MAIAC-C61-Latam-REU_v02.png",plot_reu_1,
       width = 10,
       height = 8,
       units = "cm",
       dpi = 500)

###############################################################################

plot_reu_2<- 
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 140),breaks = c(0,20,40,60,80,100,120,140))+
  labs(title = "Collection C6.1 Latam",
       x = "Spatial window (km)",
       y = "REU",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


################################################################################
plot_reu_2 <-
  ggplot(metricas_subset, aes(y = valor, x = temporal,  size =espacial,color=estacion )) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 140),breaks = c(0,20,40,60,80,100,120,140))+
  labs(title = "MODIS USA",
       x = "Temporal window (min)",
       y = "REUx",
       color = "Station",
       size = "Spatial window") + theme_bw()+
  theme(legend.title = element_text(
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size =12))

##################################
# Merge del plots
plot_reu_arrange <- grid.arrange(plot_reu_1, plot_reu_2, ncol=2)
# Guardar
ggsave("buff_RMSE01_C61_USA.png",plot_reu_arrange,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)



############################################################
##                     NRMSE
#############################################################

metrica_interes <-"nrmse" 

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

plot_nrmse_1<- ggplot(metricas_subset, aes(x = estacion, y = valor, shape = temporal, color = espacial)) +
  geom_point(alpha=0.6, fill="black",size=3) +
  scale_color_manual(values = c("#c51b8a","#2ca25f", "#de2d26", "#3182bd","#756bb1")) +  
  scale_shape_manual(values = c(15,16,17,18)) +  
  scale_y_continuous(limits=c(0, 1.21),breaks = c(0,0.20,0.40,0.60,0.80,1.00,1.2))+
  labs(title = "Collection C61 USA",
       x = "Station",
       y = "NRMSE",
       color = "Spatial window",
       shape = "Temporal window") + theme_bw()+  theme(legend.title = element_text(
         
         size = 14,
         face = 2))+
  theme(legend.text = element_text(size =12))
plot_nrmse_1

#############################################################################
plot_nrmse_2 <- 
  ggplot(metricas_subset, aes(y = valor, x = espacial, size = temporal, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1.21),breaks = c(0,0.20,0.40,0.60,0.80,1.00,1.2))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = "NRMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


###############################################################################
plot_nrmse_3 <-
  ggplot(metricas_subset, aes(y = valor, x = temporal,  size =espacial,color=estacion )) +
  geom_point(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1.21),breaks = c(0,0.20,0.40,0.60,0.80,1.00,1.2))+
  labs(title = "Collection C60 USA",
       x = "Temporal window (min)",
       y = "NRMSE",
       color = "Station",
       size = "Spatial window") + theme_bw()+
  theme(legend.title = element_text(
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size =12))


################################################################################

plot_nrmse_arrange <- grid.arrange(plot_nrmse_1, plot_nrmse_2,ncol=2)

ggsave("/buff_RMSE01_C61_USA.png",plot_nrmse_arrange,width = 30,
       height = 10,
       units = "cm",
       dpi = 500)


###############################################################################
##                        r2 promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "MODIS",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "r2 promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

R2_latam_60<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "MODIS USA",
       x = "Spatial window (km)",
       y = expression(R^2),
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
                                    
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


R2_latam_60
###############################################################################
##                        r2 promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "r2 promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

R2_latam_61<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "Collection C61 Latam",
       x = "Spatial window (km)",
       y = expression(R^2),
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
                                    
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


R2_latam_61

###############################################################################
##                        r2 promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "r2 promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

R2_USA_61<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "Collection C61 USA",
       x = "Spatial window (km)",
       y = expression(R^2),
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
                                    
                                    size = 14,
                                    face = 2))+
  theme(legend.text = element_text(size = 12))


R2_USA_61

###############################################################################
##                        r2 promedio espacial eu 60
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "r2 promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

R2_USA_60<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = expression(R^2),
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


R2_USA_60
##################################################################################
###############################################################################
##                        RMSE promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "latam",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "rmse promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

rmse_latam_60<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  #scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  labs(title = "MODIS Latam",
       x = "Spatial window (km)",
       y = "RMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


rmse_latam_60
###############################################################################
##                        RMSE promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "rmse promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

rmse_latam_61<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  labs(title = "Collection C61 Latam",
       x = "Spatial window (km)",
       y = "RMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


rmse_latam_61

###############################################################################
##                        rmse promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "rmse promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

rmse_USA_61<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  labs(title = "Collection C61 USA",
       x = "Spatial window (km)",
       y = "RMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


rmse_USA_61

###############################################################################
##                        rmse promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "rmse promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

rmse_USA_60<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 0.125),breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = "RMSE",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


rmse_USA_60

##################################################################################
###############################################################################
##                       BIAS  promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "latam",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "bias promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

bias_latam_60<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  #scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  labs(title = "MODIS Latam",
       x = "Spatial window (km)",
       y = "Bias",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


bias_latam_60
###############################################################################
##                       bias  promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "bias promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

bias_latam_61<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  labs(title = "Collection C61 Latam",
       x = "Spatial window (km)",
       y = "Bias",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


bias_latam_61

###############################################################################
##                        bias promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "bias promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

bias_USA_61<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  labs(title = "Collection C61 USA",
       x = "Spatial window (km)",
       y = "Bias",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


bias_USA_61

###############################################################################
##                        bias promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "bias promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

bias_USA_60<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(-0.07, 0.07),breaks = c(-0.7,-0.06,-0.04,-0.02,0,0.02,0.04,0.06))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = "Bias",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


bias_USA_60
##################################################################################
###############################################################################
##                       reux  promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "latam",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "reu promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

reux_latam_60<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 100),breaks = c(0,20,40,60,80,100))+
  labs(title = "MODIS Latam",
       x = "Spatial window (km)",
       y = "REUx",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


reux_latam_60
###############################################################################
##                       reux  promedio espacial LATAM 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]
metricas <- metricas[metricas$region == "latam",]
metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "reu promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

reux_latam_61<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 100),breaks = c(0,20,40,60,80,100))+
  labs(title = "Collection C61 Latam",
       x = "Spatial window (km)",
       y = "REUx",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


reux_latam_61

###############################################################################
##                        reux promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C61",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "reu promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

reux_USA_61<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 100),breaks = c(0,20,40,60,80,100))+
  labs(title = "Collection C61 USA",
       x = "Spatial window (km)",
       y = "REUx",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


reux_USA_61

###############################################################################
##                        reux promedio espacial eu 61
###############################################################################
metricas <- read_csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/final/metricas.csv")
metricas <- metricas[metricas$collection == "C60",]

metricas <- metricas[metricas$region == "USA",]

metricas$temporal <- factor(metricas$temporal)
metricas$espacial <- factor(metricas$espacial)
metrica_interes <- "reu promedio espacial"

#Len 120 6 ciudades * 5 buffer espaciales * 4 buff temporal
metricas_subset <- metricas[metricas$metrica == metrica_interes,]

reux_USA_60<- # Crea el gr?fico de dispersi?n con ggplot2
  ggplot(metricas_subset, aes(y = valor, x = espacial, group=estacion, color=estacion)) +
  geom_point(shape=1,stroke = 0.2) +
  #geom_ribbon(metricas_subset,mapping=aes(ymin = min, ymax = max,stroke=1, fill=estacion),alpha = 0.2, linetype = "dotted", size =0.1, show.legend = FALSE)+ # Banda sombreada
  geom_line(shape=1,stroke = 0.2) +
  scale_y_continuous(limits=c(0, 100),breaks = c(0,20,40,60,80,100))+
  labs(title = "Collection C60 USA",
       x = "Spatial window (km)",
       y = "REUx",
       color = "Station",
       size = "Temporal window") + theme_bw()+
  theme(legend.title = element_text(#family = "Roboto",
    
    size = 14,
    face = 2))+
  theme(legend.text = element_text(size = 12))


reux_USA_60

############################################################
## Definir colores y formas para cada estaci?n
colores <- c('#74c476', '#fed976', '#fb6a4a', '#74a9cf', '#df65b0', '#807dba')
formas <- c(1, 1, 2, 5, 6, 23)
metricas_subset_r2 <- metricas[metricas$metrica == "r2",]

r2<-ggplot(metricas_subset_r2, aes(y = valor, x = espacial, size = temporal, shape = estacion)) +
  geom_point(aes(color = estacion),stroke=0.5) +  # Aqu? especificamos fill dentro de aes() y tambi?n dentro de geom_point()
  scale_shape_manual(values = formas) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +  # Personaliza los colores
  labs(x = "Spatial window (km)", y = expression(R^2), fill = "Station", size = "Temporal window") +
  theme_bw() +scale_y_continuous(limits=c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 19.5),
        axis.title.x = element_text(size = 19.5),
        legend.title = element_text(size = 20, face = 2),
        legend.text = element_text(size = 20))#)     # Elimina todas las leyendas
  # theme(legend.title = element_text(size = 14, face = 2),
  #       legend.text = element_text(size = 12),legend.title=none)+ 
  
r2
metricas_subset_rmse <- metricas[metricas$metrica == "rmse",]
rmse<-ggplot(metricas_subset_rmse, aes(y = valor, x = espacial, size = temporal, shape = estacion)) +
  geom_point(aes(color = estacion),stroke=0.5) +  # Aqu? especificamos fill dentro de aes() y tambi?n dentro de geom_point()
  scale_shape_manual(values = formas) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +  # Personaliza los colores
  labs(x = "Spatial window (km)", y = "RMSE", fill = "Station", size = "Temporal window") +
  theme_bw() +scale_y_continuous(limits=c(0, 0.25),breaks = c(0,0.04,0.08,0.12,0.16,0.2,0.24))+
  
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 19.5),
        axis.title.x = element_text(size = 19.5),
        legend.title = element_text(size = 20, face = 2),
        legend.text = element_text(size = 20))#)    # Elimina todas las leyendas
# theme(legend.title = element_text(size = 14, face = 2),
#       legend.text = element_text(size = 12),legend.title=none)+ 
rmse

metricas_subset_bias <- metricas[metricas$metrica == "bias",]
bias<-ggplot(metricas_subset_bias, aes(y = valor, x = espacial, size = temporal, shape = estacion)) +
  geom_point(aes(color = estacion),stroke=0.5) +  # Aqu? especificamos fill dentro de aes() y tambi?n dentro de geom_point()
  scale_shape_manual(values = formas) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +  # Personaliza los colores
  labs(x = "Spatial window (km)", y = "Bias", fill = "Station", size = "Temporal window") +
  theme_bw() +scale_y_continuous(limits=c(-0.08, 0.18),breaks = c(-0.08,-0.04,0,0.04,0.08,0.12,0.16,0.2))+
  
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 19.5),
        axis.title.x = element_text(size = 19.5),
        legend.title = element_text(size = 20, face = 2),
        legend.text = element_text(size = 20))#)    # Elimina todas las leyendas
# theme(legend.title = element_text(size = 14, face = 2),
#       legend.text = element_text(size = 12),legend.title=none)+ 
bias
metricas_subset_REU <- metricas[metricas$metrica == "reu",]

metricas_subset_REU$Station <- metricas_subset_REU$estacion
reu<-ggplot(metricas_subset_REU, aes(y = valor, x = espacial, size = temporal, shape = Station)) +
  geom_point(aes(color = Station),stroke=0.5) +  # Aqu? especificamos fill dentro de aes() y tambi?n dentro de geom_point()
  scale_shape_manual(values = formas) +
  scale_color_manual(values = c("#74c476","#fed976","#fb6a4a", "#74a9cf","#df65b0","#807dba")) +  # Personaliza los colores
  labs(x = "Spatial window (km)", y = "REU", fill = "Station", size = "Temporal") +
  theme_bw() +scale_y_continuous(limits=c(0,100),breaks = c(0,20,40,60,80,100))+
  
  guides(
    color = guide_legend(override.aes = list(size = 5)),  # Ajusta el tama?o de los puntos en la leyenda
    shape = guide_legend(override.aes = list(size = 5))   # Ajusta el tama?o de las formas en la leyenda
  )+
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 19),
        axis.text.x = element_text(size = 19),
        axis.title.y = element_text(size = 19.5),
        axis.title.x = element_text(size = 19.5),
        legend.title = element_text(size = 20, face = 2),
               legend.text = element_text(size = 20))#) 
 # Elimina todas las leyendas
# theme(legend.title = element_text(size = 14, face = 2),
#       legend.text = element_text(size = 12),legend.title=none)+ 
reu
950 - 484
