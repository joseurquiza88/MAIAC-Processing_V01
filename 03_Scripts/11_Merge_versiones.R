
#######################################################################
# Objetivo: hacer un merge diario entre MODIS-MAIAC-AERONET C6.1
######################################################################


###########                          ------ BA ------
data_modis_BA <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/BA-25KM-MODIS-60-AER-DIA.csv")
data_maiac_BA <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/3_BA-25KM-MAIAC-60-AER_MEAN.csv")

#Unir a partir de la columna llamda date

merge_sat <- merge(x = data_modis_BA, y = data_maiac_BA, by = "date") # Equivalente

#Eliminar columnas, dejar las necesarias
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_modis,
                        AOD_maiac = merge_sat$AOD_550_maiac_mean,
                        AOD_550_AER_mean = (merge_sat$AOD_550_AER_mean))

names(merge_sat) <- c("date", "AOD_550_MODIS_mean", "AOD_550_MAIAC_mean", "AOD_550_AER_mean")

write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/3_BA-25KM-MM-60-AER-C61.csv")



