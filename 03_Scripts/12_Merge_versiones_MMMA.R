
#######################################################################
# Objetivo: hacer un merge diario entre MODIS-MAIAC C6.0/C6.1-AERONET C6.1
######################################################################



data_maiac <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61AER/1_SP-MAIAC-V6-61-AER_DIA.csv")
data_modis <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA-C61/1_SP-25KM-MM-60-AER-C61.csv")



#Merge with the column called "date"

merge_sat <- merge(x = data_maiac, y = data_modis, by = "date") # Equivalente

#Eliminate columns
# 
merge_sat <- data.frame(date = merge_sat$date,
                        AOD_modis = merge_sat$AOD_550_MODIS_mean,
                        AOD_maiac_60 = merge_sat$maiac_6,
                        AOD_maiac_61 = merge_sat$maiac_61,
                        AOD_550_AER_mean = merge_sat$AOD_550_AER_mean)


write.csv(merge_sat,"D:/Josefina/paper_git/paper_maiac/datasets/processed/M6M61-AER-MOD/1_SP-25KM-60-M6M61-AER-MOD.csv")

