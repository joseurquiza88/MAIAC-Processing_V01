
#######################################################################
# Objetivo: calcula un promedio de las mediciones de AERONET para un 
# intervalo de tiempo dado, centrado en el sobrevuelo del satélite, 
# con el fin de compararlo con el promedio de las recuperaciones de 
# MODIS-MAIAC-AERONET
#Similar a funciones anteriores pero aca unimos MAIAC con MODIS
######################################################################


#Dice path_aeronet pero es MAIAC

time_correlation_MMA <- function(path_aeronet,path_modis){
  
  # Abrir archivos AERONET/MAIAC
  data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
  # Formato de fecha, revisar antes de continuar
  #data_aeronet$date <- as.POSIXct(strptime(data_aeronet$timestamp, format = "%Y-%m-%d %H:%M", "GMT"))
  data_aeronet$date <- as.POSIXct(strptime(data_aeronet$timestamp, format = "%Y%j%H%M", "GMT"))
   # Abrir MAIAC
  data_sat <- read.csv(path_modis, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")

  #Remover NAs
  data_modis <- data_sat  [complete.cases(data_sat),]
  # Formato de fecha, revisar antes de continuar
  data_modis$date  <- strptime(data_modis$Date_MODIS, tz= "GMT", format = "%Y-%m-%d")
  data_modis<- data.frame(date = data_modis$date,
                          AOD_550 = data_modis$AOD_550_modis
  )
  #Info extra
  data_modis$hora_2  <- strptime( data_modis$date, tz= "GMT", format = "%Y-%m-%d")
  data_modis$date  <- strptime( data_modis$date, tz= "GMT", format = "%Y-%m-%d")
  MODIS_aeronet <- data.frame()
  AOD <- data.frame()
  
  for (i in 1: nrow(data_modis)){ 
    if (i %% 50 == 0) {
      print (i)
    }
    #Se busca la concordancia día-mes-año entre AERONET y MAIAC
    # Hay otras formas de hacerlo, revisar y mejorar codigo
    table_aeronet<- data_aeronet 
    eq_year <- which(year(table_aeronet$date) == year(data_modis[i,]$date))
    
    table_aeronet<- table_aeronet[eq_year,] 
    
    eq_month <- which(month(table_aeronet$date) == month(data_modis[i,]$date))
    table_aeronet<- table_aeronet[eq_month,] 
    
    eq_day <- which(day(table_aeronet$date) == day(data_modis[i,]$date))
    table_aeronet<- table_aeronet[eq_day,]
    dim_table <- dim(table_aeronet)
    
    if(dim_table[1] == 0){
      out_data <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA)   
      
    }else{ 
      #Si hay una coincidencia, se busca la ventana temporal de AERONET.
      table_dif <-data.frame()
      
      
      mach <- which(abs(difftime(table_aeronet$date, data_modis[i,]$hora_2,units = "days")) <1)
      
      
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA)
        names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                       "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
      }else{
        #Se crea un archivo de salida con los datos co-localizados de MAIAC, MODIS y AERONET.
        out_data <- data.frame(mean(table_dif[,6],  na.rm=TRUE),mean(table_dif[,9],  na.rm=TRUE),
                              #median(tabla_dif[,5],  na.rm=TRUE),
                              #sd(tabla_dif[,5], na.rm=TRUE)
                              (dim_table[1]))
        names(out_data) <- c("AOD_550_MAIAC_mean", "AOD_550_AER_mean","dim")
        df <- data.frame() 

        # Info completa
        df<- data.frame(data_modis[i,1:2], out_data[,1:3])
        names(df) <- c("date","AOD_550_MODIS_mean","AOD_550_MAIAC_mean", "AOD_550_AER_mean","dim")
        
      }
      AOD <- rbind(AOD, df)
      
      names(AOD) <- c("date","AOD_550_MODIS_mean","AOD_550_MAIAC_mean", "AOD_550_AER_mean","dim")
      
    }
  }
  
  return(AOD)
}


######     -------  Ejemplo para una estacion     -------  ######

#C6.1, Daily mean C6.1
data_modis_BA <- "D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/BA-25KM-MODIS-60-AER-DIA.csv.csv"
data_aeronet_BA <-"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.1/dia/3_BA-25KM-MAIAC-60-AER_MEAN.csv"
combinate_BA <- time_correlation_MMA (path_aeronet=data_aeronet_BA,path_modis=data_modis_BA)
# Save the file with co-located data from AERONET and modis on local path
write.csv (combinate_BA,"D:/Josefina/paper_git/paper_maiac/datasets/processed/MMA/BA-25KM-MM-60-AER.csv")
