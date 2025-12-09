#######################################################################
# Objetivo: calcular un promedio de las mediciones de AERONET para un
# intervalo de tiempo dado, centrado en el sobrevuelo del satélite, 
# con el fin de compararlo con el promedio de las recuperaciones de MODIS
######################################################################

# Funcion para tomar el intervalo de tiempo dado +
# La funcion tiene como entrada:
# path_aeronet: directorio donde se encuentran los archivos AERONET procesados
# path_modis : directorio donde se encuentran los archivos MODIS procesados
# time_buffer: de acuerdo a la literatura el buffer puede ser 15 min - 30min - 60min - 90min - 120min

#Funcion similar a MAIAC


time_correlation <- function(path_aeronet,path_modis,time_buffer){
  # Abrir info de aeronet
  data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
  # Formato fecha, revisar bien antes de seguir
  data_aeronet$date <- as.POSIXct(strptime(data_aeronet$date, format = "%Y-%m-%d %H:%M", "GMT"))
  # Abrir info de MODIS
  data_sat <- read.csv(path_modis, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")
    
  #Descartar NAs
  data_modis <- data_sat  [complete.cases(data_sat$AOD),]
  # Formato fecha, revisar bien antes de seguir
  data_modis$date  <- strptime(data_modis$dia, tz= "GMT", format = "%d/%m/%Y")
  data_modis $timestamp <- paste( data_modis$dia, data_modis$hora, sep = " ")
  data_modis $hour <- strptime( data_modis$timestamp, tz= "GMT", format = "%d/%m/%Y %H:%M")
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
      #Si hay coincidencia se busca la ventana temporal de AERONET.
      table_dif <-data.frame()
      mach <- which(abs(difftime(table_aeronet$date, data_modis[i,]$hour,units = "mins")) <time_buffer)
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA)
        names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                       "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
      }else{
        ##Se crea un archivo de salida con los datos co-localizados de MODIS y AERONET.
        out_data <- data.frame(mean(table_dif[,5],  na.rm=TRUE),
                               median(table_dif[,5],  na.rm=TRUE),
                               sd(table_dif[,5], na.rm=TRUE), (dim_table[1]))
        names(out_data) <- c("mean", "mediana","sd","dim")
        df <- data.frame() 
        df <- data.frame(data_modis[i,10],data_modis[i,4], data_modis[i,8], substr(table_dif[1,1],1,10),out_data[,1:4])
        names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                            "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
        
      }
      AOD <- rbind(AOD, df)
      
      names(df) <- c("Date_MODIS", "AOD_550_modis", "satellite",
                     "Date_AERONET","AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")#, "AOT_550_2", "AOT_550_3")
      AOD <- AOD[complete.cases(AOD),]
    }
  }
  return(AOD)
}


######     -------  Ejemplo para una estacion     -------  ######

buffer_time <- 60 #minutes

#Cambiar directorio segun archivo
data_modis <- "D:/Josefina/papers_escritos/paper_maiac/datasets/modis/BA-25KM-MODIS.csv"
data_aerone <-"D:/Josefina/papers_escritos/paper_maiac/datasets/aeronet/datasets_interp_s/BA_2015-2022_interp-s.csv"
combinate <- time_correlation (path_aeronet=data_aeronet_BA,path_modis=data_modis_BA,time_buffer=buffer_time)
# Guardar el archivo con los datos co-localizados de AERONET y MODIS en la ruta local.
write.csv (combinate,"D:/Josefina/papers_escritos/paper_maiac/datasets/processed/BA-25KM-MODIS-60-AER.csv")


###############################################################################
###############################################################################
# Prueba para revisar promedios diarios de MODIS-AERONET

dire <- "D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_tot/" 
# directorio donde estan los archivos procesados
id <- dir(dire, pattern = ".csv")
setwd(dire) 
for (i in 1:length(id)){
  data_modis <- read.csv(id[i])
  data_modis %>%
    group_by(Date_MODIS) %>%  
    group_split() -> data
  df_rbind <- data.frame()
  for (p in 1:length(data)){ 
    df <- data.frame(date = data[[p]][["Date_MODIS"]][1],# only first date
                     AOD_modis = mean(data[[p]][["AOD_550_modis"]],na.rm=T),
                     AOD_aeronet = mean(data[[p]][["AOD_550_AER_sd"]],na.rm=T))
    df_rbind <- rbind(df_rbind ,df)
  }
  print(substr(id[i],1,20))
  name <- paste("D:/Josefina/paper_git/paper_maiac/datasets/processed/MODIS/MODIS_dia/",substr(id[i],1,20),"-DIA.csv",sep = "")
  write.csv(df_rbind,name)
}

