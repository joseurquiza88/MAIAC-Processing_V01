#######################################################################
# Objetivo: calcular un promedio de las mediciones de AERONET para un
# intervalo de tiempo dado, centrado en el sobrevuelo del satélite, 
# con el fin de compararlo con el promedio de las recuperaciones de MAIAC
######################################################################

# Funcion para tomar el intervalo de tiempo dado +
# La funcion tiene como entrada:
# path_aeronet: directorio donde se encuentran los archivos AERONET procesados
# path_maiac : directorio donde se encuentran los archivos MAIAC procesados
# time_buffer: de acuerdo a la literatura el buffer puede ser 15 min - 30min - 60min - 90min - 120min


time_correlation <- function(path_aeronet,path_maiac,time_buffer,formato_fecha){

  # Abrir archivo en formato csv de AERONET
   data_aeronet <- read.csv(path_aeronet, header=TRUE, sep=",", dec=".", na.strings = "NA", stringsAsFactors = FALSE)
   # Formato fecha. Revisar bien antes de hacerlo
   data_aeronet$date <- as.POSIXct(strptime(data_aeronet$date, format = formato_fecha, "GMT"))
   # Abrir archivo en formato csv de MAIAC
   data_sat <- read.csv(path_maiac, header=TRUE, sep=",",dec=".", stringsAsFactors = FALSE, na.strings = "NA")
   #Descartar NAs que estan en la columna del AOD
   data_maiac <- data_sat[complete.cases(data_sat$AOD_055),]
   # Setear el formato de la fecha
   data_maiac$date  <- strptime(data_maiac$date, tz= "GMT", format = "%Y%j")
   data_maiac$hour  <- strptime(data_maiac$timestamp, tz= "GMT", format = "%Y%j%H%M")
  
  #DF vacios para el for
  MODIS_aeronet <- data.frame()
  AOD <- data.frame()

  for (i in 1: nrow(data_maiac)){ 
    if (i %% 50 == 0) {
      print (i)
    }
    #Se busca la concordancia día-mes-año entre AERONET y MAIAC
    # Hay otras formas de hacerlo, revisar y mejorar codigo
    table_aeronet<- data_aeronet 
    eq_year <- which(year(table_aeronet$date) == year(data_maiac[i,]$date))
    
    table_aeronet<- table_aeronet[eq_year,] 
    
    eq_month <- which(month(table_aeronet$date) == month(data_maiac[i,]$date))
    table_aeronet<- table_aeronet[eq_month,] 
    
    eq_day <- which(day(table_aeronet$date) == day(data_maiac[i,]$date))
    table_aeronet<- table_aeronet[eq_day,]
    dim_table <- dim(table_aeronet)
    
    if(dim_table[1] == 0){
      out_data <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA)   
      
    }else{ 
      #Si hay una coincidencia, se busca la ventana temporal de AERONET.
      table_dif <-data.frame()
      mach <- which(abs(difftime(table_aeronet$date, data_maiac[i,]$hour,units = "mins")) <time_buffer)
      
      
      table_dif <- table_aeronet[mach,]
      dim_table <- dim(table_dif)
      if(dim_table[1] == 0){  
        df <- data.frame()
        df <- data.frame(NA, NA,NA, NA, NA,NA,NA,NA,NA,NA,NA)
        names(df) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
        
      }else{
        #Se crea un archivo de salida con los datos co-localizados de MAIAC y AERONET.
        out_data <- data.frame(mean(table_dif[,5],  na.rm=TRUE),
                             median(table_dif[,5],  na.rm=TRUE),
                             sd(table_dif[,5], na.rm=TRUE), (dim_table[1]))
        names(out_data) <- c("mean", "mediana","sd","dim")
        df <- data.frame() 
        #Revisar info de interes en el df final
        #df <- data.frame(data_maiac[i,2],data_maiac[i,16], data_maiac[i,10:13], substr(table_dif[1,1],1,10),out_data[,1:4])
        df <- data.frame(data_maiac[i,2],data_maiac[i,16], data_maiac[i,10:13], substr(table_dif[1,1],1,10),out_data[,1:4])
        #df <- data.frame(data_maiac[i,1],data_maiac[i,15], data_maiac[i,9:12], substr(table_dif[1,1],1,10),out_data[,1:4])
        names(df) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      }
      AOD <- rbind(AOD, df)
      
      names(AOD) <- c("Date_MODIS","timestamp", "satellite","AOD_470","AOD_550_maiac","uncert", "date_AERO", "AOD_550_AER_mean","AOD_550_AER_median","AOD_550_AER_sd","AOD_550_AER_dim")
      AOD <- AOD[complete.cases(AOD),]
    }
  }
  return(AOD)
}


######     -------  Ejemplo para una estacion     -------  ######

buffer_time <- 120 #minutes
buffer_spatial <- "25km"
city <- "SP"

formato_fecha <- "%d/%m/%Y %H:%M"
data_maiac_BA <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/maiac/Latam_C61/",city,"/prueba_",buffer_spatial,"_",city,"_tot.csv", sep="")
#data_maiac_BA_60 <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/maiac/Latam_C60/BA/prueba_25km_BA_tot.csv"
data_maiac <- paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/maiac/Latam_C61/",city,"/prueba_",buffer_spatial,"_",city,"_C61_tot.csv",sep="")

data_aeronet <-"D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/datasets_interp_s_L02/USA/7_NY_2015-2022_interp-s_V02_L2.csv"
combinate_BA <- time_correlation (path_aeronet=data_aeronet,path_maiac=data_maiac,time_buffer=buffer_time,formato_fecha)
# Save the file with co-located data from AERONET and MAIAC on local path
write.csv (combinate_BA,paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C61/tot/",buffer_spatial,"/7_",city,"-",buffer_spatial,"-MAIAC-",buffer_time,"-AER_C61.csv",sep=""))
length(combinate_BA$Date_MODIS)
# BA
data_maiac_BA <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.0/BA-25KM-MAIAC.csv"
data_maiac_BA <- "D:/Josefina/paper_git/paper_maiac/datasets/maiac/C6.1/BA-25KM-MAIAC_C61.csv"
data_aeronet_BA <-"D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets_interp_s_v2/3_BA_2015-2022_interp-s.csv"
combinate_BA <- time_correlation (path_aeronet=data_aeronet_BA,path_maiac=data_maiac_BA,time_buffer=buffer_time)
# Save the file with co-located data from AERONET and MAIAC on local path
write.csv (combinate_BA,"D:/Josefina/paper_git/paper_maiac/datasets/processed/C6.0/tot/3_BA-25KM-MAIAC-60-AER.csv")


###############################################################################
###############################################################################
# Prueba para revisar promedios diarios de MAIAC-AERONET
promedios <- function(combinate){
  rbind_combinate <- data.frame()
  combinate$date <-   as.POSIXct(strptime(combinate$Date_MODIS, format = "%Y-%m-%d", "GMT"))
  combinate%>%
    group_by(date) %>%  
    group_split() ->combinate_group
  
  for (i in 1:length(combinate_group)){
    df <- data.frame( date = combinate_group[[i]][["date"]][1],
                      AOD_550_maiac_mean = mean(combinate_group[[i]][["AOD_550_maiac"]],na.rm=T),
                      AOD_550_AER_mean = mean(combinate_group[[i]][["AOD_550_AER_mean"]],na.rm=T))
    rbind_combinate <- rbind(rbind_combinate,df)
  }
  return(rbind_combinate)
  
}

buffer_time <- 120 #minutes
buffer_spatial <- "25km"
city <- "SC"
num_estacion <- 7
# BA
#SP
#combinate_SP <- read.csv("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/Latam_C61/tot/1_SP-25KM-MAIAC-60-AER.csv")
combinate <- read.csv(paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C61/tot/",buffer_spatial,"/",num_estacion ,"_",city ,"-",buffer_spatial ,"-MAIAC-",buffer_time ,"-AER_C61.csv",sep=""))

SP_com_promedios <- promedios(combinate)
write.csv(SP_com_promedios,paste("D:/Josefina/paper_git/paper_maiac/datasets/V02/processed/merge_AER-MAIAC/Latam_C61/dia/",buffer_spatial,"/",num_estacion,"_",city,"-",buffer_spatial,"-MAIAC-",buffer_time,"-AER_MEAN_C61.csv",sep=""))

