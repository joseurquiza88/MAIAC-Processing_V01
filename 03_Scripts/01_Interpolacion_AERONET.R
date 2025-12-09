###############################################################

# Objetivo: interpolar las mediciones de cada estación AERONET a 
# la longitud de onda proporcionada por MAIAC-MODIS 
# AOD 550 nm a traves de diferentes metodologias mencionadas 
# en la literatura
###############################################################


### Funcion para estimar AOD550 a partir del coeficiente de Ångström
path <- "./AERONET"
path_write="."

# Funcion
interpolate <- function(path,path_write){
  df <- data.frame()
  setwd(path)
  archive <- dir(path, pattern = ".csv")
  for(i in 1:length(archive)){
    print(i)
    # Leer todos los archivos que estan en el path (n=6)
    data <- read.csv(archive[i],  header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE, na.strings = "-999")
    #Poner la fecha en formato
    data$date <- strptime(paste(data$Date.dd.mm.yyyy., data$Time.hh.mm.ss., sep=" "), format="%d:%m:%Y %H:%M", tz="GMT") 
    # El nombre del archivo se toma del path.
    name <- substr(archive[i],1,12)
    
    # Calculation of alpha, SEGUN WENMIN QUIN 2021
    #data$alfa <- -(log(data$AOD_440nm/data$AOD_675nm) )/log(440/675)
    #data$aod_550 <- data$AOD_440nm * (550/440)**data$alfa
    # Calculation of AOD550
    #data$aod_550 <- data$AOD_675nm*(500/675)**(-(data$Angstrom_Exponent_440_675))# change name BO-COLUMN
    #Create a new dataframe
    # data$aod_550 <-  data$AOD_500nm * ((550/500)**(-data$Angstrom_Exponent_440_675))
    data$aod_550 <-  data$AOD_500nm * ((550/500)**(-data$X440.675_Angstrom_Exponent))
    
    df <- data.frame (data$date,data$aod_550)
    # Cambiar el nombre de las columnas
    names(df) <- c("date", "aod_550")
    # Se genera un nuevo dataset que se guarda en el "path_write"
    write.csv(df,(paste(path_write,name,"_interp.csv",sep="")) , row.names = FALSE)
    
  }
  
}
# Correr funcion
interpolate(path,path_write)


################################################################################
###   Función para estimar AOD550 a traves de la interpolacion cuadratica
# Se toman 3 puntos (3 longiudes de onda), basada en los polinomios de Lagrange.

interpolate_s <- function(path,path_write){
  df <- data.frame()
  # Archivos en el path local
  setwd(path)
  archive <- dir(path, pattern = ".csv")
  interpol_cuad <- function(x, x0, y0, x1, y1, x2, y2){ 
    a = ((x - x1)*(x-x2))/((x0-x1)*(x0-x2))
    b = ((x - x0)*(x-x2))/((x1-x0)*(x1-x2))
    c = ((x - x0)*(x-x1))/((x2-x0)*(x2-x1))
    y = (y0*a) + (y1*b) + (y2*c)
    return(y)
  }
  for(i in 1:length(archive)){
    print(i)
    # Leeer tidis kis archivos
    data <- read.csv(archive[i],  header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE, na.strings = "-999")
    #Poner la fecha en formato
    date <- strptime(paste(data$Date.dd.mm.yyyy., data$Time.hh.mm.ss., sep=" "), format="%d:%m:%Y %H:%M", tz="GMT") 
    # El nombre del archivo se toma del path.
    name <- substr(archive[i],1,12)
    
    #Valor desconocido
    x = log(550)
    # Primera interpolacion
    #Punto 1
    x0= log(440)
    y0= log(data$AOD_440nm)
    #Punto 2
    x1= log(500)    
    y1=log(data$AOD_500nm) 
    #Punto 3
    x2= log(675) #500        
    y2= log(data$AOD_675nm)
    # Funcion 1
    y <- exp(interpol_cuad(x, x0, y0, x1, y1, x2, y2))
    
    #Se genera dataframe con la primera interpolacion
    data_aeronet <- data.frame(date, y)
    names(data_aeronet) <- c("date", "AOD_550")
    
    # Segunda interpolacion
    x2= log(870)
    y2= log(data$AOD_870nm)
    # Agregar informacion en el dataframe
    data_aeronet$AOT_550_2  <- exp(interpol_cuad(x, x0, y0, x1, y1, x2, y2))
    
    #Tercera interpolacion
    x2= log(1020)
    y2= log(data$AOD_1020nm)
    # Agregar informacion en el dataframe
    data_aeronet$AOT_550_3 <- exp(interpol_cuad(x, x0, y0, x1, y1, x2, y2))
    # Media de las 3 interpolaciones
    data_aeronet$AOT_550_mod <- rowMeans(data_aeronet[,2:4], na.rm = TRUE)
    # Setear el nombre de las columnas
    names(data_aeronet) <- c("date", "aod_550","aod_550_2","aod_550","aod_550_mod")
    # Guardar el dataframe en el path local
    write.csv(data_aeronet,(paste(path_write,name,"_interp-s.csv",sep="")) , row.names = FALSE)
    
  }
  
}

####################################################################
# Pruebas de uso
interpolate(path="D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets",path_write="D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp/")
interpolate_s (path="D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets",path_write="D:/Josefina/papers_escritos/MAIAC_paper/datasets/aeronet/datasets_interp_s_v2/")
interpolate_s (path="D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets",path_write="D:/Josefina/paper_git/paper_maiac/datasets/aeronet/datasets_interp_s_v2/")
interpolate (path="D:/Josefina/Proyectos/aeronet/datos/GSFC",path_write="D:/Josefina/Proyectos/aeronet/datos/GSFC/interpolado/")
interpolate_s (path="D:/Josefina/Proyectos/aeronet/datos/GSFC",path_write="D:/Josefina/Proyectos/aeronet/datos/GSFC/interpolado/")
interpolate_s (path="D:/Josefina/Proyectos/aeronet/datos/MD_Science_Center",path_write="D:/Josefina/Proyectos/aeronet/datos/MD_Science_Center/interpolado/")

# VERSION 02
interpolate(path="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/original",path_write="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/interpolate")
interpolate_s (path="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/original",path_write="D:/Josefina/Proyectos/aeronet/datos/AERONET_02112023_L02/interpolate/")


# Actualizacopn 2023-2024
path <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/periodoFaltante_2023-2024/data/"
path_write <- "D:/Josefina/paper_git/paper_maiac/datasets/V02/aeronet/periodoFaltante_2023-2024/proceed/"


