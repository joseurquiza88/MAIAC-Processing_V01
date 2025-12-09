
#######################################################################
# Objetivo: conteo de categorias land cover segun las estaciones de interes
######################################################################


# -----------------------------
# Parametros
# -----------------------------
num_categorias <- 17

dist_buffer_m <- 1000  
dist_buffer_m <- 1500
dist_buffer_m <- 2000
dist_buffer_m <- 1200 
dist_buffer_m <- 3000  
dist_buffer_m <- 7500
dist_buffer_m <- 15000 
dist_buffer_m <- 125000 
dist_buffer_m <- 25000 
directorio_tiff <- "D:/Josefina/Proyectos/LandCover/data/GeoTiff/"
raster_tiff <- raster(paste0(directorio_tiff, "SouthAmerica/Combinado_sudamerica_recortado.tif"))
# Cambiar la region
#raster_tiff <- raster(paste0(directorio_tiff, "CentroAmerica/CentroAmerica_recortado.tif"))
#raster_tiff <- raster(paste0(directorio_tiff, "USA/combinado_USA_recortado.tif"))

# Leer info de cada estacion
estaciones_coords <- read.csv(paste0(directorio_tiff, "estaciones_coords.csv"))
cat("Total de estaciones:", nrow(estaciones_coords), "\n")

### Funcion de conteo
contar_por_categoria <- function(r, estacion, region, num_categorias) {
  vals <- values(r)
  vals <- vals[!is.na(vals)]  # eliminar NA
  
  # crear un factor con todos los niveles 1:num_categorias
  conteo <- table(factor(vals, levels = 0:num_categorias))
  df <- as.data.frame(t(as.vector(conteo)))
  colnames(df) <- paste0("Categoria_", 0:num_categorias)
  df$Estacion <- estacion
  df$Region <- region
  df$Total_pixeles <- sum(conteo)
  
  return(df)
}

# Setear para todas las estaciones de interes
df_rbind <- data.frame()
unique(estaciones_coords$Region)
estaciones_coords <- estaciones_coords[estaciones_coords$Region== "South America",]
estaciones_coords <- estaciones_coords[estaciones_coords$Region== "Mexico",]
for (i in 1:nrow(estaciones_coords)) {
  cat("Procesando estaci?n", i, "de", nrow(estaciones_coords), "...\n")
  
  # Obtener coordenadas
  coord_sep <- strsplit(estaciones_coords$coords[i], ", ")
  lon <- as.numeric(coord_sep[[1]][2])
  lat <- as.numeric(coord_sep[[1]][1])
  estacion <- estaciones_coords$Estacion[i]
  region <- estaciones_coords$Region[i]
  
  # Crear punto como sf
  p_sf <- st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon","lat"), crs = 4326)
  
  # Crear proyeccion local centrada en el punto
  crs_local <- paste0("+proj=aeqd +lat_0=", lat, " +lon_0=", lon)
  
  # Transformar a proyeccion local
  p_proj <- st_transform(p_sf, crs_local)
  
  # Crear buffer de 25 km exactos
  buffer_proj <- st_buffer(p_proj, dist = dist_buffer_m)
  
  # Volver a WGS84
  buffer_sf <- st_transform(buffer_proj, 4326)
  
  # Convertir a Spatial para usar con raster
  buffer_sp <- as(buffer_sf, "Spatial")
  
  # Recortar y enmascarar el raster
  raster_crop <- crop(raster_tiff, buffer_sp)
  raster_mask <- mask(raster_crop, buffer_sp)
  
  # Validar que existan pixeles dentro del buffer
  if (sum(!is.na(values(raster_mask))) == 0) {
    cat("  ??????  Sin datos de Land Cover para:", estacion, "\n")
    next
  }
  
  # Contar categor?as
  resultado <- contar_por_categoria(raster_mask, estacion, region, num_categorias)
  df_rbind <- bind_rows(df_rbind, resultado)
}
View(df_rbind)


# Exportar resultados

output_file <- paste0(directorio_tiff, "Conteo_categorias_landcover_25km_south.csv")
output_file <- paste0(directorio_tiff, "Conteo_categorias_landcover_25km_Mex.csv")

write.csv(df_rbind, output_file, row.names = FALSE)

cat("??? Proceso finalizado.\n")
cat("Archivo exportado a:\n", output_file, "\n")

View(df_rbind)
