# 📊 Procesamiento y análisis de productos satelitales de AOD en centros urbanos de América Latina

---
- Este repositorio contiene scripts, notebooks y recursos para el **procesamiento y análisis de productos satelitales de Aerosol Optical Depth (AOD)** en centros urbanos de América Latina.
  
- Esta investigación se desarrolla en el marco de una tesis de doctorado en Ingenieria Ambiental de la Universidad Tecnologica Nacional -  Facultad Regional Mendoza actualmente en proceso 🚧
---

## 🎯 Objetivo

Transformar **datos satelitales crudos** en información útil para:

- Evaluación de la **calidad del aire**  
- Estimación de **PM2.5**  
- Caracterización **espacio-temporal de los aerosoles**  

---

## 🛠 Qué incluye

- **Metodologia utilizada:** (🚧 En desarrollo)
La carpeta [`/00_Informacion_de_Base`](./00_Informacion_de_Base/) reúne la metodología aplicada en cada etapa del proyecto, junto con la descripción de los productos utilizados 

- **Procesamiento de datos:**  
La carpeta [`/03_Scripts`](./03_Scripts/) muestra los codigos en R y Python para la descarga, recolección, limpieza, interpolación y análisis de variables satelitales utilizadas (AOD superficial y satelital), con integración final en un **dataset unificado**.

- **Evaluacion del desempeño:**  
La carpeta [`/03_Scripts`](./03_Scripts/) muestra los codigos en R y Python para el cálculo de métricas estadisticas como R², RMSE, Bias y la incertidumbre.
  
- **Ejemplos de uso** (🚧 En desarrollo)
La carpeta [`/04_Codigos ejemplo`](./04_Codigos_ejemplo/) contiene scripts en Python y R que sirven como guía para cargar, visualizar y procesar las imágenes raster y los archivos asociados al proyecto.

- **Requerimientos**  (🚧 En desarrollo)
La carpeta [`/01_Requerimientos`](./01_Requerimientos/) incluye los **requerimientos técnicos** para ejecutar los scripts en **R y Python**, donde se incluyen las librerías necesarias y su instalación.

- **Datasets:**  
  La carpeta [`/02_Datasets`](./02_Datasets/) contiene los datasets utilizados en este análisis de AOD satelital.  
  ⚠️ **Nota:** los nombres de los archivos y/o las rutas pueden diferir de los utilizados actualmente en el código, ya que el repositorio se encuentra en proceso de organización.

---

## 📦 Dataset disponibles

La carpeta [`/02_Datasets`](./02_Datasets) incluye los datasets generados en este proyecto para cada centro urbano generado.

---


---
## 🌐 Dashboard interactivo (🚧 En desarrollo)

Objetivo: Visualizar y explora los resultados obtenidos en este proyecto para distintas ciudades de América Latina con un **dashboard interactivo en Shiny**:


---


## 📚 Publicaciones asociadas

- Urquiza, J., Diez, S., Tames, M. F., & Puliafito, S. E. (2025). Assessment of aerosol remote sensing uncertainty in urban centers of Latin America. Atmósfera, 39, 613–627. https://doi.org/10.20937/ATM.5348
