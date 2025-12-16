#Libreroas
import streamlit as st
from pathlib import Path

from plots.serie_temporal_AERONET import aeronet_plot_latam
from plots.regresion_maiac import (plot_regresion_maiac_csv, get_ciudad_from_filename, COLOR_MAIAC)
from plots.comparativa_c60_c61 import plot_comparativa_c60_c61
from plots.modis_maiac import plot_modis_maiac

# -------------------------------------------------
# Setear los path para los archivos en cada carpeta

BASE_DIR = Path(__file__).resolve().parent  # 03_Scripts/app
AERONET_DIR = BASE_DIR.parent.parent / "02_Datasets" / "AERONET"
REGRESION_DIR = (
    BASE_DIR.parent.parent
    / "02_Datasets"
    / "processed"
    / "merge_AER-MAIAC"
    / "Latam"
    / "1km"
    / "60mins"
)

COMPARATIVA_DIR = (
    BASE_DIR.parent.parent
    / "02_Datasets"
    / "processed"
    / "merge_AER-MAIAC_versiones"
)

MODIS_MAIAC_MODIS_DIR = (
    BASE_DIR.parent.parent
    / "02_Datasets"
    / "processed"
    / "merge_AER-MAIAC-MODIS"
    / "60mins"
)

 # -------------------------------------------------
# Configuracion del STREAMLIT

st.set_page_config(page_title="Evaluación MAIAC vs AERONET", layout="wide")

# -------------------------------------------------
# Titulo general
st.markdown("<h2 style='text-align:center;'>Evaluación del desempeño de AOD satelital frente a AERONET</h2>",
    unsafe_allow_html=True
)

# Separar secciones entre los plots y texto
st.divider()

# -------------------------------------------------
# Cargar los archivos para cada funcion 
files_reg = sorted(REGRESION_DIR.glob("*.csv"))
files_comp = sorted(COMPARATIVA_DIR.glob("*.csv"))
files_modis = sorted(MODIS_MAIAC_MODIS_DIR.glob("*.csv"))

# Si no encuentra los achivos hay error
if not files_reg:
    st.error("No hay archivos de regresión MAIAC C6.1")
    st.stop()

# Diccionarios por ciudad
dict_reg = {get_ciudad_from_filename(f): f for f in files_reg}
dict_comp = {get_ciudad_from_filename(f): f for f in files_comp}
dict_modis = {get_ciudad_from_filename(f): f for f in files_modis}


# -------------------------------------------------
# Seleccionar ciudad a partir de una lista

# station_name = st.selectbox(
#     "Seleccionar estación AERONET",
#     sorted(dict_reg.keys())
# )

col_sel, col_empty = st.columns([1.2, 4.8])

with col_sel:
    st.markdown(
        "<div style='font-size:24px; font-weight:600;'>Seleccionar estación AERONET</div>",
        unsafe_allow_html=True
    )

    station_name = st.selectbox(
        "",
        sorted(dict_reg.keys()),
        label_visibility="collapsed"
    )


# -------------------------------------------------
# Tomar los nombres de la ciudades pra que se muestren en cada plot
file_reg = dict_reg[station_name]
file_comp = dict_comp.get(station_name)
file_modis = dict_modis.get(station_name)
# usar colroes
color_ciudad = COLOR_MAIAC.get(station_name, "#666666")


# -------------------------------------------------
# Configuracion del Layout 2 filas x 2 columnas
col1, col2 = st.columns(2)

# -------------------------------------------------
# Plot n° 1: Serie temporal de AERONET
with col1:
    
    fig_ts, metrics_ts = aeronet_plot_latam(
        path=AERONET_DIR,
        ciudad=station_name,
        color=color_ciudad
    )
    # Titulo de la seccion
    st.subheader("Serie temporal diaria AERONET")
    if fig_ts:
        st.pyplot(fig_ts, use_container_width=True)
        #Metricas abajo del plot
        if metrics_ts:
            st.markdown(
                f"""
                 <div style="text-align:center; "font-size:24px; line-height:1.5;">
                    <b>Media</b> = {metrics_ts['mean']:.2f} &nbsp;|&nbsp;
                    <b>SD</b> = {metrics_ts['std']:.2f} &nbsp;|&nbsp;
                    <b>n</b> = {metrics_ts['n']}
                </div>
                """,
                unsafe_allow_html=True
            )
    else:
        st.warning("No hay datos AERONET")


# -------------------------------------------------
# Plot n° 2: Regresión MAIAC C6.1 vs AERONET

with col2:
    st.subheader("Regresión MAIAC C6.1 vs AERONET")

    fig_reg, metrics_reg = plot_regresion_maiac_csv(file_reg)

    if fig_reg:
        st.pyplot(fig_reg, use_container_width=True)

        if metrics_reg:
            st.markdown(
    f"""
    <div style="text-align:center;"font-size:24px; line-height:1.4;">
        <b>R²</b> = {metrics_reg['R2']:.2f} &nbsp;|&nbsp;
        <b>RMSE</b> = {metrics_reg['RMSE']:.2f} &nbsp;|&nbsp;
        <b>Bias</b> = {metrics_reg['Bias']:.2f} &nbsp;|&nbsp;
        <b>n</b> = {int(metrics_reg['n'])}
    </div>
    """,
    unsafe_allow_html=True
)

    else:
        st.warning("Datos insuficientes para regresión")



# -------------------------------------------------
# Dividir para generar una 2da fila
st.divider()
col3, col4 = st.columns(2)

# -------------------------------------------------
# Plot n° 3: Comparativa MAIAC C6.0 vs C6.1

with col3:
    st.subheader("MAIAC C6.0 vs C6.1")

    if file_comp is None:
        st.warning("No hay dataset C6.0 para esta estación")
    else:
        fig_comp, metrics_comp = plot_comparativa_c60_c61(
            file=file_comp,
            ciudad=station_name,
            color_c61=color_ciudad
        )

        if fig_comp:
            st.pyplot(fig_comp, use_container_width=True)

            if metrics_comp:
                st.markdown(
    f"""
    <div style="text-align:center; "font-size:24px; line-height:1.5;">
        <b>C6.0</b> →
        <b>R²</b> = {metrics_comp['R2_C60']:.2f} &nbsp;|&nbsp;
        <b>RMSE</b> = {metrics_comp['RMSE_C60']:.2f} &nbsp;|&nbsp;
        <b>Bias</b> = {metrics_comp['Bias_C60']:.2f} &nbsp;|&nbsp;
        <b>n</b> = {metrics_comp['n_C60']:.2f} &nbsp;|&nbsp;
        <br>
        <b>C6.1</b> →
        <b>R²</b> = {metrics_comp['R2_C61']:.2f} &nbsp;|&nbsp;
        <b>RMSE</b> = {metrics_comp['RMSE_C61']:.2f} &nbsp;|&nbsp;
        <b>Bias</b> = {metrics_comp['Bias_C61']:.2f} &nbsp;|&nbsp;
        <b>n</b> = {metrics_comp['n_C61']:.2f} &nbsp;|&nbsp;
    </div>
    """,
    unsafe_allow_html=True
)

        else:
            st.warning("Datos insuficientes para comparativa")

# -------------------------------------------------
# Plot n° 4: Comparativa MAIAC vs MODIS

with col4:
    st.subheader("MAIAC vs MODIS")

    if file_modis is None:
        st.warning("No hay datos MODIS–MAIAC para esta estación")
    else:
        fig_mm, metrics_mm = plot_modis_maiac(
    file=file_modis,
    ciudad=station_name,
    color_maiac=color_ciudad   # MISMO verde que MAIAC C6.1
)


        if fig_mm:
            st.pyplot(fig_mm, use_container_width=True)

            if metrics_mm:
                st.markdown(
    f"""
    <div style="text-align:center; "font-size:24px; line-height:1.5;">
        <b>MODIS DT </b> →
        <b>R²</b> = {metrics_mm['R2_modis']:.2f} &nbsp;|&nbsp;
        <b>RMSE</b> = {metrics_mm['RMSE_modis']:.2f} &nbsp;|&nbsp;
        <b>Bias</b> = {metrics_mm['Bias_modis']:.2f} &nbsp;|&nbsp;
        <b>n</b> = {metrics_mm['n_modis']:.2f} &nbsp;|&nbsp;
        <br>
        <b>MAIAC C6.1</b> →
        <b>R²</b> = {metrics_mm['R2_maiac']:.2f} &nbsp;|&nbsp;
        <b>RMSE</b> = {metrics_mm['RMSE_maiac']:.2f} &nbsp;|&nbsp;
        <b>Bias</b> = {metrics_mm['Bias_maiac']:.2f} &nbsp;|&nbsp;
        <b>n</b> = {metrics_mm['n_maiac']:.2f} &nbsp;|&nbsp;
    </div>

    """,
    unsafe_allow_html=True
)
        else:
            st.warning("Datos insuficientes para MODIS–MAIAC")



# -------------------------------------------------
# Footer de la pagina

st.divider()
st.markdown(
    "<p style='text-align:center; font-size:13px;'>Validación de productos satelitales</p>",
    unsafe_allow_html=True
)
