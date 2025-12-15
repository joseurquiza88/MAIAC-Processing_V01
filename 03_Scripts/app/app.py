import streamlit as st
from pathlib import Path
from plots.regresion_maiac import plot_regresion_maiac_csv
from plots.regresion_maiac import plot_regresion_maiac_csv, get_ciudad_from_filename
from plots.serie_temporal_AERONET import aeronet_plot_latam
from plots.regresion_maiac import COLOR_MAIAC


# ===============================
# PATH
# ===============================
BASE_DIR = Path(__file__).resolve().parent
# DATA_DIR = (
#     BASE_DIR.parent
    
#     / "02_Datasets"
#     / "processed"
#     / "merge_AER-MAIAC"
#     / "Latam"
#     / "1km"
#     / "60mins"
# )

# AERONET_DIR = (
#     BASE_DIR.parent
#     / "02_Datasets"
#     / "AERONET"
# )


from pathlib import Path

BASE_DIR = Path(__file__).resolve().parent  # esto apunta a 03_Scripts/app

DATA_DIR = BASE_DIR.parent.parent / "02_Datasets" / "processed" / "merge_AER-MAIAC" / "Latam" / "1km" / "60mins"
AERONET_DIR = BASE_DIR.parent.parent / "02_Datasets" / "AERONET"




# ===============================
# UI
# ===============================
st.set_page_config(layout="centered")
# st.title("Evaluacion del desempeño de AOD satelital (MAIAC C6.1) frente a la Red AERONET")
st.markdown(
    '<h2 style="font-size:30px; text-align:center;">Evaluación del desempeño de AOD satelital (MAIAC C6.1) frente a la Red AERONET</h2>',
    unsafe_allow_html=True
)

files = sorted(DATA_DIR.glob("*.csv"))

if len(files) == 0:
    st.error("No se encontraron archivos CSV")
    st.stop()

# nombres amigables
station_dict = {
    get_ciudad_from_filename(f): f
    for f in files
}


station_name = st.selectbox(
    "Seleccionar estación",
    station_dict.keys()
)

file_selected = station_dict[station_name]

# ===============================
# PLOT
# ===============================

st.subheader("Serie temporal AERONET")

color_ciudad = COLOR_MAIAC.get(station_name, "#666666")


fig_ts = aeronet_plot_latam(
    path=AERONET_DIR,
    ciudad=station_name,
    color=color_ciudad
)

if fig_ts is None:
    st.warning("No hay datos AERONET para esta estación")
else:
    st.pyplot(fig_ts)

st.divider()



fig, metrics = plot_regresion_maiac_csv(file_selected)
st.subheader("Regresión lineal MAIAC C6.1 frente a AERONET")
if fig is None:
    st.warning("Datos insuficientes para esta estación")
else:
    st.pyplot(fig)

    st.markdown("### Métricas")
    # col1, col2, col3, col4 = st.columns(4)

    # col1.metric("R²", f"{metrics['R2']:.2f}")
    # col2.metric("RMSE", f"{metrics['RMSE']:.2f}")
    # col3.metric("Bias", metrics["Bias"])
    # col4.metric("n", metrics["n"])
   

col1, col2, col3, col4 = st.columns(4)

def metric_block(label, value):
    st.markdown(
        f"""
        <div style="text-align:center">
            <div style="font-size:25px; font-weight:600;">{label}</div>
            <div style="font-size:20px; color:#555;">{value}</div>
        </div>
        """,
        unsafe_allow_html=True
    )

with col1:
    metric_block("R²", f"{metrics['R2']:.2f}")

with col2:
    metric_block("RMSE", f"{metrics['RMSE']:.2f}")

with col3:
    metric_block("Bias", f"{metrics['Bias']:.2f}")

with col4:
    metric_block("n", int(metrics["n"]))


