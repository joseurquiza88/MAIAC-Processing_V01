
 # -------------------------------------------------
# Librerias
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats
from sklearn.metrics import root_mean_squared_error
from sklearn.linear_model import LinearRegression

COLOR_MAIAC = {
    "SP": "#005a32",
    "ST": "#fd8d3c",
    "BA": "#99000d",
    "MD": "#023858",
    "LP": "#ce1256",
    "MX": "#3f007d",
}

 # -------------------------------------------------
 # Funcion para obtener el nombre de la ciudad
 # a esto lo usamos en todas las funciones
def get_ciudad_from_filename(filepath):
    filename = os.path.basename(filepath)
    try:
        ciudad = filename.split("_")[1].split("-")[0]
    except IndexError:
        ciudad = "Desconocida"
    return ciudad

 # -------------------------------------------------
 # Funcion para generar la regresion lineal de MAIAC frente a AERONET
def plot_regresion_maiac_csv(
    file,
    date_format="%d/%m/%Y"#, # Puede ser que el formato de la fecha no coincida
    #color="#fd8d3c"
):
    # Se obtiene el archivo que corresponde al nombre seleccionado segun la ciudad
    ciudad = get_ciudad_from_filename(file)
    color = COLOR_MAIAC.get(ciudad, "#666666")  # gris por defecto

    #Leer el archivo
    data = pd.read_csv(file)
    # Generar index con la fecha
    data["Timestamp"] = pd.to_datetime(data["date"], format=date_format)
    data.set_index("Timestamp", inplace=True)
    #Tomar las variables 
    x = data["AOD_550_AER_mean"]
    y = data["AOD_550_maiac_mean"]

    # Enmascarar los NAs
    mask = (~np.isnan(x)) & (~np.isnan(y))
    x = x[mask]
    y = y[mask]

    # Se agrega, revisar!
    n = len(x)
    if n < 10:
        return None, None
    
     # -------------------------------------------------
     #Calcular metrias de interes
    slope, intercept, r, _, _ = scipy.stats.linregress(x, y)
    R2 = r**2
    RMSE = root_mean_squared_error(y, x)
    bias = round((y - x).mean(), 2)

    # -------------------------------------------------
    # Modelo
    x_p = x.values.reshape(-1, 1)
    model = LinearRegression().fit(x_p, y.values.reshape(-1, 1))
    y_pred = model.predict(x_p)

    # -------------------------------------------------
    # Figura
    fig, ax = plt.subplots(figsize=(8, 4), dpi=300)
    ax.scatter(x, y, alpha=0.4, s=8, color=color)
    ax.plot(x, y_pred, color=color, linewidth=1)
    ax.plot([0, 1.8], [0, 1.8], "--", color="black")

    # Configurar los ejes
    ax.set_xlim(0, 1.8)
    ax.set_ylim(0, 1.8)
    ax.tick_params(labelsize=11)
    ax.set_xlabel("AOD AERONET", fontsize=11)
    ax.set_ylabel("AOD MAIAC", fontsize=11)
    #ax.legend(fontsize=9)

    # -------------------------------------------------
    # Corresponde a las metricas dentro del plot
    # ax.text(0.05, 0.95, f"$R^2$ = {R2:.2f}", transform=ax.transAxes)
    # ax.text(0.05, 0.90, f"RMSE = {RMSE:.2f}", transform=ax.transAxes)
    # ax.text(0.05, 0.85, f"Bias = {bias}", transform=ax.transAxes)
    # ax.text(0.05, 0.80, f"n = {n}", transform=ax.transAxes)

    #plt.tight_layout()
    #ax.set_title(f"Regresión MAIAC vs AERONET – {ciudad}", fontsize=10)

    plt.close(fig)

    # -------------------------------------------------
    # Visualizar las metricas por afuera del plot
    metrics = {
        "R2": R2,
        "RMSE": RMSE,
        "Bias": bias,
        "n": n
    }

    return fig, metrics
