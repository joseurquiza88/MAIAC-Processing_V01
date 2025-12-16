

# Librerias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats
from sklearn.linear_model import LinearRegression
from sklearn.metrics import root_mean_squared_error

 # -------------------------------------------------
 # Funcion para plotear la regresion lineal para comparar los productos MODIS vs MAIAC
def plot_modis_maiac(file, ciudad, color_maiac="#005a32", color_modis="#bdbdbd"):
    """
    Comparativa MODIS vs MAIAC (C6.1) frente a AERONET.
    """

    # -------------------------------------------------
    # Lectura de datos
    
    data = pd.read_csv(file)

    # -------------------------------------------------
    # Variables
    x = data["AOD_550_AER_mean"]
    y_modis = data["AOD_modis"]
    y_maiac = data["AOD_maiac_61"]

    # Descartar NA /Mascara
    mask_modis = (~np.isnan(x)) & (~np.isnan(y_modis))
    mask_maiac = (~np.isnan(x)) & (~np.isnan(y_maiac))

    x_modis, y_modis = x[mask_modis], y_modis[mask_modis]
    x_maiac, y_maiac = x[mask_maiac], y_maiac[mask_maiac]

    #Se agrega revisar
    if len(x_modis) < 10 or len(x_maiac) < 10:
        return None, None

    # -------------------------------------------------
    # Métricas

    r2_modis = scipy.stats.linregress(x_modis, y_modis).rvalue ** 2
    r2_maiac = scipy.stats.linregress(x_maiac, y_maiac).rvalue ** 2

    rmse_modis = root_mean_squared_error(y_modis, x_modis)
    rmse_maiac = root_mean_squared_error(y_maiac, x_maiac)

    bias_modis = (y_modis - x_modis).mean()
    bias_maiac = (y_maiac - x_maiac).mean()

    # -------------------------------------------------
    # Modelos
    model_modis = LinearRegression().fit(x_modis.values.reshape(-1, 1), y_modis)   
    model_maiac = LinearRegression().fit(x_maiac.values.reshape(-1, 1), y_maiac)
    x_line = np.linspace(0, 1.8, 100)

    # -------------------------------------------------
    # Plot

    fig, ax = plt.subplots(figsize=(8, 4), dpi=300)
    ax.scatter(x_modis, y_modis, s=10, alpha=0.4, color = color_modis, label = "MODIS")
    ax.scatter(x_maiac, y_maiac, s=10, alpha=0.4, color = color_maiac, label = "MAIAC")
    ax.plot(x_line, model_modis.predict(x_line.reshape(-1, 1)), color = color_modis, lw = 2)
    ax.plot(x_line, model_maiac.predict(x_line.reshape(-1, 1)), color=color_maiac, lw = 2)
    # Línea 1:1
    ax.plot([0, 1.8], [0, 1.8], "--", color="black", lw=1)

    # Configurar ejes
    ax.set_xlim(0, 1.8)
    ax.set_ylim(0, 1.8)
    ax.tick_params(labelsize=11)
    ax.set_xlabel("AOD AERONET", fontsize=11)
    ax.set_ylabel("AOD SATELITAL", fontsize=11)

    ax.legend(fontsize=9)
    plt.close(fig)

     # -------------------------------------------------
    # Métricas abajo del plot

    metrics = {
        "R2_modis": r2_modis,
        "RMSE_modis": rmse_modis,
        "Bias_modis": bias_modis,
        "n_modis": len(x_modis),
        "R2_maiac": r2_maiac,
        "RMSE_maiac": rmse_maiac,
        "Bias_maiac": bias_maiac,
        "n_maiac": len(x_maiac),
    }

    return fig, metrics
