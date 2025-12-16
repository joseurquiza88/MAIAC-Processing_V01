import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats
from sklearn.linear_model import LinearRegression
from sklearn.metrics import root_mean_squared_error


def plot_comparativa_c60_c61(
    file,
    ciudad,
    color_c61="#005a32",
    color_c60="#bdbdbd",
    date_format="%d/%m/%Y"
):
    """
    Comparativa entre MAIAC C6.0 vs C6.1 frente a AERONET para una ciudad.

    Parámetros
    ----------
    file : es el path donde se encuentran los csv con los datos MAIAC + AERONET
    ciudad : Nombre de la estación / ciudad
    color_c61 : Color del producto de la version C6.1
    color_c60 : Color del producto de la version C6.0

    Retorna
    -------
    fig : matplotlib.figure.Figure
    metrics : dict
    """

    # -------------------------------------------------
    # Lectura de datos
    data = pd.read_csv(file)

    # -------------------------------------------------
    # Variables de cada version
    x = data["AOD_550_AER_mean_60"]
    y_c60 = data["AOD_550_maiac_mean_60"]
    y_c61 = data["AOD_550_maiac_mean_61"]

    #Mascara para descartar los na
    mask_c60 = (~np.isnan(x)) & (~np.isnan(y_c60))
    mask_c61 = (~np.isnan(x)) & (~np.isnan(y_c61))

    x60, y60 = x[mask_c60], y_c60[mask_c60]
    x61, y61 = x[mask_c61], y_c61[mask_c61]

    # Se agrega ver
    if len(x60) < 10 or len(x61) < 10:
        return None, None

     # -------------------------------------------------
    # Métricas
    # Se calculan las metricas de interes
    r2_c60 = scipy.stats.linregress(x60, y60).rvalue ** 2
    r2_c61 = scipy.stats.linregress(x61, y61).rvalue ** 2

    rmse_c60 = root_mean_squared_error(y60, x60)
    rmse_c61 = root_mean_squared_error(y61, x61)

    bias_c60 = (y60 - x60).mean()
    bias_c61 = (y61 - x61).mean()
    
    # -------------------------------------------------
    # Modelos
    model_c60 = LinearRegression().fit(x60.values.reshape(-1, 1), y60)
    model_c61 = LinearRegression().fit(x61.values.reshape(-1, 1), y61)

    x_line = np.linspace(0, 1.8, 100)

     # -------------------------------------------------
    # Plot
    fig, ax = plt.subplots(figsize=(8, 4), dpi=300)

    ax.scatter(x60, y60, s=10, alpha=0.4, color=color_c60, label="MAIAC C6.0")
    ax.scatter(x61, y61, s=10, alpha=0.4, color=color_c61, label="MAIAC C6.1")

    ax.plot(x_line, model_c60.predict(x_line.reshape(-1, 1)), color=color_c60,lw=2)
    ax.plot(x_line,model_c61.predict(x_line.reshape(-1, 1)),color=color_c61,lw=2)
    
    # Línea 1:1
    ax.plot([0, 1.8], [0, 1.8], "--", color="black", lw=1)

    #Configuracion de los ejes
    ax.set_xlim(0, 1.8)
    ax.set_ylim(0, 1.8)
    ax.tick_params(labelsize=11)
    ax.set_xlabel("AOD AERONET", fontsize=11)
    ax.set_ylabel("AOD SATELITAL", fontsize=11)


    # Métricas en el plot
    # ax.text(
    #     0.05, 0.92,
    #     f"C6.0  R²={r2_c60:.2f}  RMSE={rmse_c60:.2f}",
    #     transform=ax.transAxes,
    #     fontsize=9,
    #     color=color_c60
    # )

    # ax.text(
    #     0.05, 0.85,
    #     f"C6.1  R²={r2_c61:.2f}  RMSE={rmse_c61:.2f}",
    #     transform=ax.transAxes,
    #     fontsize=9,
    #     color=color_c61
    # )

    #Leyenda
    ax.legend(fontsize=9)
    plt.close(fig)

     # -------------------------------------------------
    # Métricas abajo del plot
    metrics = {
        "R2_C60": r2_c60,
        "R2_C61": r2_c61,
        "RMSE_C60": rmse_c60,
        "RMSE_C61": rmse_c61,
        "Bias_C60": bias_c60,
        "Bias_C61": bias_c61,
        "n_C60": len(x60),
        "n_C61": len(x61)
    }

    return fig, metrics
