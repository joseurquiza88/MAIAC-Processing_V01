

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

def get_ciudad_from_filename(filepath):
    filename = os.path.basename(filepath)
    # Ej: 1_SP-1km-MAIAC-60-AER_MEAN_C61.csv
    try:
        ciudad = filename.split("_")[1].split("-")[0]
    except IndexError:
        ciudad = "Desconocida"
    return ciudad

def plot_regresion_maiac_csv(
    file,
    date_format="%d/%m/%Y"#,
    #color="#fd8d3c"
):

    ciudad = get_ciudad_from_filename(file)
    color = COLOR_MAIAC.get(ciudad, "#666666")  # gris por defecto


    data = pd.read_csv(file)
    data["Timestamp"] = pd.to_datetime(
        data["date"], format=date_format
    )
    data.set_index("Timestamp", inplace=True)

    x = data["AOD_550_AER_mean"]
    y = data["AOD_550_maiac_mean"]

    mask = (~np.isnan(x)) & (~np.isnan(y))
    x = x[mask]
    y = y[mask]

    n = len(x)
    if n < 10:
        return None, None

    slope, intercept, r, _, _ = scipy.stats.linregress(x, y)
    R2 = r**2
    RMSE = root_mean_squared_error(y, x)
    bias = round((y - x).mean(), 2)

    # modelo
    x_p = x.values.reshape(-1, 1)
    model = LinearRegression().fit(x_p, y.values.reshape(-1, 1))
    y_pred = model.predict(x_p)

    fig, ax = plt.subplots(figsize=(4, 4), dpi=100)


    ax.scatter(x, y, alpha=0.4, s=8, color=color)
    ax.plot(x, y_pred, color=color, linewidth=1)
    ax.plot([0, 1.8], [0, 1.8], "--", color="black")

    ax.set_xlim(0, 1.8)
    ax.set_ylim(0, 1.8)

    ax.set_xlabel("AOD AERONET", fontsize=11)
    ax.set_ylabel("AOD MAIAC", fontsize=11)


    # ax.text(0.05, 0.95, f"$R^2$ = {R2:.2f}", transform=ax.transAxes)
    # ax.text(0.05, 0.90, f"RMSE = {RMSE:.2f}", transform=ax.transAxes)
    # ax.text(0.05, 0.85, f"Bias = {bias}", transform=ax.transAxes)
    # ax.text(0.05, 0.80, f"n = {n}", transform=ax.transAxes)

    #plt.tight_layout()
    #ax.set_title(f"Regresión MAIAC vs AERONET – {ciudad}", fontsize=10)

    plt.close(fig)

    metrics = {
        "R2": R2,
        "RMSE": RMSE,
        "Bias": bias,
        "n": n
    }

    return fig, metrics
