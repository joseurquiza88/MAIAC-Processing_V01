
# -------------------------------------------------
# Librerias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path

# -------------------------------------------------
# Funcion para mostrar la serie temporal de AERONET
def aeronet_plot_latam(path, ciudad, color="#000000"):
    path = Path(path)
    files = list(path.glob(f"*_{ciudad}_*.csv"))

    if len(files) == 0:
        return None, None

    file = files[0]
    data = pd.read_csv(file)

    #Poner en formato la fecha y setearlo como index
    data["Timestamp"] = pd.to_datetime(data["date"], format="mixed", dayfirst=True, errors="coerce")
    #Eliminar los NA
    data = data.dropna(subset=["Timestamp"])
    data.set_index("Timestamp", inplace=True)
    data = data.sort_index()

    # -------------------------------------------------
    # Serie diaria
    
    y = data["aod_550"].resample("1D").mean()

    # Fijar el periodo de interes
    y = y["2015-01-01":"2025-12-31"]

    # -------------------------------------------------
    # Métricas
 
    metrics = {
        "mean": y.mean(),
        "std": y.std(),
        "n": int(y.count())
    }

    # -------------------------------------------------
    # Plot

    fig, ax = plt.subplots(figsize=(8, 4), dpi=300)

    ax.plot(y, color=color, linewidth=1.2)
    ax.set_ylabel("AOD 550 nm", fontsize=11)
    ax.set_xlabel("Fecha", fontsize=11)
    ax.tick_params(labelsize=11)
    ax.set_ylim(0, 1.8)
    #ax.legend(fontsize=9)
    plt.close(fig)

    return fig, metrics
