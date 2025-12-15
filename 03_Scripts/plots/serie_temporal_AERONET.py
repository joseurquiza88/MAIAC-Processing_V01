import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path

COLOR_MAIAC = {
    "SP": "#005a32",
    "ST": "#fd8d3c",
    "BA": "#99000d",
    "MD": "#023858",
    "LP": "#ce1256",
    "MX": "#3f007d",
}


def aeronet_plot_latam(
    path,
    ciudad,
    color="#000000"
):
    path = Path(path)
    files = list(path.glob(f"*_{ciudad}_*.csv"))  # <-- cambio aquí

    if len(files) == 0:
        return None

    file = files[0]

    data = pd.read_csv(file)

    data["Timestamp"] = pd.to_datetime(
        data["date"],
        format="mixed",
        dayfirst=True,
        errors="coerce"
    )

    data = data.dropna(subset=["Timestamp"])
    data.set_index("Timestamp", inplace=True)
    data = data.sort_index()

    y = data["aod_550"].resample("1D").mean()

    fig, ax = plt.subplots(figsize=(10, 4), dpi=300)
    ax.plot(y, color=color, linewidth=1.2)
    ax.set_title(f"Serie temporal AERONET – {ciudad} (2015–2024)", fontsize=14)
    ax.set_ylabel("AOD 550 nm")
    ax.tick_params(labelsize=10)

    plt.tight_layout()
    plt.close(fig)
    return fig

