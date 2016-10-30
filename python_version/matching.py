import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
from statsmodels.graphics.gofplots import qqplot

def standardize(data):
    return (data - data.mean())/data.std()

def draw_figures():
    bdims = pd.read_csv("bdims.csv")
    fdims = bdims[ bdims["sex"] == 0]

    fig, plots = plt.subplots(4, 2)

    biidi = standardize(fdims["bii.di"])
    elbdi = standardize(fdims["elb.di"])
    age = standardize(bdims["age"])
    chede = standardize(fdims["che.de"])

    plots[0][0].hist(biidi, bins=range(-4,4))
    plots[1][0].hist(elbdi, bins=range(-3,5))
    plots[2][0].hist(age, bins=range(-2,5))
    plots[3][0].hist(chede, bins=range(-2,6))

    plots[0][0].set_title("Histogram of female biiliac diameter")
    plots[1][0].set_title("Histogram of female elbow diameter")
    plots[2][0].set_title("Histogram of general age")
    plots[3][0].set_title("Histogram of female chest depth")
    
    qqplot(biidi, ax=plots[1][1], line="q")
    qqplot(elbdi, ax=plots[2][1], line="q")
    qqplot(age, ax=plots[3][1], line="q")
    qqplot(chede, ax=plots[0][1], line="q")

    plots[0][1].set_title("Normal Q-Q Plot A")
    plots[1][1].set_title("Normal Q-Q Plot B")
    plots[2][1].set_title("Normal Q-Q Plot C")
    plots[3][1].set_title("Normal Q-Q Plot D")
  
    for i in range(0,4):
        plots[i][0].set_xlabel("standarized data")
        plots[i][0].set_ylabel("frequency")

    fig.set_size_inches(12, 12)
    plt.tight_layout()

    return fig
