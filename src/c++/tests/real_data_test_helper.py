import os
import pandas as pd
import numpy as np
from mne.filter import filter_data
import argparse

def tofile(arr, name : str):
    file = open(name, 'w')
    arr.tofile(file)
    file.close()

def get_Scv (chbmit : str, patient : str, signal : str, samplerate : int, filtered : bool):
    print("\033[1m", end="")
    print(f"CHBMIT dir : {chbmit}")
    print(f"Patient    : {patient}")
    print(f"Signal     : {signal}")
    print(f"Sample Rate: {samplerate}")
    print(f"Filtered   : {filtered}")
    print("\033[0m", end="")
    dfi = pd.read_hdf(f'{chbmit}/{patient}/signal_{patient}.h5', key='data_frame')
    ts = dfi[signal].values.reshape(1,-1)
    if filtered:
        lowcut = 0.5
        highcut = 70.0
        data_all = filter_data(ts, samplerate, lowcut, highcut, verbose=False)[0]
    else:
        data_all = ts[0]
    return data_all

def my_eval (s : str):
    return eval(s)
