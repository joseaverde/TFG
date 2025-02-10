from typing import Final
from mne.filter import filter_data
import numpy as np
import pandas as pd
import json
import csv
import struct

def process (patient    : str,
             chbmit_dir : str,
             model_file : str,
             output     : str,
             format     : str = "d"):
    with open(model_file, "r") as fp:
        data = json.load(fp)
    def get (name : str) -> object:
        return data[patient][name]
    # Load the signal
    signal     : Final[str]   = get("channel")
    samplerate : Final[int]   = 256
    lowcut     : Final[float] = 0.5
    highcut    : Final[float] = 70.0

    dfi = pd.read_hdf(f'{chbmit_dir}/{patient}/signal_{patient}.h5',
                      key='data_frame')
    ts = dfi[signal].values.reshape(1,-1)
    Scv = filter_data(ts, samplerate, lowcut, highcut, verbose=False)[0]
    Scv = ts[0]

    # Get the batch
    query_size : Final[int] = 256 * 5
    bounds = [get(x) for x in ["p1_min", "p1_max", "p2_min", "p2_max",
                               "p3_min", "p3_max", "d_min", "d_max",
                               "e_min", "e_max", "dmax"]]
    indices = map(int, get("batch").replace(" ", "").split(","))
    pj = [Scv[index : index + query_size] for index in indices]
    # Write to the file
    with open(output, 'wb') as fp:
        fp.write(struct.pack("i", len(pj)))
        fp.write(struct.pack("i", len(Scv)))
        for bound in bounds:
            fp.write(struct.pack(format, bound))
        for pattern in pj:
            for sample in pattern:
                fp.write(struct.pack(format, sample))
        c = 0
        for sample in Scv:
            if sample != int(sample):
                c+=1
            fp.write(struct.pack(format, sample))
        print(100*c/len(Scv))

process(patient    = "chb03",
        chbmit_dir = "/home/joseaverde/Development/seizure-algorithm/CHBMIT",
        model_file = "/home/joseaverde/Development/seizure-algorithm/datos/" +
                     "DatosDeteccionSeizure/aa_ma_params_b3z2mz18ws5.json",
        output     = "chb03.bin")
