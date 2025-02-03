#!/usr/bin/env  python3
# *-* encoding=utf8 *-*

import os
import pandas as pd
import numpy as np
from mne.filter import filter_data
import argparse

def tofile(arr, name : str):
    file = open(name, 'w')
    arr.tofile(file)
    file.close()

def main():
    # TODO: Ask for parameters.
    """
    parser = argparse.ArgumentParser(description='Computes the PSD using a slide window. The result is a vector whose size depends on the chosen STRIDE and QUERY sizes.')
    parser.add_argument('-P','--patient', required=True, help='Patient number. Eg: chb24')
    parser.add_argument('-S','--signal', required=True, help='Signal label. Eg: F4-C4')
    parser.add_argument('-f','--filter', type=bool, default=False, help='Active bandpass filter.')
    args = parser.parse_args()

    # patient = args.patient
    # signal = args.signal
    # filtered = args.filter
    """

    print("Generating data_all and query_all...")

    patient = "chb06"
    signal = "F3-C3"
    filtered = True
    samplerate = 256

    dfi = pd.read_hdf(f'../CHBMIT/{patient}/signal_{patient}.h5', key='data_frame')
    ts = dfi[signal].values.reshape(1,-1)
    dfq = pd.read_csv(f'../CHBMIT/{patient}/seizures0_{patient}.csv')
    seizures = dfq[signal].values.reshape(1,-1)

    if filtered:
        lowcut = .5
        highcut = 70.
        data_all = filter_data(ts, samplerate, lowcut, highcut, verbose=False)[0]
        print(data_all)
        query_all = filter_data(seizures, samplerate, lowcut, highcut, verbose=False)[0]
    else:
        data_all = ts[0]
        query_all = seizures[0]
    path = os.path.join(os.path.dirname(__file__), "generated")
    tofile(data_all, os.path.join(path, "data_all"))
    tofile(query_all, os.path.join(path, "query_all"))
    print("Done!")

if __name__ == "__main__":
    main()
