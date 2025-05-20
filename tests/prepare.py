from typing import Final
import sys
import os

root : Final[str] = os.path.dirname(os.path.dirname(os.path.join(os.getcwd(),
                                                                 sys.argv[0])))
sys.path.append(os.path.join(root, "reference"))
sys.path.append(os.path.join(root, "reference/mylibrary/computeDTW/EEGLib/"
                                 + "build/lib.linux-x86_64-cpython-310"))

import EEGLIB
from typing import Final
from mne.filter import filter_data
import numpy as np
import itertools
import pandas as pd
import json
import csv
from mylibrary.optimizer.functions import f_validation
from mylibrary.common.classes import DTW_Matrix
from mylibrary.common.utils import call_psd_tri, call_energy, call_max_dist

class Batch:
    psd_1    : tuple[float, float]
    psd_2    : tuple[float, float]
    psd_3    : tuple[float, float]
    energy   : tuple[float, float]
    max_dist : tuple[float, float]
    d_max_c  : float
    patterns : list[np.array]

class Metadata:
    def __init__ (self, name : str):
        with open(name, "r", newline="") as csvfile:
            reader = csv.reader(csvfile,delimiter=",")
            self.__bounds = [(int(row[4]) * stride, int(row[5]) * stride,)
                             for row in list(reader)[1:-1]]
    def is_seizure (self, seizure : int) -> bool:
        index : Final[int] = seizure * stride
        for (f, t) in self.__bounds:
            if f <= index <= t:
                return True
        return False

    def get_bounds (self) -> list[tuple[int, int]]:
        return list(self.__bounds)

class Patient:
    signal : np.array
    batch  : Batch

def load (patient : str, chbmit_dir : str, model_file : str):
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
    # NOFILTER: Scv = ts[0]

    # Get the batch
    query_size : Final[int] = 256 * 5
    bounds = [get(x) for x in ["p1_min", "p1_max", "p2_min", "p2_max",
                               "p3_min", "p3_max", "d_min", "d_max",
                               "e_min", "e_max", "dmax"]]
    indices = map(int, get("batch").replace(" ", "").split(","))

    result = Patient()
    Scv = Scv.astype("float32")
    result.signal = Scv
    result.batch = Batch()
    result.batch.psd_1    = (get("p1_min"), get("p1_max"))
    result.batch.psd_2    = (get("p2_min"), get("p2_max"))
    result.batch.psd_3    = (get("p3_min"), get("p3_max"))
    result.batch.energy   = (get("e_min"),  get("e_max"))
    result.batch.max_dist = (get("d_min"),  get("d_max"))
    result.batch.d_max_c  = get("dmax")
    result.batch.patterns = [Scv[index * samplerate
                                 : index * samplerate + query_size]
                                 for index in indices]
    return result

def dump_input (signal : np.array, output : str):
    with open(output, 'w') as fp:
        print(f"{len(signal)}", file=fp)
        print(" ".join(list(map(str, signal))), file=fp)

def dump_batch(batch : Batch, output : str):
    with open(output, 'w') as fp:
        print(f"{len(batch.patterns)}", file=fp)
        print(f"{batch.psd_1[0]}    {batch.psd_1[1]}",    file=fp)
        print(f"{batch.psd_2[0]}    {batch.psd_2[1]}",    file=fp)
        print(f"{batch.psd_3[0]}    {batch.psd_3[1]}",    file=fp)
        print(f"{batch.energy[0]}   {batch.energy[1]}",   file=fp)
        print(f"{batch.max_dist[0]} {batch.max_dist[1]}", file=fp)
        print(f"{batch.d_max_c}", file=fp)
        for pattern in batch.patterns:
            print(" ".join(map(str, pattern)), file=fp)

class SlidingWindow:
    def __init__(self, channel : np.array, size : int, stride : int):
        self.__channel = channel
        self.__size = size
        self.__stride = stride
        self.__index = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self.__index + self.__size > len(self.__channel):
            raise StopIteration
        view = self.__channel[self.__index : self.__index + self.__size]
        self.__index += self.__stride
        return view

class Result:
    pre      : float
    sen      : float
    f_1      : float
    features : list[tuple[bool, bool, float, float, float, float, float, float]]

def within (x, t):
    return t[0] <= x <= t[1]

def PRE(tp, tn, fp, fn):
    if tp + fp == 0:
        return 1
    else:
        return tp / (tp + fp)

def SEN(tp, tn, fp, fn):
    if tp + fn == 0:
        return 0
    else:
        return tp / (tp + fn)

def F_1(tp, tn, fp, fn):
    sen = SEN(tp, tn, fp, fn)
    pre = PRE(tp, tn, fp, fn)
    if sen + pre == 0:
        return 0
    else:
        return 2 * (pre * sen) / (pre + sen)

def solve (patient : Patient) -> Result:
    result = Result()
    result.features = []
    epoch = 1280
    stride = 256
    max_warp = 16
    tp = tn = fp = fn = 0
    for (view, idx) in zip(SlidingWindow(patient.signal, epoch, stride),
                           itertools.count(epoch)):
        psd_1, psd_2, psd_3 = call_psd_tri(view, epoch, stride, 1)[0]
        energy = call_energy(view, epoch, stride, 1)[0]
        max_dist = call_max_dist(view, epoch, stride, 1)[0]
        dtws = []
        for pattern in patient.batch.patterns:
            d = EEGLIB.GetDistMtx(view, len(view), pattern, len(pattern),
                                  epoch, stride, max_warp, False)[0]
            dtws.append(d)
        guess = (within(psd_1,    patient.batch.psd_1)
             and within(psd_2,    patient.batch.psd_2)
             and within(psd_3,    patient.batch.psd_3)
             and within(energy,   patient.batch.energy)
             and within(max_dist, patient.batch.max_dist)
             and (all(dtw <= patient.batch.d_max_c for dtw in dtws)))
        solution = guess
        result.features.append((solution, guess, psd_1, psd_2, psd_3,
                                energy, max_dist, *dtws))
        if guess:
            if solution:
                tp += 1
            else:
                fp += 1
        else:
            if solution:
                fn += 1
            else:
                tn += 1
    result.pre = PRE(tp, tn, fp, fn)
    result.sen = SEN(tp, tn, fp, fn)
    result.f_1 = F_1(tp, tn, fp, fn)
    return result

def call_energy(signal, mw, stride, operations):
    vE = [np.nan]*operations

    for idx in range(operations):
        window = signal[idx*stride:idx*stride+mw]
        mu = window - np.mean(window)
        vE[idx] = np.sum(mu**2)/mw
    return vE

def dump_output(result : Result, output : str):
    with open(output, "w") as fp:
        print(f"{result.pre} {result.sen} {result.f_1}", file=fp)
        for feature in result.features:
            print(" ".join(map(str, feature)), file=fp)

chbmit_dir = "/home/joseaverde/TFG/reference/CHBMIT"
model_file = ("/home/joseaverde/DatosDeteccionSeizure/aa_ma_params_b3z2mz18ws5.json")
for patient in range(1,8):
    chb = "chb%02d" % patient
    print(f"Processing {chb}")
    print(f"  Loading {chb}")
    patient = load(chb, chbmit_dir, model_file)
    print(f"  Generating batch file")
    dump_batch(patient.batch, f"{chb}.batch")
    print(f"  Generating input file")
    dump_input(patient.signal, f"{chb}.in")
    print(f"  Executing model")
    result = solve(patient)
    print(f"  Generating output file")
    dump_output(result, f"{chb}.out")
    print(f"  Done")
