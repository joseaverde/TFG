import numpy as np
import real_data_test_helper as real_data
import itertools
import time
from typing import Final
import csv
import json
import os
import sys

CWD  : Final[str] = os.path.dirname(__file__)
ROOT : Final[str] = os.path.dirname(CWD)
sys.path.append(os.path.join(ROOT, "build", "Release", "modules"))

import signals
import EEGLIB

Channel = np.array
win_size    : Final[int] = 1024
stride      : Final[int] = 256
query_size  : Final[int] = 1280
warping_win : Final[int] = 16

class SlidingWindow:
    def __init__(self, channel : Channel, size : int, stride : int):
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

class Batch:
    psd_1    : tuple[float, float] = (-np.inf, np.inf)
    psd_2    : tuple[float, float] = (-np.inf, np.inf)
    psd_3    : tuple[float, float] = (-np.inf, np.inf)
    max_dist : tuple[float, float] = (-np.inf, np.inf)
    energy   : tuple[float, float] = (-np.inf, np.inf)
    d_max_c  : float               = np.inf
    pj       : Channel
    def __init__(self, scv : Channel, patient : str, json_file : str, worst : bool):
        with open(json_file, "r") as fp:
            data = json.load(fp)
        if not worst:
            self.psd_1 = (data[patient]["p1_min"], data[patient]["p1_max"])
            self.psd_2 = (data[patient]["p2_min"], data[patient]["p2_max"])
            self.psd_3 = (data[patient]["p3_min"], data[patient]["p3_max"])
            self.max_dist = (data[patient]["d_min"], data[patient]["d_max"])
            self.energy = (data[patient]["e_min"], data[patient]["e_max"])
        self.d_max_c = data[patient]["dmax"]
        index = int(data[patient]["batch"]) * stride
        self.pj = scv[index : index + query_size]

    def __repr__ (self):
        return ( f"Batch [\n"
               + f"   PSD (1)  : {self.psd_1}\n"
               + f"   PSD (2)  : {self.psd_2}\n"
               + f"   PSD (3)  : {self.psd_3}\n"
               + f"   Distance : {self.max_dist}\n"
               + f"   Energy   : {self.energy}\n"
               + f"   D_max_c  : {self.d_max_c}\n"
               + f"];")

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

def distance (signal : Channel, query : Channel, mx : float):
    result = EEGLIB.GetDistMtx(signal, len(signal), query, len(query),
                               win_size, stride, warping_win, 0)
    return result

def min_distance (signal : Channel, batch : Batch, mx : float):
    mn : Final[float] = float("inf")
    return min(mn, *distance(batch.pj, signal, mx))

def validation (scv   : Channel, dncs  : Channel, metadata : Metadata,
                batch : Batch,   epoch : int):
    ops = (len(scv) - win_size) // stride + 1
    psd_1, psd_2, psd_3 = signals.call_psd(scv, win_size, stride, ops,
                                           ((2.5, 12.0), (12.0, 18.0), (18.0, 35.0)))
    energy = signals.call_energy(scv, win_size, stride, ops)
    max_dist = signals.call_max_dist(scv, win_size, stride, ops)
    def in_range(idx : int):
        return (batch.psd_1[0] <= psd_1[idx] <= batch.psd_1[1]
            and batch.psd_2[0] <= psd_2[idx] <= batch.psd_2[1]
            and batch.psd_3[0] <= psd_3[idx] <= batch.psd_3[1]
            and batch.max_dist[0] <= max_dist[idx] <= batch.max_dist[1]
            and batch.energy[0] <= energy[idx] <= batch.energy[1])

    mx : Final[float] = 1.05 * batch.d_max_c
    tp = fp = tn = fn = 0
    for (view, idx, dnc) in \
            zip(SlidingWindow(scv, win_size, stride), itertools.count(epoch), dncs):
        if dnc or not in_range(idx - epoch):
            continue
        dist = min_distance(view, batch, mx)
        if dist < mx:
            if metadata.is_seizure(idx):
                tp += 1
            else:
                fp += 1
        else:
            if metadata.is_seizure(idx):
                fn += 1
            else:
                tn += 1

    print("   True positives  ", tp)
    print("   False positives ", fp)
    print("   True negatives  ", tn)
    print("   False negatives ", fn)
    return (tp, fp, tn, fn)


scv = real_data.get_Scv("CHBMIT", "chb01", "F3-C3", 256, True)
# dncs = itertools.cycle([False])
dncs = [False for i in range(len(scv))]
metadata = Metadata("CHBMIT/chb01/chb01_seizure_table.csv")
batch = Batch(scv, "chb01",
              "datos/DatosDeteccionSeizure/aa_ma_params_b3z2mz18ws5.json",
              True)
print(batch)

if __name__ == "__main__":
    start = time.time()
    validation(scv, dncs, metadata, batch, 0)
    stop = time.time()
    print("Elapsed: ", stop - start, "s")


# C++:    176ms
# Python: 230ms
