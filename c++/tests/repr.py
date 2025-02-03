from typing import Final
import os
import sys

root : Final[str] = os.path.dirname(os.path.dirname(sys.argv[0]))
sys.path.append(root)
sys.path.append(os.path.join(root, "build", "Release", "modules"))

import EEGLIB
import signals
from mylibrary.optimizer.functions import f_validation
from mylibrary.common.classes import DTW_Matrix
import mylibrary.common.config as config
from mne.filter import filter_data
import pandas as pd
import numpy as np
import csv
import json

print("Seizure Algorithm")
print("=================")

Channel = np.array
patient    : Final[str]   = "chb03"
signal     : Final[str]   = "F3-C3"

chbmit     : Final[str]   = os.path.join(root, "CHBMIT")
samplerate : Final[int]   = 256
filtered   : Final[bool]  = True
lowcut     : Final[float] = 0.5
highcut    : Final[float] = 70.0
json_file  : Final[str]   = os.path.join(root, "datos",
                                         "DatosDeteccionSeizure",
                                         "aa_ma_params_b3z2mz18ws5.json")
csv_file   : Final[str]   = os.path.join(chbmit, patient,
                                         f"{patient}_seizure_table.csv")
print(f"""
Root directory   : {root}
CHBMIT directory : {chbmit}
Patient name     : {patient}
Signal name      : {signal}
Sample rate      : {samplerate}
Is filtered?     : {filtered}
Band pass        : [{lowcut}, {highcut}] Hz
JSON file        : {json_file}
CSV file         : {csv_file}""")

# ==== Load the signal ====================================================== #

dfi = pd.read_hdf(f'{chbmit}/{patient}/signal_{patient}.h5', key='data_frame')
ts = dfi[signal].values.reshape(1,-1)
if filtered:
    Scv = filter_data(ts, samplerate, lowcut, highcut, verbose=False)[0]
else:
    Scv = ts[0]


# ==== Compute the DoNotCompute vector ====================================== #

stride      : Final[int] = 256
epoch_size  : Final[int] = stride * 5
query_size  : Final[int] = epoch_size
epoch_count : Final[int] = (len(Scv) - epoch_size) // stride + 1
print(f"""
Stride size : {stride} samples
Epoch size  : {epoch_size} samples
Epoch count : {epoch_count} epochs""")

DNCs = [False for i in range(epoch_count)]

# ==== Compute features ===================================================== #

class Batch:
    psd_1    : tuple[float, float] = (-np.inf, np.inf)
    psd_2    : tuple[float, float] = (-np.inf, np.inf)
    psd_3    : tuple[float, float] = (-np.inf, np.inf)
    max_dist : tuple[float, float] = (-np.inf, np.inf)
    energy   : tuple[float, float] = (-np.inf, np.inf)
    d_max_c  : float               = np.inf
    pj       : Channel
    lids     : list[int]           = [] # Pattern indices
    def __init__(self, Scv : Channel, patient : str, json_file : str):
        with open(json_file, "r") as fp:
            data = json.load(fp)
        self.psd_1 = (data[patient]["p1_min"], data[patient]["p1_max"])
        self.psd_2 = (data[patient]["p2_min"], data[patient]["p2_max"])
        self.psd_3 = (data[patient]["p3_min"], data[patient]["p3_max"])
        self.max_dist = (data[patient]["d_min"], data[patient]["d_max"])
        self.energy = (data[patient]["e_min"], data[patient]["e_max"])
        self.d_max_c = data[patient]["dmax"]
        index = int(data[patient]["batch"]) * stride
        self.pj = Scv[index : index + query_size]
        self.lids = np.array([index])

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

    def get_bounds (self) -> list[tuple[int, int]]:
        return list(self.__bounds)

batch      : Final[Batch]                     = Batch(Scv, patient, json_file)
psd_ranges : Final[list[tuple[float, float]]] = [(2.5, 12.0), (12.0, 18.0),
                                                 (18.0, 35.0)]
win_size   : Final[int]                       = 1024    # FIXME: Maybe 1280
ops        : Final[int]                       = epoch_count
metadata   : Final[Metadata]                  = Metadata(csv_file)
print(f"""
PSD ranges  : {psd_ranges}
Window Size : {win_size}
Operations  : {ops}
{batch}""")
psd_1, psd_2, psd_3 = signals.call_psd(Scv, win_size, stride, ops, psd_ranges)
energy = signals.call_energy(Scv, epoch_size, stride, ops)
max_dist = signals.call_max_dist(Scv, epoch_size, stride, ops)

# ==== DTW ================================================================== #

query_all = batch.pj

yiter    : Final[int]        = (len(query_all) - epoch_size) // stride + 1
max_warp : Final[int]        = 16
mDTW     : Final[DTW_Matrix] = DTW_Matrix(0, yiter, 0, epoch_count)
config.mDTW = mDTW

cdata = Scv.astype(dtype=np.float32)
cquery = query_all.astype(dtype=np.float32)

print(f"""
yiter    = {yiter}
Max warp = {max_warp}""")

all_distances = EEGLIB.GetDistMtx(
        cdata,  len(cdata),
        cquery, len(cquery),
        epoch_size, stride, max_warp, True)
distMtx = np.reshape(all_distances, (epoch_count, yiter)).transpose()
mDTW.data = distMtx

# ==== Find pattern ========================================================= #

lookbackwards : Final[int]     = 1
fth           : Final[Channel] = np.arange(1.0, 2.0, 0.05, dtype=float)
exclusion     : Final[int]     = 10 # FIXME: Maybe 10 * 256

bounds = list(map(lambda p : (p[0] // stride, p[1] // stride),
                  metadata.get_bounds()))
idx_seizure_list = []
last = 0
for item in bounds:
    idx_seizure_list.append((last, item[0]))
    idx_seizure_list.append((item[0], item[1]))
    last = item[1]
idx_seizure_list.append((last, -1))

batch.lids = [0]

print(f"""
Look Backwards  = {lookbackwards}
Pattern Id List = {batch.lids}
th              = {batch.d_max_c}
fth             = {fth}
Exclusion       = {exclusion} samples
Seizure bounds  = {idx_seizure_list}""")

# FIXME: Possible problems in `lids' because the ids are from the bounds

result = f_validation(
    rp1_min          = batch.psd_1[0],    rp1_max = batch.psd_1[1],
    rp2_min          = batch.psd_2[0],    rp2_max = batch.psd_2[1],
    rp3_min          = batch.psd_3[0],    rp3_max = batch.psd_3[1],
    rd_min           = batch.max_dist[0], rd_max  = batch.max_dist[1],
    re_min           = batch.energy[0],   re_max  = batch.energy[1],
    lookbackwards    = lookbackwards,
    lids             = batch.lids,
    th               = batch.d_max_c,
    fth              = fth,
    exclusion        = exclusion,
    xiter            = epoch_count,
    d_p1             = psd_1,
    d_p2             = psd_2,
    d_p3             = psd_3,
    d_d              = max_dist,
    d_e              = energy,
    idx_seizure_list = idx_seizure_list,
    DoNotCompute     = DNCs)

for (tp, tn, fp, fn) in result:
    pre = tp / (tp + fp)
    sen = tp / (tp + fn)
    f_1 = 2 * (pre * sen) / (pre + sen)
    pre = round(100 * pre, 2)
    sen = round(100 * sen, 2)
    f_1 = round(100 * f_1, 2)
    print(f"\n\033[1mEVAL: TP={tp}, TN={tn}; FP={fp}, FN={fn}") 
    print(f"   PRE={pre}%, SEN={sen}%, F_1={f_1}%\033[0m")
print()
