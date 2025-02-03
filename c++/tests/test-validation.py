import numpy as np
import itertools
import time
from typing import Final
import csv
import json
import os
import sys

CWD  : Final[str] = os.path.dirname(__file__)
ROOT : Final[str] = os.path.dirname(CWD)
sys.path.append(ROOT)
sys.path.append(os.path.join(ROOT, "build", "Release", "modules"))

import signals
import EEGLIB
import matplotlib.pyplot as plt
import real_data_test_helper as real_data
from mylibrary.common.utils import call_energy as old_call_energy
from mylibrary.common.utils import call_max_dist as old_call_max_dist
from mylibrary.optimizer.functions import f_validation
from mylibrary.common.classes import DTW_Matrix
from mylibrary.dtw_functions.find_seizure import find_validation_params, call_dnc_aware_full
import mylibrary.common.config as config

Channel = np.array
win_size    : Final[int] = 1280
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
    # TODO: Multiple indices
    psd_1    : tuple[float, float] = (-np.inf, np.inf)
    psd_2    : tuple[float, float] = (-np.inf, np.inf)
    psd_3    : tuple[float, float] = (-np.inf, np.inf)
    max_dist : tuple[float, float] = (-np.inf, np.inf)
    energy   : tuple[float, float] = (-np.inf, np.inf)
    d_max_c  : float               = np.inf
    pj       : Channel
    lids     : list[int]           = [] # Pattern indices
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
        self.lids = [index]

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

def get_ranges_of (data : list[bool], offset : int) -> list[tuple[int, int]]:
    last = False;
    result = []
    for i in range(len(data)):
        if data[i] and not last:
            result.append([i + offset, -1])
        elif not data[i] and last:
            result[-1][-1] = i + offset
        last = data[i]
    if last:
        result[-1][-1] = offset + len(data)
    return result

def all_features(scv : Channel, metadata : Metadata, batch : Batch, epoch : int):
    mx : Final[float] = 1.05 * batch.d_max_c
    ops = (len(scv) - win_size) // stride + 1
    psd_1, psd_2, psd_3 = signals.call_psd(scv, win_size, stride, ops,
                                           ((2.5, 12.0), (12.0, 18.0), (18.0, 35.0)))
    energy    = signals.call_energy(scv, win_size, stride, ops)
    max_dist  = signals.call_max_dist(scv, win_size, stride, ops)
    min_dists = np.array(list(map(lambda x : min_distance(x, batch, mx),
                                  SlidingWindow(scv, win_size, stride))))
    indices   = list(range(epoch, epoch + len(min_dists)))
    bools     = np.array(list(map(metadata.is_seizure, indices)))
    spans     = get_ranges_of(bools, epoch)
    in_range  = np.array([True for i in indices])
    for data, (min, max) in [(psd_1, batch.psd_1),
                             (psd_2, batch.psd_2),
                             (psd_3, batch.psd_3),
                             (energy, batch.energy),
                             (max_dist, batch.max_dist)]:
        plt.axhline(y=min, color='r', linestyle='-')
        plt.plot(indices, data)
        in_range = in_range & np.array([min <= x <= max for x in data])
        plt.axhline(y=max, color='r', linestyle='-')
        plt.ylim(min / 1.1, max * 1.1)
        for (span_l, span_h) in spans:
            plt.axvspan(span_l, span_h, facecolor='r', alpha=0.5)
        plt.figure()

    in_range_ranges = get_ranges_of(in_range, epoch)
    for (span_l, span_h) in spans:
        plt.axvspan(span_l, span_h, facecolor='r', alpha=0.5)
    for (span_l, span_h) in in_range_ranges:
        plt.axvspan(span_l, span_h, facecolor='b', alpha=0.5)
    plt.figure()

    plt.plot(indices, min_dists)
    plt.axhline(y=mx, color='g', linestyle='-')
    for (span_l, span_h) in spans:
        plt.axvspan(span_l, span_h, facecolor='r', alpha=0.5)
    for (span_l, span_h) in in_range_ranges:
        plt.axvspan(span_l, span_h, facecolor='b', alpha=0.5)
    plt.show()

def check_features(scv, metadata, batch, epoch):
    ops = (len(scv) - win_size) // stride + 1
    epoch_count = len(list(SlidingWindow(scv, win_size, stride)))
    indices = list(range(epoch, epoch + epoch_count))
    bools = np.array(list(map(metadata.is_seizure, indices)))
    psd_1, psd_2, psd_3 = signals.call_psd(scv, win_size, stride, ops,
                                           ((2.5, 12.0), (12.0, 18.0), (18.0, 35.0)))
    energy    = signals.call_energy(scv, win_size, stride, ops)
    max_dist  = signals.call_max_dist(scv, win_size, stride, ops)
    features = {"psd1" : (batch.psd_1, psd_1),
                "psd2" : (batch.psd_2, psd_2),
                "psd3" : (batch.psd_3, psd_3),
                "enrg" : (batch.energy, energy),
                "mxdt" : (batch.max_dist, max_dist)}
    count_them = lambda x : sum(map(lambda x : 1 if x else 0, x))
    for name, pair in features.items():
        (span, data) = pair
        test = np.array([span[0] <= x <= span[1] for x in data])
        tp = count_them(bools & test)
        fp = count_them(~bools & test)
        tn = count_them(~bools & ~test)
        fn = count_them(bools & ~test)
        print(name, tp, fp, tn, fn)

def score (tp, fp, tn, fn):
    print(f"True Positives:   {tp}")
    print(f"False Positives:  {fp}")
    print(f"True Negatives:   {tn}")
    print(f"False Negatives:  {fn}")
    pre = tp / (tp + fp) if tp + fp != 0 else float("inf")
    sen = tp / (tp + fn) if tp + fn != 0 else float("inf")
    f_1 = 2 * (pre * sen) / (pre + sen) if pre + sen != 0 else float("inf")
    print("PRE: %.4f%%" % (100 * pre))
    print("SEN: %.4f%%" % (100 * sen))
    print("F_1: %.4f%%" % (100 * f_1))


patient = "chb03"
signal = "F3-C3"

scv = real_data.get_Scv("CHBMIT", patient, signal, 256, False)
# dncs = itertools.cycle([False])
xiter = (len(scv) - win_size) // stride + 1
dncs = [False for i in range(xiter)]
metadata = Metadata(f"CHBMIT/{patient}/{patient}_seizure_table.csv")
batch = Batch(scv, patient,
              "datos/DatosDeteccionSeizure/aa_ma_params_b3z2mz18ws5.json",
              False)
print(batch)
print()

if __name__ == "__main__":
    start = time.time()
    scores = validation(scv, dncs, metadata, batch, 0)
    stop = time.time()
    print("Time: ", stop - start)
    print(score(*scores))
    print("\n------------\n")

    def within(x, pair):
        return pair[0] <= x < pair[1]
    print("Computing idx_seizure_list")
    bounds = list(map(lambda p : (p[0] // stride, p[1] // stride),
                      metadata.get_bounds()))
    idx_seizure_list = []
    last = 0
    for item in bounds:
        idx_seizure_list.append((last, item[0]))
        idx_seizure_list.append((item[0], item[1]))
        last = item[1]
    idx_seizure_list.append((last, -1))
    print("   bounds = ", bounds)
    print("   indices = ", idx_seizure_list)
    xiter    = (len(scv) - win_size) // stride + 1
    seizures = np.asarray([
        any(map(lambda bound : within(index, bound), bounds))
            for index in range(xiter)])
    print()

    print("Computing features...")
    ops = (len(scv) - win_size) // stride + 1
    psd_1, psd_2, psd_3 = signals.call_psd(scv, win_size, stride, ops,
                                           ((2.5, 12.0), (12.0, 18.0),
                                            (18.0, 35.0)))
    energy = old_call_energy(scv, win_size, stride, ops)
    max_dist = old_call_max_dist(scv, win_size, stride, ops)

    b_psd_1 = np.asarray([not within(x, batch.psd_1)    for x in psd_1])
    b_psd_2 = np.asarray([not within(x, batch.psd_2)    for x in psd_2])
    b_psd_3 = np.asarray([not within(x, batch.psd_3)    for x in psd_3])
    b_energ = np.asarray([not within(x, batch.energy)   for x in energy])
    b_mxdst = np.asarray([not within(x, batch.max_dist) for x in max_dist])
    total   = b_psd_1 | b_psd_2 | b_psd_3 | b_energ | b_mxdst
    dncs    = total
    overlap = ~total & seizures

    not_filtered = [(np.count_nonzero(b), 100*np.count_nonzero(b)/len(b))
                        for b in [b_psd_1, b_psd_2, b_psd_3,
                                  b_energ, b_mxdst, total,
                                  seizures]]
    print(f"   PSD_1: %d (%.2f%%) filtered" % not_filtered[0])
    print(f"   PSD_2: %d (%.2f%%) filtered" % not_filtered[1])
    print(f"   PSD_3: %d (%.2f%%) filtered" % not_filtered[2])
    print(f"   ENERG: %d (%.2f%%) filtered" % not_filtered[3])
    print(f"   MXDST: %d (%.2f%%) filtered" % not_filtered[4])
    print(f"   TOTAL: %d (%.2f%%) filtered" % not_filtered[5])
    print(f"   ---");
    print(f"   SEZRS: %d epochs" % (np.count_nonzero(seizures)))
    print(f"   OVRLP: %d (%.2f%%) seizure epochs covered" %
            (np.count_nonzero(overlap),
             100 * np.count_nonzero(overlap) / np.count_nonzero(seizures)))
    print()

    print("Computing DTW...")
    th    = batch.d_max_c * 1.05
    xiter = (len(scv) - win_size) // stride + 1
    yiter = (len(batch.pj) - win_size) // stride + 1
    print("   Threshold = ", th)
    # FIXME: Maybe it is not Pj
    DTWs = EEGLIB.GetDistMtx(scv, len(scv), batch.pj, len(batch.pj),
                             win_size, stride, 16, True)
    DTWs = np.reshape(DTWs, (xiter, yiter)).transpose()

    tp, tn, fp, fn = find_validation_params(
                        distances        = DTWs,
                        xiter            = xiter,
                        th               = batch.d_max_c * 1.05,
                        idx_seizure_list = idx_seizure_list,
                        exclusion        = 10)
    score(tp, fp, tn, fn)

    exit()
    # check_features(scv, metadata, batch, 0)
    start = time.time()
    validation(scv, dncs, metadata, batch, 0)
    stop = time.time()
    # print("Elapsed: ", stop - start, "s")
    # all_features(scv, metadata, batch, 0)
    ops = (len(scv) - win_size) // stride + 1
    psd_1, psd_2, psd_3 = signals.call_psd(scv, win_size, stride, ops,
                                           ((2.5, 12.0), (12.0, 18.0),
                                            (18.0, 35.0)))
    # max_dist = signals.call_max_dist(scv, win_size, stride, ops)
    # energy = signals.call_energy(scv, win_size, stride, ops)
    energy = old_call_energy(scv, win_size, stride, ops)
    max_dist = old_call_max_dist(scv, win_size, stride, ops)
    xiter = (len(scv) - win_size) // stride + 1
    yiter = (len(batch.pj) - win_size) // stride + 1
    dncs = [False for y in range(xiter)]
    all_distances = EEGLIB.GetDistMtx(scv, len(scv), batch.pj, len(batch.pj),
                                      win_size, stride, 16, True)
    distMtx = np.reshape(all_distances, (xiter, yiter)).transpose()
    config.mDTW = DTW_Matrix(0, yiter, 0, xiter)
    config.mDTW.data = distMtx

    # distances
    mDTW = config.mDTW.data
    DoNotCompute = np.asarray(call_dnc_aware_full(
        dnc = dncs,
        operations = xiter,
        d_p1 = psd_1,
        d_p2 = psd_2,
        d_p3 = psd_3,
        d_d  = max_dist,
        d_e  = energy,
        psd_th  = batch.psd_1[0],    p1_max = batch.psd_1[1],
        psd2_th = batch.psd_2[0],    p2_max = batch.psd_2[1],
        psd3_th = batch.psd_3[0],    p3_max = batch.psd_3[1],
        d_min   = batch.max_dist[0], d_th   = batch.max_dist[1],
        e_min   = batch.energy[0],   e_th   = batch.energy[1],
        lb      = 1))
    other_dncs = list()
    for i in range(xiter):
        other_dncs.append(not (
                (batch.psd_1[0]    < psd_1[i]    < batch.psd_1[1])
            and (batch.psd_2[0]    < psd_2[i]    < batch.psd_2[1])
            and (batch.psd_3[0]    < psd_3[i]    < batch.psd_3[1])
            and (batch.max_dist[0] < max_dist[i] < batch.max_dist[1])
            and (batch.energy[0]   < energy[i]   < batch.energy[1])))
    other_dncs = np.array(other_dncs)
    print("DoNotCompute...", np.count_nonzero(DoNotCompute), len(DoNotCompute))
    print("dncs...", np.count_nonzero(dncs), len(dncs))
    print("other_dncs...", np.count_nonzero(other_dncs), len(other_dncs))
    # lDTW = np.full((len(batch.lids),xiter), np.nan)
    # lDTW[:, ~DoNotCompute] = mDTW[batch.lids, :][:, ~DoNotCompute]
    # cDTW = np.min(lDTW, axis=0)
    """
    epoch = 0
    indices   = list(range(epoch, epoch + len(all_distances)))
    in_range  = np.array([True for i in indices])
    for data, (min, max) in [(psd_1, batch.psd_1),
                             (psd_2, batch.psd_2),
                             (psd_3, batch.psd_3),
                             (energy, batch.energy),
                             (max_dist, batch.max_dist)]:
        in_range = in_range & np.array([min <= x <= max for x in data])
    """
    distances = list(np.min(distMtx, axis=0))
    the_real_very_good_dncs = dncs
    for i in range(len(the_real_very_good_dncs)):
        if the_real_very_good_dncs[i]:
            distances[i] = np.nan
    distances = np.array(distances)

    # idx_seizure_list
    idx_seizure_list = []
    last = 0
    for item in metadata.get_bounds():
        idx_seizure_list.append((last // stride, item[0] // stride))
        idx_seizure_list.append((item[0] // stride, item[1] // stride))
        last = item[1]
    idx_seizure_list.append((last // stride, -1))

    tp, tn, fp, fn = find_validation_params(
                        distances        = distances,
                        xiter            = xiter,
                        th               = batch.d_max_c * 1.05,
                        idx_seizure_list = idx_seizure_list,
                        exclusion        = 10)

    print(f"True positives   {tp}")
    print(f"False positives  {fp}")
    print(f"True negatives   {tn}")
    print(f"False negatives  {fn}")
    score(tp, fp, tn, fn)

    """
    result = f_validation(
        rp1_min          = batch.psd_1[0],    rp1_max = batch.psd_1[1],
        rp2_min          = batch.psd_2[0],    rp2_max = batch.psd_2[1],
        rp3_min          = batch.psd_3[0],    rp3_max = batch.psd_3[1],
        rd_min           = batch.max_dist[0], rd_max  = batch.max_dist[1],
        re_min           = batch.energy[0],   re_max  = batch.energy[1],
        lookbackwards    = 1,
        lids             = batch.lids,
        th               = batch.d_max_c,
        fth              = np.arange(1.0, 2.0, 0.05, dtype=float),
        exclusion        = 10,
        xiter            = xiter,
        d_p1             = psd_1,
        d_p2             = psd_2,
        d_p3             = psd_3,
        d_d              = signals.call_max_dist(scv, win_size, stride, ops),
        d_e              = signals.call_energy(scv, win_size, stride, ops),
        idx_seizure_list = metadata.get_bounds(),
        DoNotCompute     = dncs)
    """

    # print(result)

