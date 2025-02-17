import numpy as np
from scipy.integrate import simps
from scipy.signal import welch

def call_max_dist(signal, mw, stride, operations):
    vD = [np.nan]*operations

    for idx in range(operations):
        window = signal[idx*stride:idx*stride+mw]
        mu = window - np.mean(window)
        vD[idx] = max(mu) - min(mu)
    return vD

def call_energy(signal, mw, stride, operations):
    vE = [np.nan]*operations

    for idx in range(operations):
        window = signal[idx*stride:idx*stride+mw]
        mu = window - np.mean(window)
        vE[idx] = np.sum(mu**2)/mw
    return vE

def calc_delta_tri(x):    
    # nperseg=512 because I want to choose 2.5Hz
    freqs, Pxx = welch(x, 256., nperseg=512)
    idx_delta1 = np.logical_and(freqs >= 2.5, freqs < 12.)
    idx_delta2 = np.logical_and(freqs >= 12., freqs < 18.)
    idx_delta3 = np.logical_and(freqs >= 18., freqs < 35.)
    # Frequency resolution
    freq_res = freqs[1] - freqs[0]  # = 1 / 4 = 0.25
    # Compute the absolute power by approximating the area under the curve
    return [simps(Pxx[idx_delta1], dx=freq_res),
            simps(Pxx[idx_delta2], dx=freq_res),
            simps(Pxx[idx_delta3], dx=freq_res)]

def call_psd_tri(signal, mw, stride, operations):
    pP = [[np.nan]*3]*operations

    for idx in range(operations):
        pP[idx] = calc_delta_tri(signal[idx*stride:idx*stride+mw])
    return pP
