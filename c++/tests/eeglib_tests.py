import sys
import os
CWD = os.path.dirname(__file__)
ROOT = os.path.dirname(CWD)
sys.path.append(os.path.join(ROOT, "build", "Release", "modules"))
sys.path.append(os.path.join(ROOT))
import EEGLIB
import numpy as np

# Datos de prueba
x = 1
nS=6400 * x
nQ=320 * x
np.random.seed(42)
S = np.cumsum(np.random.uniform(-0.5, 0.5, nS))
Q = np.cumsum(np.random.uniform(-0.5, 0.5, nQ))
S = S.astype(dtype=np.float32)
Q = Q.astype(dtype=np.float32)

stride = 8
sEpoPat = 16
w=8

## Z-normalized, pattern=symmetric1 and square root
distMtx_ = EEGLIB.GetDistMtx_(S, nS, Q, nQ, sEpoPat, stride, w, False)
distMtx = EEGLIB.GetDistMtx(S, nS, Q, nQ, sEpoPat, stride, w, False)
print(distMtx_[:10])
print(distMtx[:10])

dM=np.array(distMtx)
np.set_printoptions(formatter={'float': lambda x: "{0:0.2f}".format(x)})
print("Distance Matrix partial output: {}".format(dM[:5]))

## Un-z-normalized, pattern=symmetric2 and square root
distMtx2_ = EEGLIB.GetDistMtxU_(S, nS, Q, nQ, sEpoPat, stride, w, False)
distMtx2 = EEGLIB.GetDistMtxU(S, nS, Q, nQ, sEpoPat, stride, w, False)

dM2=np.array(distMtx2)
print("Distance Matrix partial output: {}".format(dM2[:5]))

## Un-z-normalized, pattern=symmetric2 and no square root
distMtx3_ = EEGLIB.GetDistMtxUNS_(S, nS, Q, nQ, sEpoPat, stride, w, False)
distMtx3 = EEGLIB.GetDistMtxUNS(S, nS, Q, nQ, sEpoPat, stride, w, False)

dM3=np.array(distMtx3)
print("Distance Matrix partial output: {}".format(dM3[:5]))

nEpo=(nS-sEpoPat)//stride +1
nPat=(nQ-sEpoPat)//stride +1
nEpoPat=nEpo*nPat
nEpoPat

distMtx=np.reshape(dM,(nEpo,nPat)).T/(2*sEpoPat)
distMtxU=np.reshape(dM2,(nEpo,nPat)).T/(2*sEpoPat)
distMtxUNS=np.reshape(dM3,(nEpo,nPat)).T/(2*sEpoPat)
distMtx.shape

from dtw import *

distMtxP=np.zeros((nEpo, nPat))
for i in range(nEpo):
    for j in range(nPat):
        distMtxP[i,j]=dtw(S[i*stride:i*stride+sEpoPat], Q[j*stride:j*stride+sEpoPat],dist_method='euclidean',
                               # step_pattern='symmetric1', #by default is symmetric2
                                window_type='sakoechiba',
                                window_args={'window_size':w},
                                keep_internals=False, 
                                distance_only=True).normalizedDistance #.distance
distMtxP=distMtxP.T

import matplotlib.pyplot as plt
#Configuration variables
titlefs = 20
ylabelfs = 18
xlabelfs = 18
xticksfs = 16
yticksfs = 16
legendfs = 14
linew = 1

fig = plt.figure()

marks=['o-','x-','s-','v-','+-']

plt.plot(np.arange(nEpo), distMtx[0,:], linewidth=linew)
plt.plot(np.arange(nEpo), distMtxU[0,:], linewidth=linew)
plt.plot(np.arange(nEpo), distMtxUNS[0,:], linewidth=linew)
plt.plot(np.arange(nEpo), distMtxP[0,:], linewidth=linew)

leg=['UCR','UCR Un-z-normalized symmetric2','UCR Un-z-normalized symmetric2 no sqrt','DTW-Python().normalizedDistance'] 
plt.title('Comparison',  fontweight='bold', fontsize=titlefs)
plt.legend(leg,loc='best', fontsize= legendfs)
plt.ylabel('Distance', fontsize=ylabelfs)
plt.xlabel('Epoch ID', fontsize=xlabelfs)
plt.yticks(fontsize=yticksfs)
plt.grid()

plt.show()

fig = plt.figure()

plt.plot(np.arange(nEpo), distMtxU[0,:], linewidth=linew)
plt.plot(np.arange(nEpo), distMtxP[0,:], linewidth=linew)

leg=['UCR Un-z-normalized symmetric2','DTW-Python().normalizedDistance']
plt.title('Comparison',  fontweight='bold', fontsize=titlefs)
plt.legend(leg,loc='best', fontsize= legendfs)
plt.ylabel('Distance', fontsize=ylabelfs)
plt.xlabel('Epoch ID', fontsize=xlabelfs)
plt.yticks(fontsize=yticksfs)
plt.grid()

plt.show()

print(np.allclose(distMtxU, distMtxP, rtol=1e-05, atol=1e-08, equal_nan=False))
