# *-* encoding=utf8 *-*
"""
@description A Toolbox with several functions.
@author      José Antonio Verde Jiménez
"""

import os
import sys
import numpy as np
import multiprocessing as mp
import time
from typing import Final

CWD       : Final[str]   = os.path.dirname(__file__)
ROOT      : Final[str]   = os.path.dirname(CWD)
GENERATED : Final[str]   = os.path.join(CWD, "generated")
TIMES     : Final[int]   = 4

mw       : Final[int]         = 256 * 5

data_all : Final[list[float]] = np.fromfile(os.path.join(GENERATED, "data_all"))
stride   : Final[int]                  = 256
nchunks  : Final[int]                  = mp.cpu_count()
xiter    : Final[int]                  = (len(data_all)-mw)//stride + 1
chunks   : Final[np.array]             = np.linspace(0, xiter, num=nchunks+1,
                                                     endpoint=True, dtype=int)
dlist    : Final[list[tuple[int,int]]] = list(zip(chunks, chunks[1:]))

query_all : Final[list[float]] = np.fromfile(os.path.join(GENERATED, "query_all"))
qstride   : Final[int]                  = stride
yiter     : Final[int]                  = (len(query_all)-mw)//qstride + 1
qchunks   : Final[np.array]             = np.linspace(0, yiter, num=nchunks+1,
                                                      endpoint=True, dtype=int)
qlist     : Final[list[tuple[int,int]]] = list(zip(qchunks,qchunks[1:]))

sys.path.append(os.path.join(ROOT, "build", "Release", "modules"))
sys.path.append(os.path.join(ROOT))

import signals as cpp
from scipy.integrate import simps as py_simps
from scipy.signal import welch as py_welch
from mylibrary.common import utils as py

def py_bottleneck(data_all, mw, stride, dlist, qstride, query_all, qlist):
    pool = mp.Pool(mp.cpu_count())
    d_aux = pool.starmap(py.call_psd_tri,
        [(data_all[start*stride:stop*stride+mw-1], mw, stride, stop-start)
            for start,stop in dlist])
    d_psd = np.asarray([element for sublist in d_aux for element in sublist])
    d_p1_all = d_psd[:,0]
    d_p2_all = d_psd[:,1]
    d_p3_all = d_psd[:,2]
    # The same for the energy and the max_dist
    d_aux = pool.starmap(py.call_energy,
        [(data_all[start*stride:stop*stride+mw-1], mw, stride, stop-start)
            for start,stop in dlist])
    d_e_all = np.asarray([element for sublist in d_aux for element in sublist])
    d_aux = pool.starmap(py.call_max_dist,
        [(data_all[start*stride:stop*stride+mw-1], mw, stride, stop-start)
            for start,stop in dlist])
    d_d_all = np.asarray([element for sublist in d_aux for element in sublist])

    # The same for the query
    q_aux = pool.starmap(py.call_psd_tri,
        [(query_all[start*qstride:stop*qstride+mw-1], mw, qstride, stop-start)
            for start,stop in qlist])
    q_psd = np.asarray([element for sublist in q_aux for element in sublist])
    q_p1_all = q_psd[:,0]
    q_p2_all = q_psd[:,1]
    q_p3_all = q_psd[:,2]
    # And the energy and the max_dist of the query
    q_aux = pool.starmap(py.call_energy,
        [(query_all[start*qstride:stop*qstride+mw-1], mw, qstride, stop-start)
            for start,stop in qlist])
    q_e_all = np.asarray([element for sublist in q_aux for element in sublist])
    q_aux = pool.starmap(py.call_max_dist,
        [(query_all[start*qstride:stop*qstride+mw-1], mw, qstride, stop-start)

            for start,stop in qlist])
    q_d_all = np.asarray([element for sublist in q_aux for element in sublist])
    pool.close()
    return ((d_p1_all, d_p2_all, d_p3_all, d_e_all, d_d_all),
            (q_p1_all, q_p2_all, q_p3_all, q_e_all, q_d_all))

