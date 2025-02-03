#!/usr/bin/env  python3
# *-* encoding=utf8 *-*

import toolbox
import numpy as np
import multiprocessing as mp
from typing import Final
from benchmarks import benchmark, Subbenchmark, ignore

import signals as cpp
from scipy.integrate import simps as py_simps
from scipy.signal import welch as py_welch
from mylibrary.common import utils as py

data_args = (toolbox.data_all, toolbox.mw, toolbox.stride,
    toolbox.dlist[-1][1] - toolbox.dlist[0][0])
query_args = (toolbox.query_all, toolbox.mw, toolbox.qstride,
    toolbox.qlist[-1][1] - toolbox.qlist[0][0])
freq_ranges : Final[list[tuple[float, float]]] = \
    [(2.5, 12.0), (12.0, 18.0), (18.0, 35.0)]

print(toolbox.mw)
print(toolbox.stride)

class CXXvsPython(Subbenchmark):
    def fields(self) -> tuple[str, str]:
        return ("C++", "Python")
    def base (self) -> int:
        return 1

class CallFunctions(CXXvsPython):
    def params(self) -> tuple:
        return data_args

def parallel(func):
    def inner(signal, mw, stride, _):
        pool = mp.Pool(mp.cpu_count())
        aux = pool.starmap(func,
            [(signal[start*stride:stop*stride+mw-1], mw, stride, stop-start)
                for start,stop in toolbox.dlist])
        return np.asarray([element for sublist in aux for element in sublist])
    return inner

def split_psd(arr):
    return (arr[:,0], arr[:,1], arr[:2])

@benchmark(30.0)
class BenchmarkSignals:
    class BenchmarkSimpson(CXXvsPython):
        def params(self) -> tuple[np.array, float]:
            return (toolbox.data_all, 0.5)
        def funcs(self) -> tuple:
            return (cpp.simpson, lambda a, b : py_simps(a, dx=b))
    class BenchmarkWelch(CXXvsPython):
        def params(self) -> tuple[np.array, float, int]:
            return (toolbox.data_all, 256.0, 512, 256)
        def funcs(self) -> tuple:
            return (cpp.welch, lambda a, b, c, d : py_welch(a, b, nperseg=c, noverlap=d))
    class BenchmarkCallPSD(CallFunctions):
        def funcs(self) -> tuple:
            return (lambda *args : cpp.call_psd(*args, freq_ranges),
                    lambda *args : split_psd(parallel(py.call_psd_tri)(*args)))
    class BenchmarkEnergy(CallFunctions):
        def funcs(self) -> tuple:
            return (cpp.call_energy,
                    parallel(py.call_energy))
    class BenchmarkCallMaxDist(CallFunctions):
        def funcs(self) -> tuple:
            return (cpp.call_max_dist,
                    parallel(py.call_max_dist))
    class BenchmarkAll(CXXvsPython):
        def funcs(self) -> tuple:
            return (lambda da, mw, ds, dl, qs, qa, ql :
                        cpp.begin(da, ds, dl[-1][1] - dl[0][0],
                                  qa, qs, ql[-1][1] - ql[0][0], mw),
                    toolbox.py_bottleneck)
        def params(self) -> tuple:
            return (toolbox.data_all, toolbox.mw, toolbox.stride, toolbox.dlist,
                    toolbox.qstride, toolbox.query_all, toolbox.qlist)

if __name__ == "__main__":
    benchmarks = BenchmarkSignals()
    benchmarks.run()
