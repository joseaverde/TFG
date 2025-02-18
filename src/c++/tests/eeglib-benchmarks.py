#!/usr/bin/env  python3
# *-* encoding=utf8 *-*

import toolbox
import EEGLIB
from benchmarks import benchmark, Subbenchmark, ignore
import EEGLIB
import numpy as np

class NewVsOld():
    def fields(self) -> tuple[str, str]:
        return ("New", "Old")
    def base (self) -> int:
        return 1

class BGetDistMtx():
    def funcs (self) -> tuple:
        return (EEGLIB.GetDistMtx, EEGLIB.GetDistMtx_)

class BGetDistMtxU():
    def funcs (self) -> tuple:
        return (EEGLIB.GetDistMtxU, EEGLIB.GetDistMtxU_)

class BGetDistMtxUNS():
    def funcs (self) -> tuple:
        return (EEGLIB.GetDistMtxUNS, EEGLIB.GetDistMtxUNS_)

def create(nS, nQ):
    np.random.seed(42)
    S = np.cumsum(np.random.uniform(-0.5, 0.5, nS))
    Q = np.cumsum(np.random.uniform(-0.5, 0.5, nQ))
    stride = 8
    sEpoPat = 16
    w = 8
    verbose = 0
    return (S, nS, Q, nQ, sEpoPat, stride, w, verbose)

class BSmall():
    def params (self):
        return create(6400, 320)

class BNormal():
    def params (self):
        return create(6400 * 4, 320 * 4)

class BHuge():
    def params (self):
        return create(6400 * 16, 320 * 16)

@benchmark(2.0)
class BenchmarkEEGLIB:
    class BenchmarkSmallGetMtx(NewVsOld, BGetDistMtx, BSmall): pass
    class BenchmarkNormalGetMtx(NewVsOld, BGetDistMtx, BNormal): pass
    class BenchmarkHugeGetMtx(NewVsOld, BGetDistMtx, BHuge): pass

    class BenchmarkSmallGetMtxU(NewVsOld, BGetDistMtxU, BSmall): pass
    class BenchmarkNormalGetMtxU(NewVsOld, BGetDistMtxU, BNormal): pass
    class BenchmarkHugeGetMtxU(NewVsOld, BGetDistMtxU, BHuge): pass

    class BenchmarkSmallGetMtxUNS(NewVsOld, BGetDistMtxUNS, BSmall): pass
    class BenchmarkNormalGetMtxUNS(NewVsOld, BGetDistMtxUNS, BNormal): pass
    class BenchmarkHugeGetMtxUNS(NewVsOld, BGetDistMtxUNS, BHuge): pass

if __name__ == "__main__":
    benchmarks = BenchmarkEEGLIB()
    benchmarks.run()
