import numpy
from mylibrary.optimizer.functions import f_validation
import real_data_test_helper
import signals

mw = 5 * 256
stride = 256
ranges = ((2.5, 12.0), (12.0, 18.0), (18.0, 35.0))
data_all = get_Scv("CHBMIT", "chb01", "F3-C3", sample_rate, filtered)
nchunks = 16
xiter = (len(data_all) - mw) // stride + 1
chunks = numpy.linspace(0, xiter, num = nchunks + 1, endpoint=True, dtype=int)
dlist = list(zip(chunks, chunks[1:]))

data_args = (data_all, mw, stride, dlist[-1][1] - dlist[0][0])

sample_rate = 256
filtered = True

rp1_min, rp1_max = 6929.454570722453, 53164.80932408748
rp2_min, rp2_max = 239.48039056290412, 533.3331593090345
rp3_min, rp3_max = 211.3964813369833, 562.766638425254
rd_min,  rd_max  = 1150.59419837107, 1628.730057178081
re_min,  re_max  = 21330.545279324462, 110283.24342497626

# lookbackwards = ???
# lids = ???
th = 1.05
# fth = ???
# exclusion = ???
d_p1, d_p2, d_p3 = signals.call_psd(*args, freq_ranges)
d_d = signals.call_max_dist(*args)
d_e = signals.call_energy(*args)
# idx_seizure_list = ???
DoNotCompute = [False]

