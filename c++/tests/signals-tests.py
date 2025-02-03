#!/usr/bin/env  python3
# *-* encoding=utf8 *-*
"""
@description Unittest for signals modules. The signal module implements 6
functions:
 * simpson(array[float], float) which replaces `scipy.integrate.simps`
 * welch(array[float], float, int, int), replaces `scipy.signal.welch`
 * call_psd(array[float], int, int, int, list[tuple[float, float]]),
   replaces `mylibrary.common.call_psd_tri`
 * call_energy(array[float], int, int, int), replaces
   `mylibrary.common.call_energy`
 * call_max_dist(array[float], int, int, int), replaces
   `mylibrary.common.call_max_dist`
 * begin(array[float], int, int, array[float], int, int, int), which replaces
   the part on the full algorithm which uses the three `call_*` functions.

The output of those functions must be within a maximum error of 1e-2 from the
Python implementation. Some functions have a result closer than 1e-2. All the
tested values can be found in the SignalsTests.epsilons attribute.

@author      José Antonio Verde Jiménez
"""

import toolbox
import unittest
import numpy as np
import random
from typing import Final

import signals as cpp
from scipy.integrate import simps as py_simps
from scipy.signal import welch as py_welch
from mylibrary.common import utils as py

# ======= Signal Tests ====================================================== #

class SignalsTests(unittest.TestCase):
    welch_borders : Final[int] = 2
    epsilons = {
        "simpson"  : (1e-6, 1e-9),
        "welch"    : (1e-2, 1e-2),
        "psd"      : (1e-2, 1e-2),
        "energy"   : (1e-6, 1e-9),
        "max_dist" : (1e-6, 1e-9),
        "same"     : (1e-9, 1e-12),
        "psd-final": (1e-1, 1e-2)
    }
    rngs = [(2.5, 12.0), (12.0, 18.0), (18.0, 35.0)]

    def random_array (self, size : int, seed : int) -> list[float]:
        random.seed(seed)
        return [random.random() for i in range(size)]

    def assertNear(self, name : str, left, right, msg=""):
        msg = f"({name}) not near enough!: " + msg
        self.assertTrue(np.allclose(left, right, *self.epsilons[name]), msg)

    # ======= Simpson Tests ============================================= #

    def test_simpson_empty (self):
        self.assertRaises(ValueError, cpp.simpson, [], 0.5)
    def test_simpson_one_item (self):
        self.assertRaises(ValueError, cpp.simpson, [1], 0.5)
    def test_simpson_two_items (self):
        self.assertRaises(ValueError, cpp.simpson, [1, 2], 0.5)
    def test_simpson_three_items (self):
        arr = [1, 2, 100]
        dx = 1.5
        cpp_val = cpp.simpson(arr, dx)
        py_val = py_simps(arr, dx=dx)
        self.assertNear("simpson", cpp_val, py_val)
    def test_simpson_mod_3_is_0 (self):
        arr = self.random_array(size = 300, seed = 4)
        dx = 0.5
        cpp_val = cpp.simpson(arr, dx)
        py_val = py_simps(arr, dx=dx)
        self.assertNear("simpson", cpp_val, py_val)
    def test_simpson_mod_3_is_1 (self):
        arr = self.random_array(size = 301, seed = 5)
        dx = 0.5
        cpp_val = cpp.simpson(arr, dx)
        py_val = py_simps(arr, dx=dx)
        self.assertNear("simpson", cpp_val, py_val)
    def test_simpson_mod_3_is_2 (self):
        arr = self.random_array(size = 302, seed = 6)
        dx = 0.5
        cpp_val = cpp.simpson(arr, dx)
        py_val = py_simps(arr, dx=dx)
        self.assertNear("simpson", cpp_val, py_val)

    # ======= Welch Tests =============================================== #

    def test_welch_empty (self):
        self.assertRaises(ValueError, cpp.welch, [], 256.0, 100, 50)
    def test_welch_not_enough_items (self):
        self.assertRaises(ValueError, cpp.welch, [0 for i in range(99)], 256.0, 100, 50)
    def test_welch_enough_items (self):
        cpp.welch([0 for i in range(100)], 256.0, 100, 50)
    def test_welch_one_even_window (self):
        win = 300
        freq = 123.0
        arr = self.random_array(size = win + 20, seed = 7)
        cpp_freqs, cpp_Pxx = cpp.welch(arr, freq, win, win // 2)
        py_freqs, py_Pxx = py.welch(arr, freq, nperseg=win, noverlap=win // 2)
        w = self.welch_borders
        self.assertNear("welch", cpp_Pxx[w:-w], py_Pxx[w:-w])
        self.assertNear("same", cpp_freqs, py_freqs)
    def test_welch_length_multiple_of_window (self):
        win = 300
        length = win * 4
        freq = 17.0
        arr = self.random_array(size = length, seed = 8)
        cpp_freqs, cpp_Pxx = cpp.welch(arr, freq, win, win // 2)
        py_freqs, py_Pxx = py.welch(arr, freq, nperseg=win, noverlap=win // 2)
        w = self.welch_borders
        self.assertNear("welch", cpp_Pxx[w:-w], py_Pxx[w:-w])
        self.assertNear("same", cpp_freqs, py_freqs)
    def test_welch_length_multiple_of_window_plus_overlap(self):
        win = 320
        overlap = win // 2 - 20
        length = win * 7 + overlap
        freq = 170.0
        arr = self.random_array(size = length, seed = 9)
        cpp_freqs, cpp_Pxx = cpp.welch(arr, freq, win, overlap)
        py_freqs, py_Pxx = py.welch(arr, freq, nperseg=win, noverlap=overlap)
        w = self.welch_borders
        self.assertNear("welch", cpp_Pxx[w:-w], py_Pxx[w:-w])
        self.assertNear("same", cpp_freqs, py_freqs)
    def test_welch_length_multiple_of_window_plus_1(self):
        win = 300
        length = win * 4 + 1
        freq = 2003.0
        arr = self.random_array(size = length, seed = 10)
        cpp_freqs, cpp_Pxx = cpp.welch(arr, freq, win, win // 2)
        py_freqs, py_Pxx = py.welch(arr, freq, nperseg=win, noverlap=win // 2)
        w = self.welch_borders
        self.assertNear("welch", cpp_Pxx[w:-w], py_Pxx[w:-w])
        self.assertNear("same", cpp_freqs, py_freqs)
    def test_welch_length_multiple_of_window_minus_1(self):
        win = 300
        length = win * 4 - 1
        freq = 3.14159265358
        arr = self.random_array(size = length, seed = 11)
        cpp_freqs, cpp_Pxx = cpp.welch(arr, freq, win, win // 2)
        py_freqs, py_Pxx = py.welch(arr, freq, nperseg=win, noverlap=win // 2)
        w = self.welch_borders
        self.assertNear("welch", cpp_Pxx[w:-w], py_Pxx[w:-w])
        self.assertNear("same", cpp_freqs, py_freqs)
    def test_welch_one_odd_window (self):
        win = 313
        freq = 13.0
        arr = self.random_array(size = win, seed = 12)
        cpp_freqs, cpp_Pxx = cpp.welch(arr, freq, win, win // 2)
        py_freqs, py_Pxx = py.welch(arr, freq, nperseg=win, noverlap=win // 2)
        w = self.welch_borders
        self.assertNear("welch", cpp_Pxx[w:-w], py_Pxx[w:-w])
        self.assertNear("same", cpp_freqs, py_freqs)
    def test_welch_multiple_odd_windows (self):
        win = 313
        length = win * 8
        freq = 1024.25
        arr = self.random_array(size = length, seed = 13)
        cpp_freqs, cpp_Pxx = cpp.welch(arr, freq, win, win // 2)
        py_freqs, py_Pxx = py.welch(arr, freq, nperseg=win, noverlap=win // 2)
        w = self.welch_borders
        self.assertNear("welch", cpp_Pxx[w:-w], py_Pxx[w:-w])
        self.assertNear("same", cpp_freqs, py_freqs)
    def test_welch_overlap_is_zero (self):
        win = 100
        length = 434
        overlap = 0
        freq = 2000.0
        arr = self.random_array(size = length, seed = 14)
        self.assertRaises(ValueError, cpp.welch, arr, freq, win, overlap)
    def test_welch_overlap_is_equal_to_window (self):
        win = 100
        length = 434
        overlap = win
        freq = 2000.0
        arr = self.random_array(size = length, seed = 15)
        self.assertRaises(ValueError, cpp.welch, arr, freq, win, overlap)
    def test_welch_overlap_is_greater_than_window (self):
        win = 100
        length = 434
        overlap = win + 1
        freq = 2000.0
        arr = self.random_array(size = length, seed = 16)
        self.assertRaises(ValueError, cpp.welch, arr, freq, win, overlap)
    def test_welch_overlap_is_negative (self):
        # -1 is not positive
        win = 100
        length = 434
        overlap = -1
        freq = 2000.0
        arr = self.random_array(size = length, seed = 17)
        self.assertRaises(ValueError, cpp.welch, arr, freq, win, overlap)

    # ======= Energy Tests ============================================== #
    def test_energy_no_operations (self):
        self.assertRaises(ValueError, cpp.call_energy, [1, 2], 1, 1, 0)
    def test_energy_too_many_operations (self):
        self.assertRaises(ValueError, cpp.call_energy, [], 1, 1, 1)
    def test_energy (self):
        length  = 1000
        signal  = self.random_array(size = length, seed = 18)
        stride  = 20
        mw      = 20
        ops     = 50
        cpp_res = cpp.call_energy(signal, mw, stride, ops)
        py_res  = py.call_energy(signal, mw, stride, ops)
        self.assertNear("energy", cpp_res, py_res)
    def test_energy_one_too_many_mw (self):
        length  = 1000
        signal  = self.random_array(size = length, seed = 19)
        stride  = 20
        mw      = 20 + 1
        ops     = 50
        self.assertRaises(ValueError, cpp.call_energy, signal, mw, stride, ops)
    def test_energy_one_too_many_ops (self):
        length  = 1000
        signal  = self.random_array(size = length, seed = 20)
        stride  = 20
        mw      = 20
        ops     = 51
        self.assertRaises(ValueError, cpp.call_energy, signal, mw, stride, ops)

    # ======= Max Dist Tests ============================================ #
    def test_max_dist_no_operations (self):
        self.assertRaises(ValueError, cpp.call_max_dist, [1, 2], 1, 1, 0)
    def test_max_dist_too_many_operations (self):
        self.assertRaises(ValueError, cpp.call_max_dist, [], 1, 1, 1)
    def test_max_dist (self):
        length  = 1000
        signal  = self.random_array(size = length, seed = 21)
        stride  = 20
        mw      = 20
        ops     = 50
        cpp_res = cpp.call_max_dist(signal, mw, stride, ops)
        py_res  = py.call_max_dist(signal, mw, stride, ops)
        self.assertNear("max_dist", cpp_res, py_res)
    def test_max_dist_one_too_many_mw (self):
        length  = 1000
        signal  = self.random_array(size = length, seed = 22)
        stride  = 20
        mw      = 20 + 1
        ops     = 50
        self.assertRaises(ValueError, cpp.call_max_dist, signal, mw, stride, ops)
    def test_max_dist_one_too_many_ops (self):
        length  = 1000
        signal  = self.random_array(size = length, seed = 23)
        stride  = 20
        mw      = 20
        ops     = 51
        self.assertRaises(ValueError, cpp.call_max_dist, signal, mw, stride, ops)

    # ======= PSD Tests ================================================= #
    def test_psd_no_operations (self):
        self.assertRaises(ValueError, cpp.call_psd, [1, 2], 1, 1, 0, self.rngs)
    def test_psd_too_many_operations (self):
        self.assertRaises(ValueError, cpp.call_psd, [], 1, 1, 1, self.rngs)
    def test_psd(self):
        length  = 1_000_000
        signal  = self.random_array(size = length, seed = 24)
        stride  = 1_000
        mw      = 2_000
        ops     = 999
        cpp_res = cpp.call_psd(signal, mw, stride, ops, self.rngs)
        py_res  = np.asarray(py.call_psd_tri(signal, mw, stride, ops))
        py_res  = np.asarray([py_res[:,0], py_res[:,1], py_res[:,2]])
        self.assertNear("psd", cpp_res, py_res)
    def test_psd_window_too_small(self):
        length  = 1_000_000
        signal  = self.random_array(size = length, seed = 24)
        stride  = 1_000
        mw      = 20
        ops     = 999
        self.assertRaises(ValueError, cpp.call_psd, signal, mw, stride, ops, self.rngs)
    def test_psd_one_too_many_mw (self):
        length  = 1_000_000
        signal  = self.random_array(size = length, seed = 25)
        stride  = 1_000
        mw      = 2_001
        ops     = 999
        self.assertRaises(ValueError, cpp.call_psd, signal, mw, stride, ops, self.rngs)
    def test_psd_one_too_many_ops (self):
        length  = 1_000_000
        signal  = self.random_array(size = length, seed = 26)
        stride  = 20
        mw      = 20
        ops     = 1000
        self.assertRaises(ValueError, cpp.call_psd, signal, mw, stride, ops, self.rngs)

    # ======= Global Tests ============================================== #
    def test_global (self):
        py_res  = toolbox.py_bottleneck(toolbox.data_all, toolbox.mw,
                                        toolbox.stride, toolbox.dlist,
                                        toolbox.qstride, toolbox.query_all,
                                        toolbox.qlist)
        cpp_res = cpp.begin(toolbox.data_all, toolbox.stride,
                            toolbox.dlist[-1][1] - toolbox.dlist[0][0],
                            toolbox.query_all, toolbox.qstride,
                            toolbox.qlist[-1][1] - toolbox.qlist[0][0],
                            toolbox.mw)
        self.assertNear("psd-final", cpp_res[0][0], py_res[0][0], "Data PSD(1)")
        self.assertNear("psd-final", cpp_res[0][1], py_res[0][1], "Data PSD(2)")
        self.assertNear("psd-final", cpp_res[0][2], py_res[0][2], "Data PSD(3)")
        self.assertNear("energy", cpp_res[0][3], py_res[0][3], "Data Energy")
        self.assertNear("max_dist", cpp_res[0][4], py_res[0][4], "Data Max Dist")

        self.assertNear("psd-final", cpp_res[1][0], py_res[1][0], "Query PSD(1)")
        self.assertNear("psd-final", cpp_res[1][1], py_res[1][1], "Query PSD(2)")
        self.assertNear("psd-final", cpp_res[1][2], py_res[1][2], "Query PSD(3)")
        self.assertNear("energy", cpp_res[1][3], py_res[1][3], "Query Energy")
        self.assertNear("max_dist", cpp_res[1][4], py_res[1][4], "Query Max Dist")

# ======= Main ============================================================== #

if __name__ == "__main__":
    unittest.main(verbosity=3)
