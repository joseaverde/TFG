from typing import Final
import sys
import os

root : Final[str] = os.path.dirname(os.path.dirname(os.path.join(os.getcwd(),
                                                                 sys.argv[0])))
sys.path.append(os.path.join(root, "reference"))
sys.path.append(os.path.join(root, "reference/mylibrary/computeDTW/EEGLib/"
                                 + "build/lib.linux-x86_64-cpython-310"))

import subprocess
import unittest
import scipy.integrate
import random
import numpy as np
import mylibrary.common.utils

def make_unit_tests (executable : str):

    PIPE = subprocess.PIPE
    cmd = subprocess.Popen(executable, stdin=PIPE, stdout=PIPE)

    def read () -> str:
        return cmd.stdout.readline().decode("utf-8").strip()

    def write (msg : str):
        cmd.stdin.write(msg.encode("utf-8"))
        cmd.stdin.flush()

    def stop ():
        write("Stop\n");
        cmd.stdin.close()
        cmd.terminate()
        cmd.wait(timeout=0.1)

    caller = read()
    params = {}
    while True:
        x = read().strip()
        if x == "":
            break
        name, value = map(lambda s: s.strip(), x.split("="))
        value = eval(value)
        params.update({name : value})
    globals().update(params)

    # ==== Simpson ========================================================== #

    def cmd_simpson (y : list[float], dx : float):
        write("Simpson\n")
        write(f"{len(y)}\n")
        write(" ".join(list(map(str, y))))
        write(f"\n{dx}\n")
        return eval(read())

    def py_simpson (y : list[float], dx : float):
        return scipy.integrate.simps(y, dx=dx)

    class TestSimpson (unittest.TestCase):
        epsilon = (1e-6, 1e-9)

        def random_array (self, size : int, seed : int) -> list[float]:
            random.seed(seed)
            return [random.random() for i in range(size)]

        def assertNear(self, left, right, msg=""):
            msg = f"({name}) not near enough!: " + msg
            self.assertTrue(np.allclose(left, right, *self.epsilon), msg)

        def test_simpson_three_items (self):
            arr = [1, 2, 100]
            dx = 1.5
            cmd_val = cmd_simpson(arr, dx)
            py_val = py_simpson(arr, dx=dx)
            self.assertNear(cmd_val, py_val, f"{cmd_val} != {py_val}")

        def test_simpson_mod_3_is_0 (self):
            arr = self.random_array(size = 300, seed = 4)
            dx = 0.5
            cmd_val = cmd_simpson(arr, dx)
            py_val = py_simpson(arr, dx=dx)
            self.assertNear(cmd_val, py_val, f"{cmd_val} != {py_val}")

        def test_simpson_mod_3_is_1 (self):
            arr = self.random_array(size = 301, seed = 5)
            dx = 0.5
            cmd_val = cmd_simpson(arr, dx)
            py_val = py_simpson(arr, dx=dx)
            self.assertNear(cmd_val, py_val, f"{cmd_val} != {py_val}")

        def test_simpson_mod_3_is_2 (self):
            arr = self.random_array(size = 302, seed = 6)
            dx = 0.5
            cmd_val = cmd_simpson(arr, dx)
            py_val = py_simpson(arr, dx=dx)
            self.assertNear(cmd_val, py_val, f"{cmd_val} != {py_val}")

    # ======= Energy Tests ============================================== #

    def cmd_simpson (y : list[float], dx : float):
        write("Simpson\n")
        write(f"{len(y)}\n")
        write(" ".join(list(map(str, y))))
        write(f"\n{dx}\n")
        return eval(read())

    """
    def py_simpson (y : list[float], dx : float):
        return scipy.integrate.simps(y, dx=dx)

    class TestEnergy (unittest.TestCase):
        epsilon = (1e-6, 1e-9)

        def random_array (self, size : int, seed : int) -> list[float]:
            random.seed(seed)
            return [random.random() for i in range(size)]

        def assertNear(self, left, right, msg=""):
            msg = f"({name}) not near enough!: " + msg
            self.assertTrue(np.allclose(left, right, *self.epsilon), msg)

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
    """

    # ======= Welch Tests =============================================== #

    def py_welch (arr : list[float], freq : float, win : int):
        return mylibrary.common.utils.welch(arr, freq, nperseg=win,
                                            noverlap=win // 2)[1]

    def cmd_welch (arr : list[float], freq : float, win : int):
        write("Welch\n")
        write(f"{len(arr)}\n")
        write(" ".join(list(map(str, arr))))
        write(f"\n{freq}\n")
        result = np.array(list(map(float, read().split())))
        return result

    class TestWelch (unittest.TestCase):
        epsilon = (1e-2, 1e-2)
        welch_borders : Final[int] = 2

        def random_array (self, size : int, seed : int) -> list[float]:
            random.seed(seed)
            return [random.random() for i in range(size)]

        def assertNear(self, left, right):
            msg = f"({name}) not near enough! ({len(left)}, {len(right)}):\n"
            if not np.allclose(left, right, *self.epsilon):
                for i in range(len(left)):
                    if not np.isclose(left[i], right[i], *self.epsilon):
                        msg += f"[{i}] {left[i]} != {right[i]}\n"
                self.assertTrue(np.allclose(left, right, *self.epsilon), msg)

        def test_welch_one_even_window (self):
            win = welch_window_size
            freq = 123.0
            arr = self.random_array(size = win + 20, seed = 7)
            cmd_Pxx = cmd_welch(arr, freq, win)
            py_Pxx = py_welch(arr, freq, win)
            w = self.welch_borders
            self.assertNear(cmd_Pxx[w:-w], py_Pxx[w:-w])

        def test_welch_length_multiple_of_window (self):
            win = welch_window_size
            length = win * 4
            freq = 17.0
            arr = self.random_array(size = length, seed = 8)
            cmd_Pxx = cmd_welch(arr, freq, win)
            py_Pxx = py_welch(arr, freq, win)
            w = self.welch_borders
            self.assertNear(cmd_Pxx[w:-w], py_Pxx[w:-w])
        def test_welch_length_multiple_of_window_plus_overlap(self):
            win = welch_window_size
            overlap = win // 2 - 20
            length = win * 7 + overlap
            freq = 170.0
            arr = self.random_array(size = length, seed = 9)
            cmd_Pxx = cmd_welch(arr, freq, win)
            py_Pxx = py_welch(arr, freq, win)
            w = self.welch_borders
            self.assertNear(cmd_Pxx[w:-w], py_Pxx[w:-w])
        def test_welch_length_multiple_of_window_plus_1(self):
            win = welch_window_size
            length = win * 4 + 1
            freq = 2003.0
            arr = self.random_array(size = length, seed = 10)
            cmd_Pxx = cmd_welch(arr, freq, win)
            py_Pxx = py_welch(arr, freq, win)
            w = self.welch_borders
            self.assertNear(cmd_Pxx[w:-w], py_Pxx[w:-w])
        def test_welch_length_multiple_of_window_minus_1(self):
            win = welch_window_size
            length = win * 4 - 1
            freq = 3.14159265358
            arr = self.random_array(size = length, seed = 11)
            cmd_Pxx = cmd_welch(arr, freq, win)
            py_Pxx = py_welch(arr, freq, win)
            w = self.welch_borders
            self.assertNear(cmd_Pxx[w:-w], py_Pxx[w:-w])

    tests = {}
    for (key, name) in locals().items():
        if not key.startswith("Test"):
            continue
        tests.update({key : name})
    return tests, stop

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"USAGE `{sys.argv[0]} seizure_detector_utests'")
        exit(1)
    else:
        tests, stop = make_unit_tests(sys.argv[1])
        globals().update(tests)
        sys.argv = [sys.argv[0]]
        unittest.main()
