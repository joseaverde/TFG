#!/usr/bin/env  python3
# *-* encoding=utf8 *-*

from statistics import Statistics
import time

class IgnoreBenchmark (Exception):
    pass

class Subbenchmark:
    def fields(self) -> tuple[str, ...]:
        pass

    def params(self) -> tuple[object, ...]:
        pass

    def funcs(self) -> tuple:
        pass

    def base (self) -> int:
        return -1

class benchmark:
    def __init__(self, max_time : float = 60.0):
        self.__max_time = max_time

    def __call__(self, unit):
        max_time = self.__max_time
        class Benchmark:
            def __init__(self, *args, **kwargs):
                self.__unit = unit(*args, **kwargs)
                self.__benchmarks = [(item[9:], self.__unit.__getattribute__(item))
                    for item in filter(lambda x : x.startswith("Benchmark"),
                                       self.__unit.__dir__())]

            def run (self):
                for (name, cls) in self.__benchmarks:
                    c = cls()
                    funcs = c.funcs()
                    params = c.params()
                    stats = Statistics(name, c.fields())
                    start_all = time.time()
                    valid = True
                    try:
                        while True:
                            times = []
                            for i in range(len(funcs)):
                                start = time.time()
                                funcs[i](*params)
                                stop = time.time()
                                times.append(stop - start)
                            stats.add(tuple(times))
                            if time.time() - start_all > max_time:
                                break
                    except IgnoreBenchmark as e:
                        print("\033[31;1m" + str(e) + "\033[0m")
                    else:
                        print(stats.show(c.base()))

        return Benchmark

def ignore(cls):
    class FuncTuple:
        def __init__ (self, name : str):
            self.__name : str = name
        def __getitem__ (self, n : int):
            def ignore_it(*args, **kwargs):
                raise IgnoreBenchmark("Ignored: " + self.__name[9:])
            return ignore_it
        def __len__ (self):
            return 2**32 - 1
    class Ignored(cls):
        def funcs(self) -> tuple:
            return FuncTuple(cls.__name__)
    return Ignored
