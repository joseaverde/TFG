# *-* encoding=utf8 *-*

from typing import Final
from functools import reduce
from math import sqrt

class StatisticsError(Exception):
    pass

class Statistics:
    def __init__ (self, name : str, fields : tuple[str, ...]):
        self.__name    : Final[str]              = name
        self.__fields  : Final[tuple[str, ...]]  = fields
        self.__count   : Final[int]              = len(fields)
        self.__times   : list[tuple[float, ...]] = []

    @property
    def name (self) -> str:
        return self.__name

    @property
    def fields (self) -> tuple[str, ...]:
        return self.__fields

    def add (self, times : tuple[float]):
        if len(times) != self.__count:
            raise StatisticsError("Invalid number of elements")
        self.__times.append(times)

    def __mean (self, index : int) -> float:
        return (reduce(lambda a, b : a + b[index], self.__times, 0)
               / len(self.__times))

    def __sv (self, mean : float, index : int) -> float:
        return sqrt(reduce(lambda a, b : a + (b[index] - mean)**2, self.__times, 0)
               / len(self.__times))

    def calculate(self) -> tuple[tuple[float, float], ...]:
        if len(self.__times) == 0:
            raise StatisticsError("No elements")
        means = tuple(self.__mean(i) for i in range(self.__count))
        return tuple((means[i], self.__sv(means[i], i))
                        for i in range(self.__count))

    def show (self, base=-1) -> str:
        results = self.calculate()
        mx = max(map(len, self.fields)) + 1
        places  = 4
        fmt     = f"%.{places}f±%.{places}f"
        result  = f"\033[33;1mBenchmarking {self.name}\033[0m "
        result += f"(n={len(self.__times)})\033[1m\n"
        result += f"{' ' * (mx + 3 + (places+2)//2)}μ{' '*(places+1)}σ\033[0m"
        if 0 <= base < self.__count:
            result += f"{' '*(places+2)}\033[1mSpeed Up\033[0m"
        result += "\n"
        for i in range(self.__count):
            result += (" - "
                + f"\033[1m{self.fields[i] + ' ' * (mx - len(self.fields[i]))}"
                + f"\033[0m{fmt % results[i]}\033[1m")
            if base != i and 0 <= base < self.__count:
                result += f"   x{'%.2f'%(results[base][0]/results[i][0])}"
            elif base == i and 0 <= base < self.__count:
                result += f"\033[0m   -------"
            result += "\033[0m\n"
        return result
