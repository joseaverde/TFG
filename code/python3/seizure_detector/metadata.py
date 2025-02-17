import csv
import config

class Metadata:
    def __init__ (self, name : str):
        with open(name, "r", newline="") as csvfile:
            reader = csv.reader(csvfile,delimiter=",")
            self.__bounds = [(int(row[4]) * config.stride,
                              int(row[5]) * config.stride,)
                                for row in list(reader)[1:-1]]
    def is_seizure (self, seizure : int) -> bool:
        index : Final[int] = seizure * config.stride
        for (f, t) in self.__bounds:
            if f <= index <= t:
                return True
        return False

    def get_bounds (self) -> list[tuple[int, int]]:
        return list(self.__bounds)
