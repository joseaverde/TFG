#!/usr/bin/env  python3
# *-* encoding=utf8 *-*

import sys
import os
import numpy as np

def main(patient : str) -> int:
    # Read the solution
    def convert(x):
        x = x.split()
        b1 = (True if x[0] == "True" else "False")
        b2 = (True if x[1] == "True" else "False")
        xs = list(map(float, x[2:]))
        return [b1, b2] + xs
    here = os.path.dirname(sys.argv[0])
    with open(os.path.join(here, patient + ".out"), "r") as fp:
        pre, sen, f_1 = map(float, fp.readline()[:-1].split())
        solution = list(map(convert, filter(bool, fp.read().split("\n"))))
    # Read the result
    def convert(x):
        x = x.split()
        b = (True if x[0] == "True" else "False")
        xs = list(map(float, x[1:]))
        return [b] + xs
    reading = True
    result = []
    detector = input()
    while reading:
        try:
            line = input()
        except EOFError:
            reading = False
        else:
            if line:
                result.append(convert(line))
            else:
                break
    # Compare both of them
    if len(result) != len(solution):
        print(f"ERROR: The result has {len(result)} epochs,",
              f"{len(solution)} expected!")
        return 2
    for i in range(len(result)):
        dist = 0
        for j in range(1, len(result[i])):
            dist += (result[i][j] - solution[i][j + 1]) ** 2
        dist = dist ** 0.5
        print(dist)
    return 0

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"USAGE: `{sys.argv[0]} patient'")
        exit(1)
    else:
        exit(main(sys.argv[1]))
