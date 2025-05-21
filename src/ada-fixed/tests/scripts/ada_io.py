import sys

def perror(*args, **kwargs):
    print(*args, **kwargs, file=sys.stderr, flush=True)

def read_raw ():
    data = ""
    while True:
        try:
            x = input()
        except EOFError:
            break
        else:
            data += x
    return data

def read_signal ():
    return eval(read_raw().replace("\n", " "))

def complex_img(c):
    return f"{c.real}, {c.imag}"

def write_signal (signal):
    print("[", end="", flush=True)
    if len(signal) > 0:
        print(f"{signal[0]}", flush=True, end="")
        for i in range(1, len(signal)):
            print(f", {signal[i]}", flush=True, end="")
    print("]", flush=True)

def write_complex_signal (signal):
    print("[", end="", flush=True)
    if len(signal) > 0:
        print(f"{complex_img(signal[0])}", flush=True, end="")
        for i in range(1, len(signal)):
            print(f", {complex_img(signal[i])}", flush=True, end="")
    print("]", flush=True)
