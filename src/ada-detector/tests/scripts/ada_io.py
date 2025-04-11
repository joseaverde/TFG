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

def write_complex_signal (signal):
    result = "[" + ", ".join((f"{c.real}, {c.imag}" for c in signal)) + "]"
    print(result)
