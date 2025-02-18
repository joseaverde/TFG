
Y = (lambda f : (lambda x : x (x))
                (lambda x : f(lambda v : x(x)(v))))

fact = (Y (lambda f : (lambda x : (1 if x == 0 else x * f(x - 1)))))

# No funciona lo de la reducción :(, con listas de compresión voy a probar otra
# cosa

signal = [5, 10, 2, 3, 4, 5, 6, 10]

"""
w = []
def echo(*args):
    for i in range(len(args)):
        print(f" Arg {i} = {args[i]}")
    return args[-1]
def sum(a, b):
    print(f"{a} + {b}")
    return a + b
print(
    [stack.append(sum(stack.pop(), sample)) or echo(stack, stack[0])
        for sample in signal
        for stack in ([0],)])
"""

def echo(x):
    print(x)
    return x

# Mean
print([stack.append(stack.pop() + sample) or stack[0]
        for stack in [[0]] for sample in signal][-1])

# Energy
print([stack.append(stack.pop() + (sample - mean) ** 2) or stack[0]
    for stack, mean in [(
        [0],                                                # stack
        [stack.append(stack.pop() + sample) or stack[0]     # mean
            for stack in [[0]] for sample in signal][-1] / len(signal)
        )]
    for sample in signal][-1] / len(signal))

# Max distance
print([result[1] - result[0]
        for result in [
            [stack.append([(max(acc[0], sample), min(acc[1], sample))
                        for acc in [stack.pop()]][0]) or stack[0]
                for stack in [[(signal[0], signal[0])]]
                for sample in signal[1:]][-1]]][0])


print(
[(energy, max_dist)

 for energy in [
     [stack.append(stack.pop() + (sample - mean) ** 2) or stack[0]
      for stack, mean in [(
          [0],                                                # stack
          [stack.append(stack.pop() + sample) or stack[0]     # mean
           for stack in [[0]] for sample in signal][-1] / len(signal)
          )]
      for sample in signal][-1] / len(signal)]

 for max_dist in [
     [result[1] - result[0]
      for result in [
          [stack.append([(max(acc[0], sample), min(acc[1], sample))
                         for acc in [stack.pop()]][0]) or stack[0]
           for stack in [[(signal[0], signal[0])]]
           for sample in signal[1:]][-1]]][0]]]
          )
