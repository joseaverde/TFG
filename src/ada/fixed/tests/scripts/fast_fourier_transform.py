import numpy.fft
import ada_io as io

io.write_complex_signal(numpy.fft.fft(io.read_signal()))
