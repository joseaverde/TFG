import scipy
import scipy.integrate
import ada_io as io

if scipy.__version__ != "1.10.1":
    raise Exception ("Invalid scipy version, expected 1.10.1!")

data = io.read_signal()
dx = data["dx"]
signal = data["signal"]
print("%.10f" % scipy.integrate.simps(signal, dx=dx), flush=True)
