Electroencephalogram signal processing for seizure detection
============================================================
This repository contains my barchelor thesis, where I implement the validation
phase described by the paper «PaFESD: Patterns Augmented by Features Epileptic
Seizure Detection» (the repository can be found here
<https://github.com/PPMC-DAC/PaFESD-Epileptic-Seizure-Detection>). This project
was done in collaboration with the researchers of the original paper in the
department of computer architecture (ARCOS) of Universidad Carlos III de
Madrid.

The report can be found in [report/](report/). Note, however, that it is
written in Spanish. This thesis's original description in Spanish was:

> **Procesamiento paralelo de señales de encefalograma para la detección de
> ataques epilépticos**: _El objetivo de este trabajo es implementar en C++ un
> algoritmo para la detección de ataques epilépticos utilizando señales de
> encefalograma; hacer un análisis comparativo del efecto que tiene
> paralelización y el uso C++ frente a otros lenguajes sobre el consumo
> energético y el tiempo de ejecución; y ejecutar el modelo en dispositivos
> empotrados de bajo consumo como el ESP32C3 o el ESP32S3._

Which translates to:

> **Parallel electroencephalogram signal processing for seizure detection**:
> The purpose of this project is to implement in C++ an algorithm for the
> detection of seizures by processing electroencephalogram signals. And to do a
> comparative analysis on the effect of parallelisation and the use of C++ in
> comparison to other languages on the energy consumption and execution time.
> Finally run the model in low-consume embedded systems such as ESP32C3 or
> ESP32S3.

However, as the development of the project went on, the description also
changed. The software was implemented in C++ and parallelised to improve
training time. Nevertheless, it barely gave real-time in the ESP32C3. That is
why I did a quick experiment with Ada and binary fixed point types to check the
performance. It ended being a success, so what remained was to prove the
absence of errors of the code by using SPARK. Because fixed point types are
prone to overflow and underflow (in contrast floating point types scale, while
fixed point types have the exponent encoded in the type, not the value).

## Getting Started
The data to be processed is based on an open dataset called
[CHBMIT](https://physionet.org/content/chbmit/1.0.0/). The files are
preprocessed by the original project
[PPMC-DAC](https://github.com/PPMC-DAC/PaFESD-Epileptic-Seizure-Detection),
which is provided as a submodule for convenience. Please note that this program
was only tested on GNU/Linux. It should work on Windows, but I haven't tested
it.

### Generate the patient files
First load the submodule

```bash
git submodule update
```

Instead of pip I use [`uv`](https://docs.astral.sh/uv/), because it allows me
to choose a specifi Python version. In this case we need Python 3.10 because
certain dependency later changed the implementation of certain function. Also,
we need to patch because some dependencies broke things :).

```bash
cp tests/requirements.txt reference
cd reference
uv init --bare --python "~=3.10.0"
uv python install 3.10
uv sync
uv add -r requirements.txt
```

Once `uv` is set up, we can load the program to load the patients. Note that
this files are HUGE. Only patient `chb01` uses 5.3GiB of memory, and there are
24 patients. Also it takes a very long time to download each patient. That's
why I recommend to cancel the command once one patient has been downloaded
(you'll know when it starts to download the next patient). In any case run:

```bash
uv run python3 scripts/aa_import_patients.py
# Ctrl-C to stop it.
rm *.edf
```

Then we need to compile the original library to compute the dynamic time
warping function. This is so that it can be used as a baseline to check the new
implementation. (It also needs patching).

```bash
cd mylibrary/computeDTW/EEGLib
sed -i 's/gcc-12/gcc/g' setup_cython.py
sed -i 's/g++-12/g++/g' setup_cython.py
uv run setup_cython.py build_ext --inplace
```

Once that's done, go to the root directory of the project. And enter the
[tests/](tests/) directory. The script [prepare.py](tests/prepare.py) will
clean the signal and generate text files so that they can be used by all
implementations of the algorithm. To do so:

```bash
cd tests/
uv sync
uv run prepare.py
```

This will generate for patient `chbXY` three files:

 - `chbXY.in`
   - The first line contains a positive number with number of samples.
   - The next line contains as many real numbers as the first line indicated.
     These numbers are separated by spaces and may be positive or negative.
 - `chbXY.batch`
   - The first line contains a positive number with the number of pattern.
   - The second line contains a pair with the range of the `PSD 1` feature.
   - The third line contains a pair with the range of the `PSD 2` feature.
   - The fourth line contains a pair with the range of the `PSD 3` feature.
   - The fifth line contains a pair with the range of the `Energy` feature.
   - The sixth line contains a pair with the range of the `Max Dist` feature.
   - The seventh line contains the value `d_max_c`.
   - The following lines, as many as patterns. Contain in each line an epoch
     (in this case 1280 samples) of samples.
 - `chbXY.out`
   - The first line contains three values separated by spaces: precision,
     sensitivity and F1 score.
   - Then for each epoch in the original signal (given by `chbXY.in`) there is
     a line with **at least** 8 values separated by spaces:
     - First item: `True` or `False`. Contains wether that epoch belongs to a
       seizure or not.
     - Second item: `True` or `False`. The guess given by the original
       implementation on whether it thinks it is a seizure or not. **Note: that
       we don't filter artifacts**. In the original implementation a boolean
       vector called `DoNotCompute` is generated where each elements contains
       whether that epoch contains an artificat. If you want to replicate the
       results of the original paper you need to generate this vector.
     - Third item: floating point value with the `PSD 1` feature.
     - Fourth item: floating point value with the `PSD 2` feature.
     - Fifth item: floating point value with the `PSD 3` feature.
     - Sixth item: floating point value with the `Energy` feature.
     - Seventh item: floating point value with the `Max Dist` feature.
     - For each pattern there is an additional element that represents the
       dynamic time warping distance of that epoch to each pattern.

### Compile the software
For C++ you need a modern C++ compiler. One that supports C++20. It was tested
with GCC 14 and CLang 19. `conan` manages dependencies, so there is no need to
install anything else :).

For Ada you can download [Alire](https://alire.ada.dev/), which is Ada's new
community-driven package manager. It manages toolchains and crates. So there is
no need to install any compiler at also. Also, Alire is just an executable, so
there is no need to install it if you have it on path.

If you also want to cross compile, you need other tools. For ESP32C3, ESP32C6
and ESP32S3 you need to follow the
[Espressif guide](https://docs.espressif.com/projects/esp-idf/en/stable/esp32c3/get-started/index.html)
on how to install them.

For Raspberry Pi, in C++ it can be done with Docker, and is explained in that
section. In Ada for the time being I move the code to the Raspberry Pi and
compile it there with the `gnatmake` package. It may need some changes and to
generate some stubs for `SPARK` library. In the future I'll try to make it
easier to replicate for that platform.

#### Ada Fixed Point
The directory [src/ada/fixed/](src/ada/fixed) contains a library project. It is
also the one that is proved with SPARK. You can build it with:

```bash
alr build --release
```

You can also choose different flags insteand of `--release`:

 - `--release`: It compiles with aggressive optimisations and removes every
   runtime check. Because it has been proven correct by the theorem prover and
   SPARK. This is the one to go, when you want it to go fast.
 - `--development`: It is the default one. It compiles with assertions enabled
   and compiles faster. But it generates worse code. Only when developing. Note
   that GNAT is a very fast compiler and is almost instantaneous to compile the
   whole library.
 - `--validation`: It compiles with optimisations enabled but does NOT remove
   checks. It will check everything, every loop invariant, every assertion. And
   it will go pretty slow. It is only interesting to use while testing units
   that are yet to be proven correct.

You can also use this library in your project. For that refer to Alire
documentation.

##### Tests

The [src/ada/fixed/tests](src/ada/fixed/tests) contains some unit tests. You
can build them with:

```bash
alr build --development
```

However, to run them, as they require a Python interpreter you need to do:

```bash
uv run alr run
```

##### Main
Finally in the [src/ada/fixed/main](src/ada/fixed/main) directory contains
other executables for other types of tests. These ones are to be compiled with
release flags.

```bash
alr build --release
```

##### Cross Compilation ESP32C*
Go to [src/ada/fixed/esp32](src/ada/fixed/esp32), if you don't have IDF tools
in environment run:

```bash
get_idf
```

Then copy the `sdkconfig.<platform>` as `sdkconfig` to compile for the platform
you want and run:

```bash
cp sdkconfig.esp32c3 sdkconfg
alr build --release
idf.py build flash monitor
```

##### Cross Compilation ESP32S3
There is no current support for this platform and Ada.

##### Cross Compilation Raspberry Pi
As stated above, until I figure out how to do it with Docker. Copy the files to
the Raspberry and figure it out with `gprbuild` and moving files and
directories.


#### Ada Floating Point
Got to [src/ada/float/](src/ada/float). If you run:

```bash
bash config.sh
```

It will compile many different configurations for many profiles. In `bin/` you
have the different executables.

##### Cross Compilation for ESP32C*
Got to [src/ada/float/esp32](src/ada/float/esp32). And do:

```bash
get_idf     # If not imported
idf.py build flash monitor
```

#### C++
TODO


## Formal proof
TODO

## Cross compilation
TODO

## Run tests
TODO


## Embedded
### Ada Fixed
> cd src/ada/fixed/esp32

> alr build --release

> get_idf

> idf.py build flash monitor

## Validator
### Ada Fixed
> cd src/ada/fixed/main

> alr build --release

> ./bin/release-native/seizure_detector_validator < ../../../../tests/chb01.batch < ../../../../tests/chb01.in

## Functional tests
### Ada Fixed
> cd src/ada/fixed/main

> alr build --release

> ./bin/release-native/seizure_detector_ftest < ../../../../tests/chb01.batch < ../../../../tests/chb01.in > chb01.got

> awk -f ../../../compare.awk chb01.got ../../../../tests/chb01.out chb01.got

## Benchmarks
### Ada Fixed
> cd src/ada/fixed/main

> alr build --release

> ./bin/release-native/seizure_detector_fixed
