Trabajo Fin de Grado
====================
**Procesamiento paralelo de señales de encefalograma para la detección de
ataques epilépticos**: _El objetivo de este trabajo es implementar en C++ un
algoritmo para la detección de ataques epilépticos utilizando señales de
encefalograma; hacer un análisis comparativo del efecto que tiene
paralelización y el uso C++ frente a otros lenguajes sobre el consumo
energético y el tiempo de ejecución; y ejecutar el modelo en dispositivos
empotrados de bajo consumo como el ESP32C3 o el ESP32S3._



Generate the files

> git submodule update

> cp tests/requirements.txt reference

> cd reference

> uv init --bare --python "~=3.10.0"

> uv sync

> uv python install 3.10

> uv add -r requirements.txt

<!-- TODO: Change dlib to version 20.0.0 the other one doesn't compile -->


Load the files

> uv run python3 scripts/aa_import_patients.py

There is no need to download everything, Ctrl-C once the first one has
finished. And remove the rest of the edf files:

> rm *.edf

Then compile PaFESD

> cd mylibrary/computeDTW/EEGLib

> sed -i 's/gcc-12/gcc/g' setup_cython.py

> sed -i 's/g++-12/g++/g' setup_cython.py

> uv run setup_cython.py build_ext --inplace

Generate the file

> cd ../../../tests

> uv sync

> uv run prepare.py


# To run
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
