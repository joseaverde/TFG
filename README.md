Trabajo Fin de Grado
====================
**Procesamiento paralelo de señales de encefalograma para la detección de
ataques epilépticos**: _El objetivo de este trabajo es implementar en C++ un
algoritmo para la detección de ataques epilépticos utilizando señales de
encefalograma; hacer un análisis comparativo del efecto que tiene
paralelización y el uso C++ frente a otros lenguajes sobre el consumo
energético y el tiempo de ejecución; y ejecutar el modelo en dispositivos
empotrados de bajo consumo como el ESP32C3 o el ESP32S3._

## Implementaciones
Uno de los propósitos del TFG es hacer un análisis comparativo de distintas
implementaciones: su rendimiento, uso de energía, etcétera. En principio vamos
a tener las siguientes implementaciones:

* Ada
  - fixed-2 (punto fijo binario)
  - fixed-10 (punto fijo decimal)
  - float (IEEE754, punto flotante 32 bits)
  - long float (IEEE754, punto flotante 64 bits)
* C++
  - float (IEEE754, punto flotante 32 bits)
  - double (IEEE754, punto flotante 64 bits)
  - long double (IEEE754, punto flotante 80 bits)
  - fixed
* Scheme
  - exact (fracciones y enteros)
  - inexact (punto flotante)
* Python
  - original
  - propio
  - con C++
  - con Ada

## Pruebas
Todas las pruebas (unitarias, funcionales y de rendimiento) se realizan en
comparación con la implementación de referencia, que está como submódulo.

### Pruebas unitarias
#### Simpson
#### Fast Fourier Transform
#### Welch
#### Power Spectral Density (PSD)
#### Dynamic Time Warping (DTW)
#### Max Distance
#### Energy

### Pruebas funcionales
Para las pruebas funcionales vamos a contrastar los resultados del algoritmo
con pacientes reales. Tenemos que ver cómo difieren (cuál es la distancia)
entre la implementación original, y cada una de las implementaciones y
estrategias de resolución. Y (si es posible), si se obtienen mejores
resultados.

Para ello todos los ejecutables deben tener el mismo formato de salida:

```plain
<lenguaje> ftests <muestras_por_segundo> <segundos_por_epoch> <estrategia> ...
<predicción> <energía> <distancia> <psd_1> <psd_2> <psd_3> <dtw>
<predicción> <energía> <distancia> <psd_1> <psd_2> <psd_3> <dtw>
...
<predicción> <energía> <distancia> <psd_1> <psd_2> <psd_3> <dtw>
```

La entrada sigue siendo la misma: el _batch_, seguido por la señal. El
`chbXX.batch` y `chbXX.in` que genera [./tests/prepare.py](prepare.py).

Previamente el programa [./tests/prepare.py](prepare.py) ha generado un archivo
llamado `chbXX.out` con la salida que hubiera generado el programa original
utilizando un formato parecido:

```plain
<precisión> <sensibilidad> <f1 score>
<esperado> <predicción> <energía> <distancia> <psd_1> <psd_2> <psd_3> <dtw>
<esperado> <predicción> <energía> <distancia> <psd_1> <psd_2> <psd_3> <dtw>
...
<esperado> <predicción> <energía> <distancia> <psd_1> <psd_2> <psd_3> <dtw>
```

El programa [./tests/ftests.py](ftests.py) debe llamarse de la siguiente
manera:

> cat chbXX.batch chbXX.in | seizure_detector_ftests | python3 ftests.py chbXX

Este programa genera por salida estándar un JSON con las diferencias entre la
salida esperada y la salida obtenida. Y cuánto difiere de la solución correcta.
Esto nos permite comparar cómo se acercan las soluciones de punto fijo, punto
flotante y exactas (scheme). Y cómo se acumulan los errores de precisión.

Estas pruebas se deben comprobar a mano.


### Pruebas de rendimiento
#### Empotrado
#### PC

## Executables
### detector
### ftest
### utest

### benchmark

 * No secondary stack
 * Sqrt doesn't work
 * Stack size
