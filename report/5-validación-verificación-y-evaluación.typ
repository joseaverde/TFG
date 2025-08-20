#import "@preview/lilaq:0.4.0" as lq

= Validación, verificación y evaluación
*TODO*

== Validación
*TODO*
== Verificación
*TODO*
== Evaluación
En esta sección se hace una evaluación del rendimiento del programa y un
análisis del mismo. Se hace un análisis en orden cronológico y se indica las
conclusiones a las que se llegó en cada paso.

=== Comparación en la validación
En un primer lugar como se ve en la @sec:8-planificación en la *tarea 3.3* se
hicieron pruebas de rendimiento con el algoritmo después de optimizarlo y
paralelizarlo. Las pruebas se realizan en dos dispositivos:

- _Slimbook_: Un portátil con un _12th Gen Intel i7-12700H (20) \@ 4.600GHz_
  con 20 hilos lógicos.
- Servidor: Un servidor con un _Intel Xeon Gold 6326 (64) \@ 3.500GHz_ con 64
  hilos lógicos.

Las comparación se realiza entre la de referencia, que está escrita en _Python
3_ con alguna parte en #box([C++]) (la función `dtw`, deformación dinámica del
tiempo); y con la que de este proyecto que está escrita en #box([C++]). Cabe
destacar que ambas implementaciones están paralelizadas, la de Python 3 usa
`multiprocessing` y la de C++ usa `OpenMP`.

Para la prueba se toma la señal de un paciente en bruto, en este caso el
paciente CHB-MIT 6 @CHBMIT en el canal `C3-P3` con un _stride_ de 256 muestras
por segundo y épocas de 5 o _strides_ o 1280 muestras por segundo. Que tiene un
total de 61502976 muestras (66 horas, 44 minutos y 6 segundos de señal).

El campo *muestras* indica cuántas veces se ha medido cada una de las
funciones, en este caso se le ha dado un tiempo máximo de ejecución a cada una
de ellas de 180 segundos. Se da la media ($mu$) y la desviación típica
($sigma$) de los tiempos de ejecución. La función llamada *subrutina* es la
función combinada que computa _Simpson_, _PSD_ y _max distance_ a toda la
señal. Véase la @tab:pre-slimbook y la @tab:pre-moa:

#let pre-results-slimbook = (
  //                     Python3          C++
  //        Muestras    μ       σ       μ       σ
  simpson  : (1159,  0.1335, 0.0109, 0.0219, 0.0018),
  welch    : ( 109,  1.3980, 0.0323, 0.2598, 0.0049),
  psd      : (  18,  9.9002, 0.1139, 0.1596, 0.0049),
  energy   : ( 172,  1.0355, 0.0298, 0.0152, 0.0024),
  max_dist : (  41,  4.4274, 0.0728, 0.0304, 0.0044),
  all      : (  12, 15.1082, 0.1109, 0.2664, 0.0398))
  
#let pre-results-moa = (
  //                     Python3          C++
  //        Muestras    μ       σ       μ       σ
  simpson  : (831, 0.1845, 0.0024, 0.0321, 0.0008),
  welch    : ( 75, 2.0053, 0.0048, 0.4095, 0.0022),
  psd      : ( 53, 3.3504, 0.2755, 0.0704, 0.0069),
  energy   : ( 56, 3.2378, 0.6921, 0.0129, 0.0021),
  max_dist : ( 60, 3.0195, 0.7865, 0.0148, 0.0053),
  all      : ( 21, 8.5647, 1.4619, 0.0997, 0.0192))

#let pre-names = (
  simpson  : [Simpson],
  welch    : [Welch],
  psd      : [PSD],
  energy   : [Energy],
  max_dist : [Max Distance],
  all      : [*Subrutina*])

#let pre-table(items, id, caption) = {

  let contents = ()
  for (key, name) in pre-names {
    if key == "all" {
      contents.push(table.hline(stroke: 0.25pt))
    }
    contents.push(name)
    for i in range(5) {
      contents.push([$#items.at(key).at(i)$ #if i == 0 { [] } else { [s] } ])
    }
    let incr = calc.round(items.at(key).at(1) / items.at(key).at(3), digits: 1)
    contents.push([*$times #incr$*])
  }

  [
    #figure(
      caption: caption,
      table(
        columns: (auto, auto, auto, auto, auto, auto, auto),
        align: (horizon, horizon, center, center, center, center, horizon),
        table.header(
          table.cell(rowspan: 2, [*Función*]),
          table.cell(rowspan: 2, [*Muestras*]),
          table.cell(colspan: 2, [*Python 3*]),
          table.cell(colspan: 2, [*C++*]),
          table.cell(rowspan: 2, [*_Speed-up_*]),
          table.hline(start:2,end:6, stroke: 0.25pt, position: bottom),
          [*$mu$*], [*$sigma$*], [*$mu$*], [*$sigma$*]),
        ..contents
        ))
    #label(id)
  ]
}

#let pre-diagram(id, caption, field) = {

  let xs = range(2)
  let ys1 = (pre-results-slimbook.at(field).at(3), pre-results-moa.at(field).at(3))
  let yerr1 = (pre-results-slimbook.at(field).at(4), pre-results-moa.at(field).at(4)) 
  let ys2 = (pre-results-slimbook.at(field).at(1), pre-results-moa.at(field).at(1))
  let yerr2 = (pre-results-slimbook.at(field).at(2), pre-results-moa.at(field).at(2)) 
  [
    #figure(
      caption: caption,
      lq.diagram(
        width: 5cm,
        legend: (position: left + top),

        xaxis: (
          ticks: ([_Slimbook_], [Servidor])
            .map(rotate.with(-45deg, reflow: true))
            .map(align.with(right))
            .enumerate()),
        lq.bar(xs, ys1, offset: -0.2, width: 0.4, label: [C++]),
        lq.bar(xs, ys2, offset: 0.2, width: 0.4, label: [Python 3]),
        
        lq.plot(
          xs.map(x => x - 0.2), ys1, 
          yerr: yerr1,
          color: black,
          stroke: none),
        lq.plot(
          xs.map(x => x + 0.2), ys2, 
          yerr: yerr2,
          color: black,
          stroke: none)
      )
    )
    #label(id)
  ]
}

#pre-table(pre-results-slimbook, "tab:pre-slimbook", [_Slimbook_: 20 hilos])
#pre-table(pre-results-moa, "tab:pre-moa", [Servidor: 64 hilos])

De los resultados se ve que la versión de C++ es mucho más rápida que la
versión de Python bajo las mismas condiciones (paralelismo con el mismo número
de hilos). El incremento se ve menor en las funciones de _Simpson_ y _Welch_,
posiblemente porque ambas pertenecen a `SciPy` que utiliza `numpy` por debajo,
que está escrito en C. La función _max distance_ que está escrita en Python 3
nativo es la que mejor mejora ve.

#[
  #set page(columns: 2)

  #pre-diagram("fig:pre-simpson", [Función _Simpson_], "simpson")
  #pre-diagram("fig:pre-welch", [Función _Welch_], "welch")
  #pre-diagram("fig:pre-psd", [Función _PSD_], "psd")
  #pre-diagram("fig:pre-energy", [Función _energy_], "energy")
  #pre-diagram("fig:pre-max-distance", [Función _max distance_], "max_dist")
  #pre-diagram("fig:pre-subrutina", [*Subrutina*], "all")
]

=== Punto flotante
=== Punto fijo

// - Alcance: Validación y ejecución. Validación se refiere a la parte
//   paralelizable de la fase de entrenamiento. Y ejecución se refiere a la
//   detección de ataques epilépticos
// - Computador: Portátil, ESP32C3, ESP32C6, ESP32S3, Raspberry Pi 3 y Raspberry
//   Pi 4.
// - Lenguaje de programación: Python3, C++, Ada/SPARK y Python3 + C++.
// - Compilador: GCC/GNAT y Clang
// - Modo de compilación (en Ada): con y sin pruebas en tiempo de ejecución
//   activadas.
// - Estrategia: Punto fijo y Punto flotante

// Consumo de energía

#let make-result(language, compiler, checks, real, target, performance) = (
  language    : language,
  compiler    : compiler,
  checks      : checks,
  real        : real,
  target      : target,
  performance : performance,
)

#let results = (
  // Float 32 : ESP32C3
  make-result("C++", "GCC 14",  false, "float32", "ESP32C3", 1.64),
  make-result("Ada", "GNAT 14", false, "float32", "ESP32C3", 0.96),
  make-result("Ada", "GNAT 14", true,  "float32", "ESP32C3", 0.96),
  // Float 32 : Raspberry Pi 4
  make-result("C++", "Clang 18", false, "float32", "RPi 4 (32 bits)", 470),
  make-result("Ada", "GNAT 10",  false, "float32", "RPi 4 (32 bits)", 286),
  make-result("Ada", "GNAT 10",  true,  "float32", "RPi 4 (32 bits)", 280),
  // Float 32 : Slimbook
  make-result("C++",     "Clang 18",   false, "float32", "Slimbook", 3036),
  make-result("Ada",     "GNAT 10",    false, "float32", "Slimbook", 1725),
  make-result("Ada",     "GNAT 10",    true,  "float32", "Slimbook", 1433),
  make-result("Python3", "python3.10", false, "float32", "Slimbook", 1264),
)

*TODO*
