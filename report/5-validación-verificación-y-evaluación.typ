#import "@preview/lilaq:0.4.0" as lq
#import "./requisitos.typ" : config, srs, reqs, reference
#import "utility.typ": cxx

= Verificación, validación y evaluación <sec:5>
Este capítulo trata sobre la verificación y validación del _software_
(@sec:5-ver-val) , y finalmente se da una evaluación completa de los resultados
obtenidos (@sec:5-evaluación).

== Verficiación y validación (_V&V_) <sec:5-ver-val>
La verificación y validación (_V&V_) es el proceso de determinar si los
requisitos para un sistema o componente están completos y son correctos, que
el producto de cada fase de desarrollo cumple los requisitos o condiciones
impuestas por la fase previa y que el sistema o componente finales se adhiere
a los requisitos especificados @IEEE24765-2017.

A continuación una lista de tests realizados al proyecto. Estos describen cómo
se han realizado, cuáles son los resultados y qué requisitos cubren. Al final
de esta sección se ofrece una matriz de trazabilidad para confirmar que todos
los requisitos están cubiertos.

Cabe mencionar que muchas de las propiedades del programa se pueden y han sido
validadas formalmente con SPARK, que utiliza probadores de teoremas por debajo.
Así que si el programa compila, implica que todas las propiedades del programa
son correctas, aunque eso no significa que los resultados sean correctos, sino
que es correcto el programa.

Puesto que como se ve en la @sec:3-análisis-de-requisitos, todos los requisitos
cubren todos los casos de uso. Solo basta con demostrar que todos los
requisitos están cubiertos para validar y verificar el programa. La
@srs:t-template muestra la plantilla que van a seguir los tests. La única
prueba que no se puede realizar es la relacionada al requisito de la
@srs:rnf-f1, pues no hay forma de asegurarse de que el modelo que va a la placa
tiene esa mínima puntuación.

#srs.show-template(reqs, srs.make-tag("T"), "t-template")
#srs.show-items(reqs, srs.make-tag("T"))

#srs.show-traceability(reqs, srs.make-tag("T"), comparing-tag: srs.make-tag("R", "F"))

#[
  #set page(flipped: true)
  #set align(horizon)
  #srs.show-traceability(reqs, srs.make-tag("T"), comparing-tag: srs.make-tag("R", "N"))
]

#pagebreak()

== Evaluación <sec:5-evaluación>
En esta sección se hace una evaluación del rendimiento del programa y un
análisis del mismo. Se hace un análisis en orden cronológico y se indica las
conclusiones a las que se llegó en cada paso.

=== Comparación en la validación <sec:5-validation>
En un primer lugar, como se ve en la @sec:8-planificación en la *tarea 3.3* se
hicieron pruebas de rendimiento con el algoritmo después de optimizarlo y
paralelizarlo. Las pruebas se realizan en dos dispositivos:

- _Slimbook_: Un portátil con un _12th Gen Intel i7-12700H (20) \@ 4.600GHz_
  con 20 hilos lógicos.
- Servidor: Un servidor con un _Intel Xeon Gold 6326 (64) \@ 3.500GHz_ con 64
  hilos lógicos.

Las comparación se realiza entre la de referencia, que está escrita en _Python
3_ con alguna parte en #cxx (la función `dtw`, deformación dinámica del
tiempo); y con la que de este proyecto que está escrita en #cxx. Cabe
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

#let pre-diagram(field) = {

  let xs = range(2)
  let ys1 = (pre-results-slimbook.at(field).at(3), pre-results-moa.at(field).at(3))
  let yerr1 = (pre-results-slimbook.at(field).at(4), pre-results-moa.at(field).at(4)) 
  let ys2 = (pre-results-slimbook.at(field).at(1), pre-results-moa.at(field).at(1))
  let yerr2 = (pre-results-slimbook.at(field).at(2), pre-results-moa.at(field).at(2)) 

  return lq.diagram(
             width: 5cm,
             legend: (position: left + top),

             xaxis: (
               ticks: ([_Slimbook_], [Servidor])
                 .map(rotate.with(-25deg, reflow: true))
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
}

#pre-table(pre-results-slimbook, "tab:pre-slimbook", [_Slimbook_: 20 hilos])
#pre-table(pre-results-moa, "tab:pre-moa", [Servidor: 64 hilos])

A partir de los resultados se ve que la versión de C++ es mucho más rápida que
la versión de Python bajo las mismas condiciones (paralelismo con el mismo
número de hilos). El incremento se ve menor en las funciones de _Simpson_ y
_Welch_, posiblemente porque ambas pertenecen a `SciPy` que utiliza `numpy` por
debajo, que está escrito en C. La función _max distance_ que está escrita en
Python 3 nativo es la que mejor mejora ve como se observa en la
@fig:original-simpson-welch, en la @fig:original-psd-energy y en la
@fig:original-max-dist-all.

#figure(
  caption: [Tiempo de ejecución de _Simpson_ y _Welch_],
  grid(
    columns: 2, row-gutter: 2mm, column-gutter: 0.75cm,
    pre-diagram("simpson"), pre-diagram("welch"),
    [_Simpson_],            [_Welch_])) <fig:original-simpson-welch>

#figure(
  caption: [Tiempo de ejecución de _PSD_ y _Energy_],
  grid(
    columns: 2, row-gutter: 2mm, column-gutter: 0.75cm,
    pre-diagram("psd"), pre-diagram( "energy"),
    [_PSD_],            [_Energy_])) <fig:original-psd-energy>

#figure(
  caption: [Tiempo de ejecución de _Max Distance_ y total],
  grid(
    columns: 2, row-gutter: 2mm, column-gutter: 0.75cm,
    pre-diagram("max_dist"), pre-diagram("all"),
    [Tiempo de ejecución],   [*Total*])) <fig:original-max-dist-all>

=== Punto flotante en C++ <sec:5-cxx-realtime>
#let make-result(language, compiler, checks, real, target, performance) = (
  language    : language,
  compiler    : compiler,
  checks      : checks,
  real        : real,
  target      : target,
  performance : performance,
)

#let results = (
  // ESP32C3
  make-result("C++", "GCC 14",  false, "float32", "ESP32C3", 1.64),
  make-result("Ada", "GNAT 14", false, "float32", "ESP32C3", 0.96),
  make-result("Ada", "GNAT 14", true,  "float32", "ESP32C3", 0.94),
  make-result("Ada", "GNAT 14", false, "fixed32", "ESP32C3", 10.36),
  make-result("Ada", "GNAT 14", true,  "fixed32", "ESP32C3", 7.68),
  // Raspberry Pi 4
  make-result("C++", "Clang 18", false, "float32", "RPi 4 (32 bits)", 470),
  make-result("Ada", "GNAT 10",  false, "float32", "RPi 4 (32 bits)", 286),
  make-result("Ada", "GNAT 10",  true,  "float32", "RPi 4 (32 bits)", 280),
  make-result("Ada", "GNAT 10",  false, "fixed32", "RPi 4 (32 bits)", 154),
  make-result("Ada", "GNAT 10",  true,  "fixed32", "RPi 4 (32 bits)", 104),
  // Raspberry Pi 3
  make-result("C++", "GCC 12",  false, "float32", "RPi 3 (64 bits)", 217),
  make-result("Ada", "GNAT 12", false, "float32", "RPi 3 (64 bits)", 199),
  make-result("Ada", "GNAT 12", true,  "float32", "RPi 3 (64 bits)", 147),
  make-result("Ada", "GNAT 12", false, "fixed32", "RPi 3 (64 bits)", 222),
  make-result("Ada", "GNAT 12", true,  "fixed32", "RPi 3 (64 bits)", 127),
  // Slimbook
  make-result("C++",     "Clang 18",   false, "float32", "Slimbook", 3036),
  make-result("Ada",     "GNAT 14",    false, "float32", "Slimbook", 1725),
  make-result("Ada",     "GNAT 14",    true,  "float32", "Slimbook", 1433),
  make-result("Ada",     "GNAT 14",    true,  "fixed32", "Slimbook", 2139),
  make-result("Ada",     "GNAT 14",    false, "fixed32", "Slimbook", 2195),
  make-result("Python3", "python3.10", false, "float32", "Slimbook", 1264),
)

En la *tarea 4.3* de planificación (@sec:8-planificación) se hizo las pruebas
en el dispositivo empotrado con C++ y punto flotante. Donde _RPi 3_ es la
_Raspberry Pi 3_, _RPi 4_ es la _Raspberry Pi 4_ y ESP32C3 es un placa con
procesador RISC-V de Espressif a 160MHz.  Y se obtuvo los siguientes resultados
(véase la @tab:cxx-pre-results):

#figure(
  caption: [Épocas por segundo con tipo flotante IEEE de 32 bits en C++.\
            Peor caso con 3 patrones],
  table(
    columns: (auto, auto, auto, auto),
    align: (left, left, left, horizon),
    table.header([*Lenguaje*], [*Compilador*], [*Máquina*], [*épocas/s*]),
    ..(results
       .filter((x) => (x.language == "C++"))
       .map((x) => ([#x.language], [#x.compiler], [#x.target], [$#x.performance$]))
       .flatten())
)) <tab:cxx-pre-results>

Se analiza el peor caso, en el que el algoritmo tiene que computar todas las
características y compararlo con tres patrones distintos. En la
@tab:cxx-pre-results se ve que en todas da tiempo real. Sin embargo, en la
ESP32C3, que carece de unidad de cómputo para punto flotante (FPU), está
bastante al límite. Como consecuencia se empezó a valorar la alternativa de
usar punto fijo a punto flotante.

=== Punto flotante en Ada
C++ carece de punto fijo de manera nativa, así que se decidió escribir el
algoritmo en Ada, que sí lo tiene. Aquí se analiza los resultados de las tareas
*5.3*, *5.4* y *6.3* (@sec:8-planificación). Después de haberlo implementado en
Ada, se obtuvieron los siguientes resultados que se muestran en la
@tab:ada-pre-results-float.

#figure(
  caption: [Épocas por segundo con tipo flotante IEEE de 32 bits en Ada.\ Peor
            caso con 3 patrones],
  table(
    columns: (auto, auto, auto, auto, auto),
    align: (left, left, left, left, horizon),
    table.header([*Lenguaje*], [*_Checks_*], [*Compilador*], [*Máquina*], [*épocas/s*]),
    ..(results
       .filter((x) => (x.language == "Ada" and x.real == "float32"))
       .map((x) => ([#x.language],
                    if x.checks { [Activados] } else { [Desactivados] },
                    [#x.compiler],
                    [#x.target],
                    [$#x.performance$]))
       .flatten())
)) <tab:ada-pre-results-float>

Se ve que no da en tiempo real por poco en la ESP32C3, y que procesa la mitad
de épocas por segundo que #cxx como se puede ver en la
@tab:float-ada-cxx-comparison. _Checks_ indica si están activados o no las
comprobaciones en tiempo de ejecución.

#let simple-comparison-diagram(real, target) = {
  let xs = range(1)
  let filtered = results.filter((x) => (x.real == real and x.target == target))
  let yss = (filtered.filter((x) => (x.language == "C++")).at(0).performance,
             filtered.filter((x) => (x.language == "Ada" and not x.checks)).at(0).performance,
             filtered.filter((x) => (x.language == "Ada" and x.checks)).at(0).performance)
  let mx = yss.reduce(calc.max)
  return lq.diagram(
    width: 5cm,
    ylim: (0, mx * 1.25),
    legend: (position: right + top),
    xaxis: (ticks: ([#target -- #real],).enumerate(), subticks: none),
    lq.bar(xs, (yss.at(0),), offset: -0.4, width: 0.4, label: [C++]),
    lq.bar(xs, (yss.at(1),), offset: 0.0, width: 0.4, label:  [Ada]),
    lq.bar(xs, (yss.at(2),), offset: 0.4, width: 0.4, label:  [Ada$\*$]))
}

#figure(
  caption: [Número de épocas procesadas por segundo en #cxx, Ada y en
            Ada con comprobaciones en tiempo de ejecución activadas
            (#box([Ada$\*$]))],
  grid(
    columns: 2,
    gutter: 0.75cm,
    simple-comparison-diagram("float32", "RPi 3 (64 bits)"),
    simple-comparison-diagram("float32", "ESP32C3"),
    simple-comparison-diagram("float32", "RPi 4 (32 bits)"),
    simple-comparison-diagram("float32", "Slimbook"),
  )) <tab:float-ada-cxx-comparison>

=== Punto fijo en Ada (pruebas preliminares)
Este análisis sigue siendo parte de la *tarea 6.3* de la planificación.
No se escribió en Ada porque se pensara que fuera a ir más rápido, sino para
comprobar su viabilidad con punto fijo. Se probó con dos tipos de punto fijo,
uno de 32 bits y otro de 64 bits, que pertenecen a $bb(X)_(32,-8)$ y
$bb(X)_(64,-16)$ respectivamente (véase la @sec:4-convenciones).
Las pruebas se hicieron para la ESP32C3 y se compiló con GNAT 14 se resumen
en la @tab:pruebas-preliminares-de-rendimiento-de-punto-fijo-en-ada.

#figure(
  caption: [Prueba preliminares de rendimiento de punto fijo en Ada.],
  table(
    columns: 3,
    align: (left, left, horizon),
    table.header([*Checks*], [*Tipo*], [*épocas/s*]),
    [Activados], [$bb(X)_(32,-8)$],  [`Constraint_Error`],
    [Activados], [$bb(X)_(64,-16)$], [$1.08$],
    [Desactivados], [$bb(X)_(32,-8)$],  [*$21.04$*],
    [Desactivados], [$bb(X)_(64,-16)$], [$1.12$],
  ))<tab:pruebas-preliminares-de-rendimiento-de-punto-fijo-en-ada>

Con 64 bits el resultado es correcto, pero rinde peor que la versión de punto
flotante de C++. Con pruebas activadas, la implementación con punto fijo de 32
bits termina abruptamente por un desbordamiento; pero con las pruebas
desactivadas alcanza un total de $21.04$ épocas por segundo.

Se llega a la conclusión de que a pesar de que no se puede traducir
directamente a punto fijo, pues habría que estudiar cómo evitar el
desbordamiento en todas las operaciones que pueden llegar a desbordar, la idea
de convertirlo a punto fijo es *viable*. Incluso si un pequeño porcentaje del
código utiliza punto fijo de 64 bits, el límite experimental nos dice que se
puede mejorar enormemente.

Después de estudiar por qué la solución de 64 bits era más lenta, se descubrió
que el 49.2% del tiempo total de ejecución lo pasaba dividiendo números en
punto fijo. Así que dividir o multiplicar números de 64 bits de punto fijo en
un procesador de 32 bits es muy costoso.

=== Punto fijo en SPARK <sec:5-spark-realtime>
Finalmente fue demostrando poco a poco la ausencia de errores de programación
utilizando SPARK junto a Ada. No dio tiempo a demostrar todas las funciones,
pero pruebas unitarias y funcionales no llevan a error. Falta por demostrar
formalmente `FFT` (_Fast Fourier Transform_) y `Welch`.  Aun así, los
resultados se pueden ver en la @tab:pruebas-finales-ada.

#figure(
  caption: [Pruebas finales de rendimiento de punto fijo en SPARK + Ada.],
  table(
    columns: 4,
    align: (left, left, left, horizon),
    table.header([*Compilador*], [*Checks*], [*Máquina*], [*épocas/s*]),
    ..(results.filter((x) => (x.real == "fixed32" and x.language == "Ada"))
           .map((x) => (
             [#x.compiler],
             if x.checks { [Activados] } else { [Desactivados] },
             [#x.target],
             [$#x.performance$]))
           .flatten())
  )) <tab:pruebas-finales-ada>

Se observa que para la ESP32C3 supera con creces el requisito de tiempo real
de una época por segundo, con $10.36$ épocas por segundo. El tiempo adicional
permitiría o bien introducir código adicional para autoentrenamiento, o bien
dormir el dispositivo empotrado para disminuir el consumo energético.

#let complex-comparison-diagram(target, legend : none) = {
  let xs = range(1)
  let filtered = results.filter((x) => (x.target == target))
  let yss = (filtered.filter((x) => (x.language == "C++")).at(0).performance,
             filtered.filter((x) => (x.real == "float32" and x.language == "Ada" and not x.checks)).at(0).performance,
             filtered.filter((x) => (x.real == "float32" and x.language == "Ada" and x.checks)).at(0).performance,
             filtered.filter((x) => (x.real == "fixed32" and x.language == "Ada" and not x.checks)).at(0).performance,
             filtered.filter((x) => (x.real == "fixed32" and x.language == "Ada" and x.checks)).at(0).performance)
  let mx = yss.reduce(calc.max)
  return lq.diagram(
    width: 6.5cm,
    ylim: (0, mx * 1.25),
    legend: legend,
    xaxis: (ticks: ([#target],).enumerate(), subticks: none),
    lq.bar(xs, (yss.at(0),), offset: -0.8, width: 0.4, label: [C++ ($cal(F)_32$)]),
    lq.bar(xs, (yss.at(1),), offset: -0.4, width: 0.4, label: [Ada ($cal(F)_32$)]),
    lq.bar(xs, (yss.at(2),), offset:  0.0, width: 0.4, label: [Ada$\* (cal(F)_32$)]),
    lq.bar(xs, (yss.at(3),), offset:  0.4, width: 0.4, label: [SPARK ($bb(X)_32$)]),
    lq.bar(xs, (yss.at(4),), offset:  0.8, width: 0.4, label: [SPARK$\* (bb(X)_32$)]))
}

#figure(
  caption: [Comparación final de implementaciones. El eje de ordenadas indica
            el número de épocas por segundo. El asterisco indica que se ha
            compilado con comprobaciones en tiempo de ejecución activadas.],
  grid(
    columns: 2,
    complex-comparison-diagram("ESP32C3", legend: (position: left + top)),
    complex-comparison-diagram("RPi 3 (64 bits)"),
    complex-comparison-diagram("RPi 4 (32 bits)"),
    complex-comparison-diagram("Slimbook"),
  )
) <fig:final-comparison>

El uso de punto fijo únicamente tiene sentido en dispositivos que no tienen
FPU (unidad de cómputo de punto flotante). A medida que las características del
máquina mejoran, en específico las capacidades de punto flotante, disminuye el
rendimiento del punto fijo. Además de tener unidades de cómputo específicas
para procesar punto flotante, computadores actuales vectorizan fácilmente
operaciones con punto flotante y no punto fijo.

Como se ve en la @fig:final-comparison, en la ESP32C3, que no tiene FPU, el
número de épocas computadas por segundo sextuplica a las implementaciones con
punto flotante. En la Raspberry Pi 3 los resultados son más parecidos, la
diferencia entre la versión de #cxx y la de SPARK con punto fijo es
despreciable. Sin embargo, en la Raspberry Pi 4, la diferencia entre punto
flotante y fijo es abismal. Curioso es el resultado para el Slimbook, pues
punto fijo supera a punto flotante en Ada, pero no a #cxx; esto se puede
deber a que las implementaciones no son exactamente iguales y el autor ha ido
incluyendo optimizaciones.

El uso de punto fijo es complejo y hay que tener mucho cuidado al trabajar con
él. Para aplicaciones modernas en dispositivos relativamente potentes, es mejor
utilizar punto flotante. Sin embargo, para este proyecto se pidió un
dispositivo de bajo consumo, así que punto fijo supera a punto flotante en este
caso específico.

Otra conclusión pertinente que se puede sacar es el efecto de las
comprobaciones en tiempo de ejecución, que hace en este caso Ada, y el impacto
que tienen en el rendimiento global. Se ve que a medida que las características
del computador aumentan el impacto parece disminuir. Posiblemente por la
complejidad del _hardware_: predictores de saltos, cachés... Como se puede ver
en la @tab:perf-effect.

#let select(
  language    : none,
  compiler    : none,
  checks      : none,
  real        : none,
  target      : none) = (

  results.filter((x) =>
        (language == none or (x.language == language))
    and (compiler == none or (x.compiler == compiler))
    and (checks   == none or (x.checks   == checks))
    and (real     == none or (x.real     == real))
    and (target   == none or (x.target   == target)))
)


#let effect-table = ()
#let effects = ()
#{
  for result in select(language: "Ada", checks: false) {
    let other = select(language: "Ada",
                       checks:   true,
                       compiler: result.compiler,
                       real:     result.real,
                       target:   result.target).first()
    let effect = other.performance / result.performance
    effect = calc.round(effect, digits: 2)
    effects.push(effect);
    effect-table.push([#result.target])
    effect-table.push([#result.real])
    effect-table.push([#result.compiler])
    effect-table.push([$times #effect$])
  }
}

#let sum = (x, y) => (x + y)
#let average-effect = effects.reduce(sum) / effects.len()
#let sd-effect = calc.sqrt(effects.map((x) => calc.pow((x - average-effect), 2))
                           .reduce(sum) / effects.len())

#figure(
  caption: [Efecto en el rendimiento de activar las comprobaciones en tiempo
            de compilación en Ada. $mu = #calc.round(average-effect, digits: 2)$,
            $sigma = #calc.round(sd-effect, digits: 2)$],
  table(
    columns: 4,
    align: (left, left, left, left),
    table.header([*Máquina*], [*Tipo*], [*Compilador*], [*Efecto*]),
    ..effect-table
  )) <tab:perf-effect>

Finalmente y como curiosidad, he aquí las estadísticas del probador del
teoremas a punto de terminar el proyecto.

#figure(
  caption: [Estadísticas del análisis de SPARK],
  table(
    columns: 6,
    align: (left, center, center, center, center, center),
    table.header([*SPARK Analysis results*], [*Total*], [*Flow*], [*Provers*],
                 [*Justified*], [*Unproved*]),
    [Data Dependencies   ], [ 41], [ 41], [       .], [.], [.],
    [Flow Dependencies   ], [  .], [  .], [       .], [.], [.],
    [Initialization      ], [ 34], [ 34], [       .], [.], [.],
    [Non-Aliasing        ], [  3], [  3], [       .], [.], [.],
    [Run-time Checks     ], [618], [  .], [ 608    ], [4], [6],
    [Assertions          ], [125], [  .], [ 123    ], [1], [1],
    [Functional Contracts], [ 68], [  .], [  57    ], [8], [3],
    [LSP Verification    ], [  .], [  .], [       .], [.], [.],
    [Termination         ], [ 44], [ 41], [3 (CVC5)], [.], [.],
    [Concurrency         ], [  .], [  .], [       .], [.], [.],
    table.hline(stroke: black + 0.25pt),
    [Total               ], [933], [119 (13%)], [791 (85%)], [13 (1%)], [10 (1%)]
  ))
