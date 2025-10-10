#import "@preview/touying:0.6.1": *
#import themes.metropolis: *
#import "@preview/numbly:0.1.0": numbly

#import "@preview/codly:1.3.0": *
#import "@preview/codly-languages:0.1.1": *
#import "@preview/lilaq:0.4.0" as lq

#show: codly-init.with()

#let azuluc3m = rgb("#000e78")

#set text(
  lang:   "es",
  region: "es")

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => self.info.institution,
  config-info(
    title: [Procesamiento de señales de encefalograma para la detección de ataques epilépticos.],
    subtitle: [Trabajo fin de grado],
    author: [Autor: José Antonio Verde Jiménez\
             Tutor: José Daniel García Sánchez],
    date: [13 de octubre de 2025],
    institution: [Grado en Ingeniería Informática -- Universidad Carlos III de Madrid],
  ),
  config-common(
    preamble: {
      codly(
        languages: (
          ada: (name: "Ada", color: rgb("#005a00")),
          adb: (name: "SPARK", color: rgb("#c69ed3")),
          cpp: (name: "C++", color: rgb("#659bd3")),
          python: (name: "Python 3", color: rgb("#ffd947")),
          lean: (name: "Lean", color: rgb("#6ba4ff")),
        )
      )
    }),
  config-colors(
    primary: azuluc3m,
    primary-light: rgb("#406ed8"),
    secondary: azuluc3m,
    neutral-lightest: rgb("#fafafa"),
    neutral-dark: rgb("#000e58"),
    neutral-darkest: rgb("#23373b"),
  )
)

#set heading(numbering: numbly("{1}.", default: "1.1"))

#show table: block.with(stroke: (y: 0.7pt))
#set table(
  row-gutter: 0.2em,   // Row separation
  stroke: (_, y) => if y == 0 { (bottom: 0.2pt) }
)

#let code(
  contents,
  caption: none,
  tag:     "") = [
    #show figure: set block(breakable: true)
    #figure(
    [
      #show raw: set text(size: 12.0pt)
      #contents
    ],
    caption: caption)
    #label(tag)
  ]


// START

#title-slide(logo: image("img/uc3m_logo.svg"))

= Introducción
== Motivación
// grupo de investigación de modelos de programación paralela y compiladores de
// la Universidad de Málaga
- Colaboración entre dos grupos de investigación (PPCM y ARCOS) de dos
  universidades (UMA y UC3M).
#pause
- Tema interesante y relevante.
  - Optimización.
  - Paralelismo.
  - Sistema en tiempo real duro.
#pause
- Un reto personal.
  - Aplicar lo aprendido.
  - Resolver un problema difícil.
  - Investigar.
#pause
- Aportar algo.

== Objetivos del proyecto
1. Módulo de Python 3 escrito en C++ para minimizar el tiempo de entrenamiento.
2. Sistema en tiempo real en un dispositivo empotrado.
  1. RISC-V
  2. Bajo consumo
  3. *ESP32C3*
3. Verificación formal.

= Estado de la cuestión
== PaFESD: _Patterns Augmented by Features Epileptic Seizure Detection_
Tres características estadísticas que deben estar en rango:

- Distancia pico a pico.
- Energía de la época (varianza).
- Potencia integrada de la banda espectral de la señal de encefalograma en las
  bandas:
  - $2.5$ a $12$ Hz ($P S D_1$)
  - $12$ a $18$ Hz ($P S D_2$)
  - $18$ a $35$ Hz ($P S D_3$)

Se usa la distancia utilizando el algoritmo de la deformación dinámica del
tiempo (DTW) para discriminar ataques.

== Demostración interactiva de problemas
- Rocq
  - Antes conocido como Coq
- Lean
  - Escrito en C++
  - Turing completo
- SPARK
  - Utiliza otros probadores (`Alt-Ergo`, `Colibri`, `cvc5`, `Z3`, `Coq`...).
  - Subconjunto de Ada con anotaciones adicionales.
  - Permite demostrar propiedades de un programa.
  - Detecta errores en tiempo de compilación.


== Técnicas de programación
- *Diseño por contrato*
  - Ada y SPARK
  - Eiffel
  - \* C++ 26
- *Rangos*
  - `range-v3`
  - `flux`
  - Estándar
- *Paralelismo*:
  - Hilos (C++ y Python 3) y tareas (Ada).
  - OneTBB.
  - OpenMP.
  - C++ y políticas de ejecución.

= Análisis
== Casos de uso
#slide[
- Craig Larman: _Applying UML and Patterns: An Introduction to Object-Oriented
  Analysis and Design and Iterative Development_.
- Desarrollo en cascada.
][
#figure(
  caption: [Casos de uso],
  image("uml/casos-de-uso.svg", height: 80%))
]

== Requisitos
#slide[
- Requisitos funcionales (*5*)
- Requsistos no funcionales (*13*)
#figure(
  caption: [Matriz de trazabilidad:\ Requisitos funcionales],
  image("img/requisitos-funcionales.png"))
][
#figure(
  caption: [Matriz de trazabilidad:\ Requisitos no funcionales],
  image("img/requisitos-no-funcionales.png"))
]

== Arquitectura
#figure(
  caption: [Arquitectura],
  image("uml/arquitectura.svg", height: 80%))

= Implementación en C++
== Módulo de Python 3
#code(caption: [Trucos de C++ con `pybind11`.])[```cpp
template <typename T>
constexpr std::span<T const> pyspan(pybind11::array_t<T> const & y) {
  return std::span{y.data(), static_cast<std::size_t>(y.size())};
}
template <class C>
auto pymove(C && y) -> pybind11::array_t<typename C::value_type> {
  using result_t = typename C::value_type;
  auto vector  = new C(std::forward<decltype(y)>(y));
  auto capsule = pybind11::capsule{vector, [](void * item) {
                                     delete static_cast<C *>(item);
                                   }};
  return pybind11::array_t<result_t>(vector->size(), vector->data(), capsule);
})```]

#pagebreak(weak: true)
#code(caption: [Facilidad de `pybind11` y C++.])[```cpp
PYBIND11_MODULE(signals, m) {
  m.doc() = "Signals library for Seizure Algorithm";
  m.def("simpson", simpson, pybind11::arg("y"), pybind11::arg("dx"));
  m.def("welch", welch);
  m.def("call_psd", call_psd);
  m.def("call_max_dist", call_max_dist);
  m.def("call_energy", call_energy);
  m.def("begin", optimization_step_begin, pybind11::arg("data_all"),
        pybind11::arg("stride"), pybind11::arg("ops"),
        pybind11::arg("query_all"), pybind11::arg("qstride"),
        pybind11::arg("qops"), pybind11::arg("mw"));
```]

== Vistas y rangos en C++
#code(caption: [Vistas y rangos en C++.])[```cpp
#include <range/v3/all.hpp>
constexpr auto sliding_window_view(auto size, auto stride) {
  return ranges::views::sliding(size) | ranges::views::stride(stride);
}
constexpr auto chunk_split_view(auto && view, auto size) {
  return view | ranges::views::chunk(view.size() / size + 1);
}

// Extracto de validator.hpp
auto window_view     = scv_view | sliding_window_view(epoch, stride);
auto indices         = std::views::iota(from, to);
for (auto && [epoch, idx] : ranges::views::zip(window_view, indices)) {
  // ...
}
```]

== Paralelización
#code(caption: [Paralelización en C++ con OpenMP])[```cpp
template <class Range, class Func>
void parallel_for_with_index(Range && rng, Func && func) {
  #pragma omp parallel for
  for (auto && item : std::forward<decltype(rng)>(rng)) {
    std::apply(std::forward<decltype(func)>(func), std::make_tuple(omp_get_thread_num(), item));
  }
}```]

#code(caption: [Paralelización en C++ con OneTBB])[```cpp
template <class Range, class Func>
void parallel_for_with_index(Range && rng, Func && func) {
  auto const range = std::move(std::forward<decltype(rng)>(rng));
  oneapi::tbb::parallel_for_each(range.begin(), range.end(), [&](auto && item) {
    std::apply(func, std::make_tuple(tbb::this_task_arena::current_thread_index(), item));
  });
}```]

== Resultados
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
_12th Gen Intel i7-12700H (20) \@ 4.600GHz_ con 20 hilos lógicos.
#pagebreak(weak: true)
#pre-table(pre-results-moa, "tab:pre-moa", [Servidor: 64 hilos])
_Intel Xeon Gold 6326 (64) \@ 3.500GHz_ con 64 hilos lógicos.

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

#pagebreak(weak: true)
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

- *1.64* épocas por segundo da tiempo real, pero se puede hacer mejor.
- En el futuro habría que limpiar la señal, filtrar artefactos.

= Implementación en Ada
== ¿Por qué?
- Pese a ser en general más lento que C++.
- Punto fijo en el estándar.
- Comportamiento no definido en C++ es comportamiento erróneo en Ada.
- Traducción rápida de C++ a Ada.
- GNAT pertenece al juego de compiladores de GCC
  - Intrínsecos del compilador para punto fijo.
  - Compilación cruzada.
- SPARK y contratos.

== Pruebas preliminares (punto flotante)
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

== Pruebas preliminares (punto fijo)
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

#focus-slide[
  Técnicamente se podrían procesar alrededor de\
  21 épocas por segundo

  #pause
  - Es viable

  #pause

  - _Después de eliminar los errores_
]

== Problemas
- Desbordamiento y subdesbordamiento de enteros con signo.
  - En C++ es comportamiento no definido (_undefined behaviour_).
  - En Ada es `Constraint_Error`.
#pause
- Precisión
  - 32 bits: `float` ($23+1+1$) y `double` ($52+1+1$).
  - Variabilidad del exponente.
  - Productos y divisiones usan el doble de bits.
#pause
  - *Divisiones ($49.2%$ del tiempo total en punto fijo de 64 bits).*
#pause
- Errores
  - En C++ son silenciosos.
  - En Ada lanzan excepciones. No capturables con la configuración de la
    biblioteca de tiempo de ejecución (_runtime_) del dispositivo empotrado
    (`No_Exception_Propagation`).

== SPARK
#focus-slide[
  SPARK
]

- Subconjunto de Ada con anotaciones adicionales.
- Probadores de teoremas: `Alt-Ergo`, `Colibri`, `cvc5`, `Z3`, `Coq`...
- Uniformizar valores porque: $x, y in (-1, 1) -> x y in (-1, 1)$.
- Hacer iterativa la transformada de Fourier.
- Punto fijo de 64 bits de manera dispersa.
- SPARK _Gold_: _*Division*, *Index*, *Length*, *Overflow*, *Range*, Tag,
  Elaboration and *Flow* checks_.

== Flujo de trabajo con SPARK
#code(caption: [Declaración de `Generic_Accumulation`])[```adb
generic
   type Fixed_Type is delta <>;
   type Index_Type is range <>;
   type Array_Type is array (Index_Type range <>) of Fixed_Type;
   First, Last : in Fixed_Type;
function Generic_Accumulation (
   Item : in Array_Type)
   return Array_Type with Ghost => True, Global => null,
   Pre => Item'Length > 0 and then (for all X of Item => X in First .. Last),
   Post => (declare Result renames Generic_Accumulation'Result;
      begin    Result'First = Item'First and then Result'Length = Item'Length
      and then Result (Item'First) = Item (Item'First)
      and then (for all I in Item'First + 1 .. Item'Last =>
                  Result (I - 1) in Positive (I - Item'First) * First
                                 .. Positive (I - Item'First) * Last
                  and then Result (I) = Result (I - 1) + Item (I))
      and then Result (Item'Last) in Item'Length * First .. Item'Length * Last);
```]
#pagebreak(weak:true)

Es decir:
- El $i"-ésimo"$ elemento del resultado de la función contiene el resultado
  parcial de la acumulación hasta el $i"-ésimo"$ elemento del vector de
  entrada.
- Si todos los elementos del vector `Item` están acotados en el rango
  `First` y `Last` (como indica la precondición).
  - Entonces sea $x = #text([`Generic_Accumulation`]) (#text([`Item`])) (i)$ el
    $i"-ésimo"$ elemento del vector resultado.
  - Se puede demostrar, como se ve en la postcondición, que entonces
    $x in [#text([`First`]) * i, #text([`Last`]) * i]$ (suponiendo que $i$
    empieza en 1).
  - Y que es igual al resultado anterior más el valor en el $i"-ésimo"$
    elemento de la entrada.

Por ejemplo:
 - `Generic_Accumulation`$([10, 3, 7, 6, 5])$ es $[10, 13, 20, 26, 31]$. El
   resultado es el último valor.

#pagebreak(weak:true)
#code(caption: [Implementación de `Generic_Accumulation`])[```adb
function Generic_Accumulation (Item : in Array_Type) return Array_Type is
   Result : Array_Type (Item'Range) := [others => 0.0];
begin
   Result (Item'First) := Item (Item'First);
   for Index in Item'First + 1 .. Item'Last loop
      pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
      pragma Loop_Invariant (
         (for all I in Item'First + 1 .. Index - 1 =>
            Result (I - 1) in Positive (I - Item'First) * First
                           .. Positive (I - Item'First) * Last
            and then Result (I) = Result (I - 1) + Item (I)
            and then Result (I) in Positive (I - Item'First + 1) * First
                              .. Positive (I - Item'First + 1) * Last));
      Result (Index) := Result (Index - 1) + Item (Index);
   end loop;
   return Result;
end Generic_Accumulation; ```]


== Resultados
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


#let complex-comparison-diagram(target, legend : none, width: 6.5cm) = {
  let xs = range(1)
  let filtered = results.filter((x) => (x.target == target))
  let yss = (filtered.filter((x) => (x.language == "C++")).at(0).performance,
             filtered.filter((x) => (x.real == "float32" and x.language == "Ada" and not x.checks)).at(0).performance,
             filtered.filter((x) => (x.real == "float32" and x.language == "Ada" and x.checks)).at(0).performance,
             filtered.filter((x) => (x.real == "fixed32" and x.language == "Ada" and not x.checks)).at(0).performance,
             filtered.filter((x) => (x.real == "fixed32" and x.language == "Ada" and x.checks)).at(0).performance)
  let mx = yss.reduce(calc.max)
  return lq.diagram(
    width: width,
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
    columns: 3,
    lq.diagram(
      width: 6.5pt,
      ylim: (0, 1),
      legend: (position: left + top),
      xaxis: (ticks: ([],).enumerate(), subticks: none),
      yaxis: none,
      lq.bar(range(1), (0,), offset: -0.8, width: 0.4, label: [C++ ($cal(F)_32$)]),
      lq.bar(range(1), (0,), offset: -0.4, width: 0.4, label: [Ada ($cal(F)_32$)]),
      lq.bar(range(1), (0,), offset:  0.0, width: 0.4, label: [Ada$\* (cal(F)_32$)]),
      lq.bar(range(1), (0,), offset:  0.4, width: 0.4, label: [SPARK ($bb(X)_32$)]),
      lq.bar(range(1), (0,), offset:  0.8, width: 0.4, label: [SPARK$\* (bb(X)_32$)])),
    complex-comparison-diagram("ESP32C3"),
    complex-comparison-diagram("RPi 3 (64 bits)"),
    complex-comparison-diagram("RPi 4 (32 bits)"),
    complex-comparison-diagram("Slimbook"),
  )
) <fig:final-comparison>




== Estadísticas del probador de teoremas
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

= Entorno socioeconómico
== Presupuesto
#figure(
  caption: [Coste total],
  table(
    columns: (auto, auto),
    align: (left+horizon, right+horizon),
    table.header([*Concepto*], [*Coste*]),
    [Recursos humanos          ], [6908.46 €],
    [Recursos materiales       ], [369.20 €],
    [Costes indirectos         ], [5175.00 €],
    table.hline(stroke: 0.3pt + black),
    [*Total del proyecto*      ], [*12452.66 €*],
    table.hline(stroke: 0.3pt + black),
    [Beneficio industrial (16%)], [1992.43€],
    [IVA (21%)                 ], [2615.06€],
    table.hline(stroke: 0.3pt + black),
    [*Importe final*           ], [*17478.56 €*]))

== Planificación
#figure(
  caption: [Diagrama Gantt del proyecto],
  image("uml/gantt.svg", height: 95%))

= Conclusiones
== Conclusiones del proyecto
Objetivos cumplidos:
1. Entrenamiento más rápido. Visto bueno del equipo de investigación de la UMA.
2. Tiempo real.
  1. ESP32C3
    1. C++ 1.64 épocas por segundo
    2. Ada 10.36 épocas por segundo
  2. Dependiendo de la calidad de la FPU mejora el rendimiento de punto
     flotante (vectorización, optimizaciones, ...).
  3. A falta de FPU, punto fijo puede llegar a ser más rápido que punto
     flotante.
  4. Trabajar con punto fijo es tedioso y requiere demostraciones.
3. Exceptuando Welch y la transformada de Fourier, se ha verificado formalmente
   todo el proyecto con un probador de teoremas.

== Conclusiones personales
1. Varios lenguajes de programación.
2. SPARK en un proyecto real.
3. Aprendido
   1. Compilación cruzada
   2. Probadores de teoremas
   3. Dispositivos empotrados

== Trabajo futuro
1. Terminar de demostrar funciones.
2. Reentrenamiento en tiempo de ejecución.
3. Limpiado de señal y artefactos.


#focus-slide[
  ¡Muchas gracias por su atención!

  Preguntas
]
