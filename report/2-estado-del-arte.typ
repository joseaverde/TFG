#import "@preview/cetz:0.3.4"
#import cetz.draw: *
#import "utility.typ": *

= Estado del arte <sec:2>
Este capítulo presenta las distintas tecnologías que guardan una estrecha
relación con el proyecto en cuestión: desde las técnicas para el procesamiento
de señales de encefalograma, hasta cuestiones relacionadas con la parte de
computación y entornos de ejecución.


/* ==== Detección de ataques epilépticos =================================== */
== Detección de ataques epilépticos
Según la Organización Mundial de la Salud, la epilepsia es una enfermedad
crónica que afecta al cerebro y a gente de todas las edades. Se calcula que
alrededor de 50 millones de personas en todo el mundo la padecen,
lo que la posiciona como una de las afecciones neurológicas más comunes de todo
el mundo. El riesgo de muerte prematura es tres veces mayor a resto de
la población.

Además los ataques epilépticos se pueden controlar, alrededor del 70% de la
gente que sufre de epilepsia puede vivir sin experimentar ninguno con las
medicación apropiada. Una etiología documentada de la epilpsia o patrones
anormales de encefalografía son los predictores más consitentes de epilepsia
@WHOEpilepsy.

Una evaluación de detección de ataques epilépticos utilizando clasificadores de
apredizaje automático explica que aplicar aprendizaje automático directamente
sobre el conjunto de datos de encefalografía en bruto puede no producir
patrones sensatos. Por lo tanto, seleccionar las características estadísticas
de la señal de encefalograma es crucial, entre ellas se encuentran distintas
técnicas de transformación como: transformaciones de ondículas discretas
(_discrete wavelet transformation_ o DWT), transformaciones de ondículas
continuas (_continuous wavelet transformation_ o CWT), transformadas de Fourier
(_Fourier transformation_, FT), transformaciones de conseno discretas
(_discrete cosine transformation_ o DCT), descomposición en valores singulares
(_singular value decomposition_ o SVD) entre otras @siddiqui2020review.

=== PaFESD: _Patterns Augmented by Features Epileptic Seizure Detection_
O patrones aumentados por características de detección de ataques epilépticos,
es un método para detección de ataques epilépticos a partir de las
características estadísticas de encefalografía además de comparación de
patrones. Las características principales son tres:

1. $f_1(E_i)$: la amplitud pico a pico (_pk-pk_) de la época, que es la
   diferencia entre el el valor máximo y mínimo. En este proyecto se refiere a
   ella como _max distance_ o distancia máxima.
2. $f_2(E_i)$: la energía de la época como la varianza de la época. En este
   proyecto se la denomina _energy_ o energía.
3. $f_3(E_i)$, $f_4(E_i)$, $f_5(E_i)$: es la potencia integrada de la banda
   espectral de la señal de encefalograma en las bandas, respectivamente:
   de $2.5$ a $12$ Hz ,cubriendo casi completamente las ondas cerebrales
   $alpha$, $beta$ y $theta$; de $12$ a $18$ Hz, banda baja de las ondas
   $beta$; y de $18$ a $35$ Hz, bandas altas de las ondas $beta$. Se computa
   como la integral de cada banda de la densidad espectral de potencia de la
   época a partir del método de Welch. A estas se las denomnina $"PSD"_1$,
   $"PSD"_2$ y $"PSD"_3$ en el resto del documento.

Finalmente se utiliza la distancia dada por el algoritmo de la deformación
dinámica del tiempo (DTW) para comparar cada época de señal con varios patrones
previamente seleccionados @PaFESD.

Se ha decidido utilizar este estudio como referencia y se ha colaborado con los
investigadores originales que escribieron el artículo. La implementación de
referencia está alojada en
https://github.com/PPMC-DAC/PaFESD-Epileptic-Seizure-Detection. Entre otras
razones, porque con tan solo un 22% de datos de entrenamiento consigue una
sensibilidad del 99.6%, una especificidad del 100% y una precisión del 98.3%,
mucho mejor que otros modelos que necesitan mucho más porcentaje de
entrenamiento @PaFESD.

/* ==== Probadores de teoremas ============================================= */
== Demostración interactiva de teoremas
=== _Rocq_
_Rocq_ conocido hasta marzo de 2025 como _Coq_ @Coq2Rocq, es un programa para
la demostración interactiva de teoremas matemáticos desarrollado por el
instituto francés de investigación en informática y automática (INRIA).
Fue diseñado para desarrollar demostraciones matemáticas y para escribir
especificaciones formales @Rocq.

_Rocq_ se ha utilizado para demostrar gran variedad de teoremas matemáticos,
entre los más notables: el teorema de los cuatro colores @gonthier2008formal,
el algoritmo del castor para $"BB"(5) = 47 176 870$ @BeaverProof o el
grupo fundamental de un círculo en teoría de tipos homotópicos
@FundamentalGroupOfTheCircle.

=== _Lean_
_Lean_ es a la vez un probador de teoremas y un lenguaje de programación, que
permite escribir código correcto, mantenible y verificado formalmente
@LeanLang. Hoy en día el su desarrollo está apoyado por la organización sin
ánimo de lucro _Lean Focused Research Organization_ (FRO) @LeanFRO. Un ejemplo
de cómo definir los números naturales (cero incluido) basándose en los axiomas
de Peano en Lean se ve en la @fig:LeanPeano:

#code(
  caption: [Axiomas de Peanno en Lean],
  tag: "fig:LeanPeano",
)[
```lean
inductive Natural : Type
| zero : Natural
| succ : Natural -> Natural
```]

=== SPARK
SPARK 2014 es un lenguaje de programación definido formalmente y un conjunto de
herramientas de verificación diseñadas específicamente para permitir el
desarrollo de _software_ de alta integridad. Con SPARK los desarrolladores
pueden verificar formalmente las propiedades de su código tales como: flujo de
información, ausencia de errores en tiempo de ejecución, corrección funcional y
políticas de seguridad @SPARKBook.

SPARK 2014 está basado en un subconjunto del lenguaje de programación Ada. Ada
es particularmente apto para la verificación formal pues fue diseñado para
desarrollo de _software_ crítico, el cual usa de base. Ada 2012 introdujo el
uso de aspectos que se pueden utilizar para denotar contratos en subrutinas.
Además SPARK 2014 también añade sus propios aspectos para extender la capacidad
de análisis estático @learnSPARK, como se muestra en la @fig:vennspark

#figure(
  image("img/01_spark_ada.png", width: 60%),
  caption: [Diagrama de Venn que muestra la relación entre SPARK y Ada @learnSPARK])
  <fig:vennspark>

El juego de herramientes de _GNATprove_ está basado en la colección de
compiladores de GCC y utiliza por debajo distintos probadores de teoremas como:
_Alt-Ergo_, _Colibri_, _cvc5_ y _Z3_ por defecto. También puede utilizar
_Coq_ 8.11 (actualmente conocido como Rocq a partir de la versión 9) para
realizar demostraciones interactivas de teoremas @SPARKaltprovers.

Por ejemplo, el siguiente programa en SPARK (@lst:2-example-1), que también es
compatible con Ada y puede compilarse utilizando un compilador de Ada, tiene
varios problemas: que las funciones `Get` y `Put` pueden lanzar excepciones
inesperadas y que `X + Y` puede desbordar. Y SPARK es capaz de reconocerlos.

#code(
  caption: [Ejemplo de programa válido en Ada y SPARK, pero en el que pueden
            saltar excepciones.],
  tag: "lst:2-example-1"
)[```adb
with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;
procedure Program with SPARK_Mode is
   X, Y : Integer;
begin
   Put ("Introduzca dos números: ");
   Get (X);
   Get (Y);

   Put ("La suma de ");
   Put (X, 1);
   Put (" + ");
   Put (Y, 1);
   Put (" es ");
   Put (X + Y, 1);
   New_Line;
end Program;
```]

Para solucionar el problema de las excepciones basta con añadir un bloque para
tratar excepciones inesperadas con `exception when others =>`. Para la suma
es más complicado, pues es la entrada del usuario y este puede introducir
cualquier valor. Una opción es utilizar un tipo más grande para almacenar el
resultado; otra opción sería utilizar un subrango de valores válidos para `X`
e `Y`; y otra opción podría ser identificar el desbordamiento o
subdesbordamiento antes de que ocurra e imprimir un mensaje de error. Por
ejemplo, el primer caso (utilizar un tipo más grande) se muestra en el
@lst:2-spark-sol.

#code(
  caption: [Posible solución para eliminar las excepciones del
  @lst:2-example-1. El `when others` solo captura las excepciones de entrada y
  salida, ninguna de tipo numérico],
  tag: "lst:2-spark-sol",
)[```adb
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Long_Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Long_Integer_Text_IO;
procedure Program with SPARK_Mode is
   X, Y : Integer;
   R : Long_Integer;
begin
   Put ("Introduzca dos números: ");
   Get (X);
   Get (Y);

   Put ("La suma de ");
   Put (X, 1);
   Put (" + ");
   Put (Y, 1);
   Put (" es ");
   R := Long_Integer (X) + Long_Integer (Y);
   Put (R, 1);
   New_Line;
exception
   when others =>
      Put_Line ("Ha ocurrido un error");
end Program;
```]

Así se puede garantizar que no va a surgir una excepción
inesperada. No se trata de evitar que el programa no tenga errores, se trata de
identificar los casos en los que pueda fallar y tratarlos adecuadamente.

Por ejemplo, la función división `"/"` en Ada
se define implícitamente para el tipo entero (`Integer`). Sin embargo, hay uno o
dos posibles puntos de fallo: el primero, y más obvio, es que la división entre
cero no está definida; el segundo, y solamente en ciertos computadores que por
ejemplo codifican los valores enteros en complemento a dos, es que la división
entre $-1$ deborda no está definida cuando el numerador es $-2^(n-1)$ en un
entero de $n$ bits, pues $(-2^(n-1))/(-1) = 2^(n-1)$ no se puede codificar en un
entero de $n$ bits. En SPARK y en Ada se puede definir dicha propiedad como
se muestra en el @lst:spark-div-pre.

#code(
  caption: [Precondiciones implícitas de la función división entera `"/"`.],
  tag: "lst:spark-div-pre",
)[```adb
function "/" (Left, Right : in Integer) return Integer with
   Pre => (Right /= 0 and then
      (if Integer'First < -Integer'Last 
         and then Left = Integer'First then Right /= -1))
      or else raise Constraint_Error;
```] 

Al predicado que se debe cumplir antes de poder llamar una subrutina se llama
precondición y es el que llama la función el que debe comprobarlo. La
postcondición es el predicado que se asegura que se cumple después de haber
llamado a una subrutina, y es la propia subrutina la que debe comprobar que
este sea cierto.

/* ==== Técnicas de programación =========================================== */
== Técnicas de programación
=== Diseño por contrato
El diseño por contrato, término acuñado por Bertrand Meyer que está conectado
con el diseño del lenguaje de programación _Eiffel_ @meyer2002applying.
En contra de la llamada «programación defensiva», que obliga a los
programadores a proteger todos y cada uno de los módulos para cualquier caso,
que añade código redundante y que complica el código, propone la noción de
contrato @meyer2002applying. Un contrato se divide en dos partes: una
precondición, que la parte que ofrece el servicio espera que sea cierta; y una
postcondición, las garantías que ofrece el servicio a la parte que lo utiliza
@meyer2002applying.

Varios lenguajes de programación dan soporte nativo para contratos, entre ellos
Eiffel (el primer lenguaje) @EiffelDbC, Ada desde la versión 2012 del lenguaje
@Ada2012PrePost y en #cxx hay una propuesta para la versión 26 del lenguaje,
pero es controvertida @Cxx26Contracts.


=== Rangos
La programación basada en rangos consiste en aplicar y combinar distintos
operadores sobre secuencias y vistas de objetos. Desde #cxx 20 están en #cxx,
la biblioteca de rangos es una extensión y generalización
de las bibliotecas de algoritmos e iteradores que las hace más potentes al
hacerlas combinables y menos propensas a errores @cppreferenceRanges.

==== _range-v3_
Es la biblioteca original en la que se basa la biblioteca de rangos del
estándar de C++ se llama _range-v3_ y fue desarrollada por Eric Niebler
@rangev3. Contiene más vistas y acciones que las que están estadarizadas en
#cxx 23 y es compatible con las del estándar. Una parte de las funciones
estandarizadas todavía no han sido implementadas en los compiladores de #cxx
más relevantes @cxxcompilersupport, así que todavía sigue siendo útil.
Un ejemplo de uso se puede ver en el @lst:2-cpp-range-v3.

#code(
  caption: [Ejemplo de uso de la biblioteca _range-v3_ para rangos.],
  tag: "lst:2-cpp-range-v3"
)[```cpp
constexpr auto result = accumulate
                      ( views::ints(0)
                      | views::remove_if([](int i){return i % 2 == 1;})
                      | views::transform([](int i){ return i * i; );
                      | views::take(3)
                      );

static_assert(result == 12);
```]



==== _flux_
Una biblioteca que utiliza un modelo alternativo para trabajar con rangos y
vistas es _flux_, cuya implementación está alojada en
https://github.com/tcbrindle/flux . En vez de utilizar el operador _pipe_
(`|`) como hace el la biblioteca de rangos estandarizada y _range-v3_, utiliza
notación punto (`.`), como se puede ver en el @lst:2-cpp-flux.

#code(
  caption: [Ejemplo de uso de la biblioteca _flux_ para rangos.],
  tag: "lst:2-cpp-flux"
)[```cpp
constexpr auto result = flux::ints()                        // 0,1,2,3,...
                         .filter(flux::pred::even)          // 0,2,4,6,...
                         .map([](int i) { return i * 2; })  // 0,4,8,12,...
                         .take(3)                           // 0,4,8
                         .sum();                            // 12

static_assert(result == 12);
```]

=== Paralelismo
El paralelismo es un conjunto de técnicas que permite realizar varias tareas de
manera simultánea para mejorar su eficiencia. En aplicaciones hay básicamente
dos tipos de paralelismo:

1. Paralelismo a nivel de datos (_data-level parallelism_ o DLP): nace de la
   posibilidad de operar muchos datos al mismo tiempo.
2. Paralelismo a nivel de tarea (_task-level parallelism_ o TLP): aparece
   porque distintos trabajos pueden operar de manera independiente.

A nivel de _hardware_ se pueden explotar esos dos tipos de paralelismo en
aplicaciones de cuatro maneras distintas:

1. Paralelismo a nivel de instrucción (_instruction-level parallelism_ o ILP):
   que explota el paralelismo a nivel de datos modestamente con ayuda del
   compilador y utiliza ideas como el _pipeline_ (cadena de montaje) y a veces
   utilizando ejecución especulativa.
2. Arquitecturas vectoriales (_vector architectures_), unidades de
   procesamiento gráfico (_graphic processor units_ o GPU) y juegos de
   instrucciones multimedia (_multimedia instruction sets_): que explotan el
   paralelismo a nivel de datos al aplicar una única instrucción a una
   colección de datos en paralelo.
3. Paralelismo a nivel de hilo (_thread-level parallelism_): que explota tanto
   paralelismo a nivel de datos o a nivel de tarea en un modelo de _hardware_
   fuertemente acoplado que permite la interacción entre hilos paralelos.
4. Paralelismo a nivel de petición (_request-level parallelism_): que explota
   el paralelismo entre tareas fuertemente desacopladas especificadas por el
   programador o el sistema operativo @ComputerArchitecture.

El concepto de concurrencia y de paralelismo están relacionados, pero existe un
matiz que los diferencia. La concurrencia signfica que varias acciones están
ocurriendo en el mismo intervalo de tiempo, mientras que paralelo indica que
están ocurriendo a la vez @proTBB.

La ley de Ambdahl, formulada por Gene Amdahl, habla sobre la máxima mejora en
el rendimiento que puede obtener un programa si una porción del sistema es
mejorada. Por ejemplo, si se mejora todo el programa para que vaya el doble de
rápido, el programa resultante irá el doble de rápido. Pero si solo duplicamos
la velocidad del programa en dos quintas partes, el sistema mejora por $1.25$.
Viene dado por la fórmula:

$ f = (1 / ((1 - t) + (t / m))) $

Donde $t$ es el la fracción del total del tiempo que tarda el sistema a
mejorar, $m$ es el factor con que se ha mejorado dicha parte y $f$ es el factor
de mejora máximo del sistema final @proTBB.

==== Paralelismo y concurrencia en lenguajes de programación
Algunos lenguajes de programación vienen con construcciones para realizar
concurrencia y a veces paralelismo de forma nativa y portable. En el caso de
este proyecto solo interesa ver cómo lo hacen los lenguajes de programación que
se han utilizado: #box([Python 3]), #cxx y Ada.

===== En C++
En C++ existen las clases `std::thread` y su versión mejorada `std::jthread`
que dan soporte para hilos en dicho lenguaje. `std::jthread` tiene el mismo
comportamiento que `std::thread`, pero cuando se destruye el objeto además hace
_join_ (espera a que el hilo termine de ejecutar @cppreferenceJoin) y puede ser
cancelado o parado en ciertas condiciones @cppreferenceJthread. El siguiente
programa del @lst:2-cpp-thread computa la suma de los elementos de un vector de
manera paralela.

#code(
  caption: [Suma de los elementos de un vector de manera paralela en C++,
  utilizando `std::thread`. Hay que partir a mano el vector en varios trozos.
  Es tedioso y todavía podría mejorarse.],
  tag: "lst:2-cpp-thread"
)[```cpp
#include <vector>
#include <thread>
#include <algorithm>

template <class InputIt, typename T>
T accumulate (InputIt first, InputIt last, T init, std::size_t thread_count) {
  std::vector<std::thread> thread_pool;
  std::vector<T> result_pool(init, thread_count);
  std::size_t const length = last - first;
  std::size_t const chunk = length / thread_count;
  std::size_t const rem = length % thread_count;
  for (std::size_t p = 0; p < thread_count; p++) {
    std::size_t first_index = p * chunk + std::min(p, rem);
    std::size_t last_index = (p + 1) * chunk + std::min(p + 1, rem);
    result_pool.push_back(init);
    thread_pool.emplace_back(std::move(std::thread{
      [&result_pool] (InputIt first, InputIt last, std::size_t index) {
        for (auto it = first; it != last; ++it) { result_pool[index] += *it; }
      },
      first + first_index, first + last_index, p}));
  }

  for (auto & thread : thread_pool) { thread.join(); }

  T result = init;
  for (auto const & x : result_pool) { result += x; }
  return result;
}```]

Sin embargo, desde C++17 se puede utilizar las políticas de ejecución
(_execution policy_), que permite ejecutar algoritmos de manera paralela
@cppreferenceExecutionPolicy, como se muestra en el
@lst:cpp-execution-policy-short y en el @lst:cpp-foreach-execution-policy.

#code(
  caption: [Reimplementación del @lst:2-cpp-thread, pero utilizando la política
            de ejecución paralela de C++17.],
  tag: "lst:cpp-execution-policy-short"
)[```cpp
#include <execution>
#include <numeric>
#include <functional>

template <class InputIt, typename T>
T accumulate (InputIt first, InputIt last, T init) {
  return std::reduce(std::execution::par, first, last, init, std::plus<T>{});
}
```]

#code(
  caption: [Bucle `for each` en C++, utilizando política de ejecución
  paralela @cppreferenceExecutionPolicy.],
  tag : "lst:cpp-foreach-execution-policy",
)[```cpp
int x = 0;
std::mutex m;
int a[] = {1, 2};
std::for_each(std::execution::par, std::begin(a), std::end(a), [&](int)
{
  std::lock_guard<std::mutex> guard(m);
  ++x;
});
```]

===== En Ada
La ejecución de un programa en Ada consiste en la ejecución de una o más
tareas. Cada tarea representa una actividad separable que procede
independientemente y concurrentemente entre puntos en los que interactúa con
otras tareas. Una única tarea, en el contexto de una construcción paralela,
puede representar múltiples hilos lógicos de control que pueden proceder en
paralelo; en otros contextos, cada tarea representa un hilo lógico de control
@ISOAda2022. Véase el @lst:2-ada-task.

#code(
  caption: [Implementación de la suma paralela de los elementos de un vector
            utilizando tareas en Ada. Tiene la misma funcionalidad que el
            @lst:2-cpp-thread.],
  tag: "lst:2-ada-task"
)[```ada
-- Specification
generic
   type Element_Type is private;
   type Index_Type is (<>);
   type Array_Type is array (Index_Type range <>) of Element_Type;
   Initial_Value : in Element_Type;
   with function "+" (Left, Right : in Element_Type) return Element_Type is <>;
function Accumulate (
   Item       : in Array_Type;
   Task_Count : in Positive := 1)
   return Element_Type with
   Pure, Global => null;

-- Body
function Accumulate (
   Item       : in Array_Type;
   Task_Count : in Positive := 1)
   return Element_Type is
   type Count_Type is new Long_Integer;
   task type Worker is
      entry Start (Index : Index_Type; Count : in Count_Type);
      entry Get (Result : out Element_Type);
   end Worker;

   task body Worker is
      Result : Element_Type := Initial_Value;
      Index  : Index_Type;
      Count  : Count_Type;
   begin
      accept Start (Index : Index_Type; Count : in Count_Type) do
         Worker.Index := Index;
         Worker.Count := Count;
      end Start;

      for I in 1 .. Count loop
         Result := Result + Item (Index);
         Index := Index_Type'Succ (Index);
      end loop;

      accept Get (Result : out Element_Type) do
         Result := Worker.Result;
      end Get;
   end Worker;

   Workers : array (Count_Type range 1 .. Count_Type (Task_Count)) of Worker;
   Result : Element_Type := Initial_Value;
   Temp   : Element_Type;
   Length : constant Count_Type := Item'Length;
   Chunk  : constant Count_Type := Length / Count_Type (Task_Count);
   Rest   : constant Count_Type := Length rem Count_Type (Task_Count);

begin
   for Partition in Workers'Range loop
      Workers (Partition).Start (
        Index => Index_Type'Val (Index_Type'Pos (Item'First)
                                + (Partition - 1) * Chunk
                                + Count_Type'Min (Rest, Partition - 1)),
        Count => Chunk + Count_Type'Min (Rest, Partition)
                       - Count_Type'Min (Rest, Partition - 1));
   end loop;

   for Worker of Workers loop
      Worker.Get (Temp);
      Result := Result + Temp;
   end loop;

   return Result;
end Accumulate;
```]

Sin embargo, la versión 2022 del estándar de Ada introduce el atributo
`'Parallel_Reduce` y el bloque de control de flujo `parallel do`, que permiten
simplificar la solución de este problema @ISOAda2022. Sin embargo, a día de hoy
ningún compilador de Ada ha implementado esta parte del estándar. Por ejemplo,
el equipo que más contribuye al _front-end_ de GCC de Ada (GNAT) dice que de
momento van a pausar el desarrollo para dar soporte al paralelismo en GNAT,
pues hay que tener en cuenta gran variedad de tecnologías actuales y habría que
hacer cambios profundos la interfaz del lenguaje @Ada202xSupport.
En el @lst:2-ada-parallel_reduce y el @lst:2-ada-parallel se muestran ejemplos
de cómo se codificaría.

#code(
  caption: [Implementación del @lst:2-ada-task de sumar los elementos de un vector,
            pero utilizando el nuevo atributo `'Parallel_Reduce`.],
  tag: "lst:2-ada-parallel_reduce"
)[```ada
-- Specification
generic
   type Element_Type is private;
   type Index_Type is (<>);
   type Array_Type is array (Index_Type range <>) of Element_Type;
   Initial_Value : in Element_Type;
   with function "+" (Left, Right : in Element_Type) return Element_Type is <>;
function Accumulate (Item : in Array_Type) return Element_Type with
   Pure, Global => null;

-- Body
function Accumulate (
   Item : in Array_Type)
   return Element_Type is (
   Item'Parallel_Reduce ("+", Initial_Value));
```]

#code(
  caption: [Ejemplo de los bloques de código `parallel for` y `parallel do` de
            Ada 2022.],
  tag: "lst:2-ada-parallel"
)[```ada
with Ada.Numerics.Elementary_Functions, Ada.Text_IO;
use Ada.Numerics.Elementary_Functions, Ada.Text_IO;
procedure Program is
   Arr : array (Positive range 1 .. 10_000) of Float;
begin
   parallel do
      Put_Line ("¡Hola mundo!");
   and
      Put_Line ("Hello, world!");
   and
      Put_Line ("Bonjour, le monde !");
   and
      Put_Line ("世界よ、こんにちは！");
   end do;

   parallel for I in Arr'Range loop
      Arr (I) := Sin (Float (I - 1) / Float (Arr'Length));
   end loop;
end Program;
```]

===== Python3
En Python3 existen dos módulos para trabajar con concurrencia, el primero es
`threading` que es similiar a `std::thread` en #cxx y a las `task` de
Ada @Python3Threading @Python3Versions; y otro específico para construcciones
paralelas llamado `multiprocessing` que utiliza procesos para paralelizar
algoritmos @Python3Multiprocessing. Obsérve el @lst:2-python3-parallel.

#code(
  caption: [Ejemplo de paralelismo en Python 3 con `multiprocessing`.
            Implementación de la suma paralela de los elementos de un vector
            como @lst:2-cpp-thread],
  tag: "lst:2-python3-parallel"
)[```python
import multiprocessing
from typing import Final

def split[T] (lst : list[T], cores : int) -> list[list[T]]:
  result : list[list[T]] = []
  length : Final[int]    = len(lst)
  chunk  : Final[int]    = length // cores
  rem    : Final[int]    = length % cores
  for i in range(cores):
    slice = lst[i * chunk + min(i, rem): (i+1) * chunk + min(i+1, rem)]
    result.append((slice,))
  return result

def parallel_sum[T] (lst : list[T], cores : int) -> int:
  with multiprocessing.Pool(cores) as p:
    return sum(p.starmap(sum, split(lst, cores), cores))
```]

==== OpenMP
OpenMP es una especificación para un juego de directivas de compilación,
rutinas de biblioteca y variables de entorno que se pueden utilizar para
especificar paralelismo de alto nivel en programas escritos en Fortran, C y
#cxx @OpenMPWhatIs.

OpenMP es una de las especificaciones para hacer paralelismo más utilizadas,
solo en GitHub en 2023 el 45% de los repositorios lo utilizan
@kadosh2023quantifying.


#code(
  caption: [Implementación del @lst:2-cpp-thread con OpenMP en #cxx.],
  tag: "lst:2-openmp"
)[```cpp
#include <omp.h>

template <class InputIt, typename T>
T accumulate (InputIt first, InputIt last, T init) {
  #pragma omp parallel for reduction(+:init)
  for (auto it = first; it != last; ++it) { init += *it; }
  return init;
}
```]

==== oneTBB
_Intel® oneAPI Threading Building Blocks_, o también conocido como opeTBB, es
una biblioteca flexible para mejorar el rendimiento que facilita añadir
paralelismo a aplicaciones complejas en multitud de arquitecturas aceleradas
@IntelOneTBB.

TBB (_Threading Building Blocks_) es una solución para escribir programas
paralelos en #cxx. TBB fue introducido en 2006, así que tiene soporte para
compiladores previos a #cxx 11, aunque características que se encuentran a
partir de #cxx 11 como soporte para funciones lambda hace TBB mucho más fácil
de comprender y utilizar @proTBB.

#code(
  caption: [Implementación del @lst:2-cpp-thread con oneTBB en C++.],
  tag: "lst:2-onetbb"
)[```cpp
#include <functional>
#include <oneapi/tbb.h>

template <class InputIt, typename T>
T accumulate (InputIt first, InputIt last, T init) {
  return tbb::parallel_reduce(
    tbb::blocked_range(range.begin(), range.end()),
    init,
    [&] (auto const & r, T acc) -> T {
      for (auto x : r) { acc += x; }
      return acc;
    },
    std::plus<T>{});
}
```]
