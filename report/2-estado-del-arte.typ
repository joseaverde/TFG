#import "@preview/cetz:0.3.4"
#import cetz.draw: *
#import "utility.typ": *

= Estado del Arte
Este capítulo presenta las distintas tecnologías que guardan una estrecha
relación con el proyecto en cuestión: desde las técnicas para el procesamiento
de señales de encefalograma, hasta cuestiones relacionadas con la parte de
computación y entornos de ejecución.


/* ==== Detección de ataques epilépticos =================================== */
== Detección de ataques epilépticos
La epilepsia, una condición neurológica caracterizada por la actividad cerebral
anormal que resulta en episodios recurrentes de convulsiones
@anchundia2024revision. Poder determinar 

Existen distintas técnicas para la detección de ataques epilépticos por
computador. @PaFESD


/* ==== Arquitecturas ====================================================== */
== Arquitecturas
=== x86-64
=== RISC-V
=== ARM

/* ==== Representación de valores numéricos en un computador =============== */
== Representación de valores numéricos en un computador
Existen diversas técnicas para representar o codificar valores númericos en
un computador. Seleccionar una codificación adecuada para cada problema
individual puede tener un gran efecto en su rendimiento, así que es necesario
tenerlas todas en cuenta.

Solo se analizan las


=== Enteros



=== Racionales


=== Punto flotante

#let make-ieee(width, sign, exponent, fraction) = {
  cetz.canvas({
    let height = 0.8
    let last = 0
    for (size, colour, tag) in ((sign,     rgb("#c5fbfe"), [sign]),
                                (exponent, rgb("#9ffeae"), [exponent]),
                                (fraction, rgb("#fdaead"), [fraction])) {
      let from = last
      let to = last + size
      last = to
      for i in range(from, to) {
        rect((i * width, 0), ((i + 1) * width, height), fill: colour)
      }
      big-brace(from * width, to * width, -0.1, -0.3)
      content(((from + to) / 2 * width, -0.5), tag)
    }
  })
}

#figure(
  make-ieee(0.4, 1, 8, 23),
  caption: [IEEE 754 32 bits]
)

#figure(
  make-ieee(0.2, 1, 11, 52),
  caption: [IEEE 754 64 bits]
)


=== Punto fijo

/* ==== Probadores de teoremas ============================================= */
== Demostración interactiva de teoremas
=== _Rocq_
_Rocq_, hasta marzo de 2025 conocido como _Coq_ @Coq2Rocq, es un programa para
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
Y SPARK 2014 también añade sus propios aspectos para extender la capacidad de
análisis estático @learnSPARK.

#figure(
  image("img/01_spark_ada.png", width: 60%),
  caption: [Diagrama de Venn que muestra la relación entre SPARK y Ada @learnSPARK])

El juego de herramientes de GNATprove está basado en la colección de
compiladores de GCC y utiliza por debajo distintos probadores de teoremas como:
Alt-Ergo, Colibri, cvc5 y Z3 por defecto. También puede utilizar Coq 8.11
(actualmente conocido como Rocq a partir de la versión 9) para realizar
demostraciones interactivas de teoremas @SPARKaltprovers.

Por ejemplo, el siguiente programa en SPARK, que también es compatible con
Ada y puede compilarse utilizando un compilador de Ada, tiene varios problemas:
que las funciones `Get` y `Put` pueden lanzar excepciones inesperadas y que
`X + Y` puede desbordar. Y SPARK es capaz de reconocerlos.

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
ejemplo, el primer caso (utilizar un tipo más grande).

#code(
  caption: [Posible solución para eliminar las excepciones del
  @lst:2-example-1. El `when others` solo captura las excepciones de entrada y
  salida, ninguna de tipo numérico],
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

De esta manera, uno se puede asegurar de que no va a surgir una excepción
inesperada. No se trata de evitar que el programa no tenga errores, se trata de
identificar los casos en los que pueda fallar y tratarlos adecuadamente.

Por ejemplo, la función división `"/"` en Ada
se define implícitamente para el tipo entero (`Integer`). Sin embargo, hay uno o
dos posibles puntos de fallo: el primero, y más obvio, es que la división entre
cero no está definida; el segundo, y solamente en ciertos computadores que por
ejemplo codifican los valores enteros en complemento a dos, es la división
entre $-1$ deborda no está definida cuando el numerador es $-2^(n-1)$ en un
entero de $n$ bits, pues $(-2^(n-1))/(-1) = 2^(n-1)$ no se puede codificar en un
entero de $n$ bits. En SPARK y en Ada se puede definir dicha propiedad como:

#code(
  caption: [Precondiciones implícitas de la función división entera `"/"`.]
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


=== Rangos


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

==== Paralelismo y concurrencia en lenguajes de programación
===== En C++
En C++ existen las clases `std::thread` y su versión mejorada `std::jthread`
que dan soporte para hilos en dicho lenguaje. `std::jthread` tiene el mismo
comportamiento que `std::thread`, pero cuando se destruye el objeto además hace
_join_ (espera a que el hilo termine de ejecutar @cppreferenceJoin) y puede ser
cancelado o parado en ciertas condiciones @cppreferenceJthread. El siguiente
programa computa la suma de los elementos de un vector de manera paralela.

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
@cppreferenceExecutionPolicy.

#code(
  caption: [Reimplementación del @lst:2-cpp-thread, pero utilizando la política
            de ejecución paralela de C++17.]
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
  paralela @cppreferenceExecutionPolicy.]
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
@ISOAda2022.

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
hacer cambios profundos la interfaz del lenguaje @Ada202xSupport. Sin embargo,
en el @lst:2-ada-parallel_reduce y el @lst:2-ada-parallel se puede ver ejemplos
de cómo sería.

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

==== OpenMP
Otro método para conseguir

==== oneTBB
Otro método para conseguir
