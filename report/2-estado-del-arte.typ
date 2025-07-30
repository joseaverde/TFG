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

=== _Idris_
=== SPARK
SPARK 2014 es un lenguaje de programación definido formalmente y un conjunto de
herramientas de verificación diseñadas específicamente para permitir el
desarrollo de _software_ de alta integridad. Con SPARK los desarrolladores
pueden verificar formalmente propiedades su código tales como: flujo de
información, ausencia de errores en tiempo de ejecución, corrección funcional y
políticas de seguridad @SPARKBook.

SPARK 2014 está basado en un subconjunto del lenguaje de programación Ada. Ada
es particularmente apto para la verificación formal pues fue diseñado para
desarrollo de _software_ crítico, el cual usa de base. Ada 2012 introdujo el
uso de aspectos, que se pueden utilizar para denotar contratos en subrutinas.
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

Por ejemplo el siguiente programa en SPARK, que también es compatible con
Ada y puede compilarse utilizando un compilador de Ada, tiene varios problemas:
que las funciones `Get` y `Put` pueden lanzar excepciones inesperadas y que
`X + Y` puede desbordar. Y SPARK es capaz de reconocerlos.

```adb
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
```

Para solucionar el problema de las excepciones basta con añadir un bloque para
tratar excepciones inesperadas con `exception when others =>`. Para la suma
es más complicado, pues es la entrada del usuario y este puede introducir
cualquier valor. Una opción es utilizar un tipo más grande para almacenar el
resultado; otra opción sería utilizar un subrango de valores válidos para `X`
e `Y`; y otra opción podría ser identificar el desbordamiento o
subdesbordamiento antes de que ocurra e imprimir un mensaje de error. Por
ejemplo el primer caso (utilizar un tipo más grande).

```adb
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
```

De esta manera, uno se puede asegurar de que no va a surgir una excepción
inesperada. No se trata de evitar que el programa no tenga errores, se trata de
identificar los casos en los que pueda fallar y tratarlos adecuadamente.

Por ejemplo la función división `"/"` en Ada
se define implícitamente para el tipo entero (`Integer`). Sin embargo hay uno o
dos posibles puntos de fallo: el primero, y más obvio, es que la división entre
cero no está definida; el segundo, y solamente en ciertos computadores que por
ejemplo codifican los valores enteros en complemento a dos, es la división
entre $-1$ deborda no está definida cuando el numerador es $-2^(n-1)$ en un
entero de $n$ bits, pues $(-2^(n-1))/(-1) = 2^(n-1)$ no se puede codificar en un
entero de $n$ bits. En SPARK y en Ada se puede definir dicha propiedad como:

```adb
function "/" (Left, Right : in Integer) return Integer with
   Pre => (Right /= 0 and then
      (if Integer'First < -Integer'Last 
         and then Left = Integer'First then Right /= -1))
      or else raise Constraint_Error;
```

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
Existe paralelismo a nivel de instrucción (ILP), a nivel de datos (DLP), a
nivel de hilos y a nivel de solicitud (RLP).
@ComputerArchitecture.

==== Hilos
==== OpenMP
==== oneTBB
==== Ada parallel
