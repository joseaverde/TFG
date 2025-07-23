#import "@preview/theorion:0.3.3": *
#import cosmos.fancy: *
// #import cosmos.rainbow: *
// #import cosmos.clouds: *
#show: show-theorion

#import "@preview/algorithmic:1.0.0"
#import algorithmic: algorithm
#import "@preview/cetz:0.3.4"
#import cetz.draw: *
#import "@preview/fletcher:0.5.7" as fletcher: diagram, node, edge, shapes
#import "utility.typ": *

#show math.equation.where(block: false): box

= Implementación y despliegue
En este capítulo se describen tanto la implementación del algoritmo con todas
sus peculiaridades matemáticas y el despliegue del proyecto entero.

La parte de implementación se introduce con una enumeración de distintas
convenciones matemáticas que se usan a lo largo del capítulo, se ofrece una
descripción del algoritmo de manera global utilizando dichas convenciones, y
termina con un análisis de todos los algoritmos que se consideran relevantes
para el propio algoritmo.

En la sección de despliegue se describen los métodos que se utilizan para
desplegar el proyecto y su reproducibilidad. Se comentan los métodos de
compilación cruzada que se han utilizado para las distintas plataformas.

== Implementación <implementación>
La implementación original del algoritmo de detección de ataques epilépticos
que se analiza en este proyecto (PaFESD) /* TODO: Referenciar */ se escribió
en Python 3.10. En este trabajo se analiza el impacto en el rendimiento de
utilizar otros lenguajes de programación, compiladores, técnicas y
herramientas, de cara a un análisis en el impacto energético y de seguridad
del programa.

Que la implementación original estuviera escrita en Python dificulta bastante
la compilación cruzada. Especialmente para dispositivos empotrados como la
ESP32-C3, que tiene pocos megabytes de memoria disponibles para almacenar el
ejecutable, pues el tamaño de la suma de todas las dependencias superaba los
cientos de megabytes. Además uno de los requisitos fundamentales del proyecto
es que corriera en tiempo real. Por razones técnicas se ha optado por utilizar
otros lenguajes de programación.

Los lenguajes usados y representación de valores numéricos utilizadas son los
siguientes: C++ 20 con punto flotante de simple precisión, C++ 20 con punto
flotante de doble precisión, Ada 2022 con punto flotante de simple precisión,
Ada 2022 con punto flotante de doble precisión, SPARK 2014 con punto fijo y
Python 3.10 con punto flotante.

Para la implementación solo se considera la parte del algoritmo de validación
del artículo original; es decir, se supone que se ha pasado un filtro de paso
bajo y algo a la señal de entrada, y que se han marcado previamente los
segundos de la señal que contienen artefactos (pestañeos, errores de medición,
etcétera).


=== Convenciones matemáticas
Las siguientes convenciones matemáticas solo se utilizan en este capítulo y
permiten desambiguar algunos conjuntos como $bb(N)$. Los conjuntos clásicos
se denotan de la siguiente manera:

#let btrue = $top$
#let bfalse = $bot$

Los conjuntos clásicos se denotan como:

- $bb(B) = {bfalse, btrue}$ es el conjunto de los valores lógicos: falso
  ($bfalse$) y verdadero ($btrue$).
- $bb(Z) = {0, 1, -1, 2, -2, ...}$ es el conjunto de los números enteros.
- $bb(N) = {0, 1, 2, ...}$ es el conjunto de los números enteros no negativos.
- $bb(N)^+ = {1, 2, 3, ...}$ es el conjunto de los números enteros positivos.
- $bb(Q)$ es el conjunto de los números racionales.
- $bb(R)$ es el conjunto de los números reales.

Como la implementación en el computador se trabaja con números con un tamaño
fijo de _bits_, a continuación se definen ciertos conjuntos interesantes para
ello. Se supone que los números enteros y en punto fijo se codifican en
complemento a dos:

- $bb(I)_n = bb(Z) inter [-2^(n-1), 2^(n-1)-1], n in bb(Z), n > 1$ es el
  conjunto de enteros de computador de $n$ _bits_.
- $bb(M)_n = bb(Z) slash 2^n bb(Z)$ es el anillo de enteros módulo 2^n.
- $bb(X)_(b,f) = { 2^f x : x in bb(I)_b }, f in bb(Z)$ es el conjunto de números
  racionales en punto fijo binario de $b$ _bits_ multiplicados por $2^f$.
  Nótese que $bb(X)_(b,0) = bb(I)_b$. Se denota además el valor más pequeño no
  nulo en valor absoluto del conjunto como $delta_(b,f) = 2^(-f)$.
- $bb(F)_32$ es el conjunto de valores codificados en el estándar de coma
  flotante IEEE 754 de simple precisión.
- $bb(F)_64$ es el conjunto de valores codificados en el estándar de coma
  flotante IEEE 754 de doble precisión.

Un vector $v in S^n$ es una secuencia de $n$ elementos del conjunto $S$:
$v = (v_1, v_2, ..., v_n)$. Se denota por $v(i)$ o $v_i$ el i-ésimo elemento
del vector $v$; donde $v_1$ o $v(1)$ es el primer elemento y $v_n$ o $v(n)$ es
el último. No están definidos los elementos $v(k), k <= 0 or k > n$.

  $ S^1 = S $
  $ S^n = S^(n-1) times S $

Además la notación $S^+$, inspirada por la clausura de Kleene, denota:

  $ S^+ = union.big_(i in bb(N)^+) S^i $

Además, si se se define $S^0 != {}$, se puede extender la notación de
$S^* = S^0 union S^+$. Estas notaciones son útiles para definir vectores.

Una muestra $m in bb(R)$ es un valor que ha sido léido por un sensor de
encefalograma en un instante de tiempo, el valor puede ser tanto negativo como
positivo como nulo. Una señal $S_r(t), S: bb(R)->bb(R)$ es una función que
asocia para un sensor cualquiera $r$ en un instante de tiempo $t$ una muestra.

Un sensor $r$ lee a una razón de $s in bb(N)^+$ muestras por segundo, a esa
constante se denomina _stride_. Así que en vez de trabajar sobre una señal
continua, se trabaja sobre una señal discreta.

/* *** SIGNAL IMAGE *** */

#figure(
  cetz.canvas({
    let width = 0.2
    let sample = 15
    let base_y = 0
    let last = 64
    let draw-part(i, name:"") = {
      if (name == "") {
        rect((i * width, base_y), ((i + 1) * width, base_y + 1))
      } else {
        rect((i * width, base_y), ((i + 1) * width, base_y + 1), name: name)
      }
    }

    content(((last + 4) * width, base_y + 0.6), [............])

    for i in range(sample) { draw-part(i) }
    draw-part(sample, name: "mid")
    for i in range(sample + 1, last) { draw-part(i) }
    draw-part(last, name: "last")

    content((sample * width, 2), [Muestra], name: "sample")

    big-brace(0, (last + 7) * width, base_y - 0.85, base_y - 1.2)
    content(((last + 7) * width / 2, base_y - 1.4), [Señal])

    let start = 10
    let stop = 20
    big-brace(start * width, stop * width, base_y - 0.05, base_y - 0.4)
    content(((start + stop) / 2 * width, base_y - 0.6), [_Stride_])

    set-style(mark: (end: ">"))   // style for arrow
    line("mid", "sample")
  }),
  caption: [Señales, _strides_, y muestras]
)

A la hora de hacer el análisis se trabaja por épocas (_epochs_) en vez de
_strides_. Si _stride_ es una secuencia contigua de muestras, una época es una
secuencia contigua de _strides_. Como referencia, se ha utilizado un _stride_
de $s = 256 "muestras/s"$, épocas de 5 segundos (1280 muestras) y los valores
de las muestras suelen estar en el rango $-10000$ a $10000$.

=== Algoritmo
El algoritmo a implementar es el algoritmo 4 (_validation phase_) del artículo
en que se basa el proyecto @PaFESD. Para determinar si una época pertenece o no
a un ataque epiléptico se computan lo que el artículo llama _features_ (o
características) que son cinco funciones matemáticas: `max_distance`, `energy`,
`psd_1`, `psd_2` y `psd_3`. Si las características de una época están en
ciertos rangos determinados por el modelo, a época no es un artefacto y la
distancia utilizando el algoritmo de deformación dinámica del tiempo es lo
suficientemente pequeña para alguno de los patrones: el modelo dice que la
época puede tratarse de un ataque epiléptico.

El modelo (al que el artículo llama _batch_) es una 7-tupla:

  $ B = (B_M, B_E, B_P_1, B_P_2, B_P_3, B_D, B_Q), B in cal(B) $

  $ B_M, B_E, B_P_1, B_P_2, B_P_3 in bb(R) times bb(R) $
  $ B_D in bb(R) $
  $ B_Q = { S_q : bb(R) -> bb(R) } $

- $B_M$, $B_E$, $B_P_1$, $B_P_2$, $B_P_3$ son 2-tuplas de dos números reales
  $B_x = (B_x_l, B_x_h), B_x_l <= B_x_h$ que indican el rango válido de los
  resultados de evaluar las funciones `max_distance`, `energy`, `psd_1`,
  `psd_2` y `psd_3` respectivamente. Además, como se verá más adelante, los
  valores no negativos: $B_M, B_E, B_P_1, B_P_2, B_P_3 >= 0$.
- $B_D$ es el valor máximo del resultado del algoritmo de deformación dinámica
  del tiempo que determina si una época puede ser una ataque epiléptico. Nótese
  que la función solo retorna números no negativos. Se puede decir que
  $B_D >= 0$.
- $B_Q$ es un conjunto de señales finitas que contienen los patrones a
  contrastar.

Así, se dice que una época tiene un ataque si cumple todas las condiciones
mencionadas anteriormente:

  $ "¿ataque?"(e, b) &=& not "¿artefacto?"(e)                 \
                    &and& b_M_l <= "max_distance"(e) <= b_M_h \
                    &and& b_E_l <= "energy"(e) <= b_M_h       \
                    &and& b_P_1_l <= "psd_1"(e) <= b_M_1_h    \
                    &and& b_P_2_l <= "psd_2"(e) <= b_M_2_h    \
                    &and& b_P_3_l <= "psd_3"(e) <= b_M_3_h    \
                    &and& exists q in b_Q: "dtw"(e, q) <= b_M dot.op d_("th")
  $
  $ "¿ataque?": (bb(R) -> bb(R)) -> cal(B) -> bb(B) $

== Algoritmos

/*
#algorithm({
  import algorithmic: *
  Function(
    "Max-Distance",
    ("S"),
    {
      Comment[Computa la diferencia entre el valor máximo y mínimo de la época]
      Assign[min][$S(S'"First")$]
      Assign[max][$S(S'"First")$]
      For(
        $i in S'"First" + 1 .. S'"Last"$,
        {
          If(
            $S(i)<"min"$,
            {
              Assign[min][$S(i)$]
            }
          )
          If(
            $S(i)>"max"$,
            {
              Assign[max][$S(i)$]
            }
          )
        },
      )
      Return[$"max"-"min"$]
    },
  )
})
*/

=== `max distance`
`max_distance` es sin duda uno de los algoritmos más sencillos. Retorna la
diferencia entre el valor máximo y el valor mínimo de una época.
Sea $e in bb(R)^m$ una época de $m$ elementos, la entrada del algoritmo.
Obsérvese el diagrama de flujo siguiente: /* TODO: Referenciar */

#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((0,0), name: <A1>, [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((0,v-sep), align(center)[*2*:
      $"mín"_v <- e(1)$\
      $"máx"_v <- e(1)$\ $i <- 2$
      ], shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*2), name: <loop>, align(center)[*3*: ¿$i <= m$?], shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((0,v-sep*3), align(center)[
      *4*:
      $"mín"_v <- "mín"("mín"_v, e(i))$\
      $"mán"_v <- "mán"("mán"_v, e(i))$)],
      shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*4), name: <endloop>, align(center)[*5*: $i <- i + 1$])
    edge("l,u,u,r", "-|>")
    node((1, v-sep*2), name: <end>, align(center)[
      *6*: $"result"<-"máx"_v - "mín"_v$],
      shape: shapes.rect)
    edge(<loop.east>, <end.west>, "-|>")
    edge("-|>")
    node((1, v-sep*3), align(center)[*7*: Fin], shape: shapes.pill)
  }),
  caption: [Algoritmo `max_distance`]
)

Los puntos de fallo suelen ser cuando se indexa o cuando se opera. Indexar
fuera de la época no está definido; y si el resultado de una operación no se
puede representar en el conjunto es un error.

Se observa posibles fallos en:

- Paso *2*: Indexar el primer elemento de la época $e(1)$. Este falla si $m=0$.
  Es decir, si no hay ningún elemento en el vector. Eso no tiene mucho sentido,
  pero podría ocurrir, así que la primera restricción es que $m > 0$.
- Paso *5*: Si se utiliza un $i in bb(I)_b$ e $i = 2^(b-1)-1$, entonces al
  añadir $1$ desborda porque $2^(b-1) in.not bb(I)$. Esto ocurre cuando
  $m=2^(b-1)-1$. Otra restricción debe ser que $m < 2^(b-1)-1$.
- Paso *6*: Este paso solo da problemas trabajando con punto fijo. Por ejemplo,
  en un vector de $m = 2$ elementos que sea $e = (-δ_(b,f), 2^f (2^(b-1)-1))$.
  Al final del algoritmo $"máx"_v=2^f (2^(b-1)-1)$ y
  $"mín"_v=-delta_(b,f)$, y el resultado
  $"máx"_v - "mín"_v = 2^f (2^(b-1)-1) - (-delta_(b,f)) = 2^f (2^(b-1))
  in.not bb(X)_(b,f)$.

==== Análisis de punto fijo
La función en punto fijo depende, además de la época, en otros dos conjuntos:
El de entrada $bb(X)_(b_e,f_e)$ y el de salida $bb(X)_(b_s,f_s)$. De manera
que la época será un vector de $m$ elementos del primer conjunto:
$e in bb(X)_(b_e,f_e)^m$. Las variables $"mín"_v, "máx"_v in bb(X)_(b_e,f_e)$,
también pertenecen al conjunto de entrada. $i in bb(I)_b$ para un $b > 1$
cualquiera.

#definition(title: [$"conv"_(b,f) (x)$])[
  *Conversión de punto fijo*: Dado un valor de punto fijo
  $x = 2^f k, x in bb(X)_(b,f), k in bb(I)_b$, se define la operación de
  conversión al tipo de punto fijo a
  $x' = 2^(f') k, x' in bb(X)_(b',f'), k' in bb(I)_(b')$ y se denota como
  $"conv"_(b',f') (x) = x'$ donde
  $"conv"_(b',f'): bb(X)_(b,n) -> bb(X)_(b',f')$ y donde:

  $ k' = cases(floor(2^(f - f') k)", " & "si k >= 0",
               ceil(2^(f - f') k)", "  & "si k < 0") $

  Porque $2^f k = 2^(f') ((2^f) / 2^(f') k)$ y $k'$ aproxima dicho valor.
  Solo está definida la función cuando se cumple la inecuación
  $b' >= b + (f - f')$ que demuestra @thm:conv-bit-cond.
]<def:fixed-point-conversion>

/* FLOOR PROOFS */

#lemma[
  $floor(x) = n$ si y solo si $n <= x < n + 1$
] <lem:floor-range>

#corollary[
  Si $b > 0$, $a - b < b floor(a / b) <= a$.
] <col:mult-floor-quot>
#proof[
  Si $floor(a / b) = x$ entonces de acuerdo con @lem:floor-range:

  $ x <= a / b < x + 1  $
  $ b x <= b a / b < b (x + 1)  $
  $ b x <= a < b (x + 1)  $
  $ b floor(a / b) <= a < b (floor(a / b) + 1)  $
  $ b floor(a / b) <= a < b floor(a / b) + b  $
  $ a - b < b floor(a / b)  $
  $ a - b < b floor(a / b) <= a $
]

/* CEIL PROOFS */

#lemma[
  $ceil(x) = n$ si y solo si $n - 1 < x <= n$
] <lem:ceil-range>

#corollary[
  Si $b > 0$, $a <= b ceil(a / b) < a + b$
] <col:mult-ceil-quot>

#proof[
  Si $ceil(a / b) = x$ entonces de acuerdo con @lem:ceil-range:

  $ x - 1 < a / b <= x $
  $ b (x - 1)  < a <= b x $
  $ b (ceil(a / b) - 1) < a <= b ceil(a / b) $
  $ b ceil(a / b) - b < a => b ceil(a / b) < a + b $
  $ a <= b ceil(a / b) < a + b $
]

/* CONVERSION THEOREM */

#theorem[
  Dados $x in bb(X)_(b,f)$ y $x' = "conv"_(b',f') (x) in bb(X)_(b',f')$:
  si $f >= f'$ entonces $x = x'$: si $k >= 0$ se cumple que
  $0 <= x - x' < 2^(f')$ y si $k < 0$ se cumple que $0 <= x' - x < 2^f'$.
] <thm:conv-error>

#proof[
  Dados $x = 2^f k$ y $x' = 2^(f') k'$, donde $x' = "conv"_(b',f') (x)$.
  El valor del suelo o techo depende del valor de la diferencia $f - f'$.

  Si $f = f'$ y $k >= 0$, entonces $k' = floor(2^(f - f') k) = floor(k) = k$,
  porque $k in bb(I)_b$. Luego $x = x'$ y $x - x' = 0$.

  Si $f = f'$ y $k < 0$, entonces $k' = ceil(2^(f - f') k) = ceil(k) = k$,
  porque $k in bb(I)_b$. Luego $x = x'$ y $x - x' = 0$.

  Si $f > f'$ y $k >= 0$, entonces $k' = floor(2^(f - f') k) = 2^(f - f') k$.
  Luego $x' = 2^(f') k' = 2^(f') 2^(f - f') k = 2^f k = x$, por lo que
  $x - x' = 0$.

  Si $f > f'$ y $k < 0$, entonces $k' = ceil(2^(f - f') k) = 2^(f - f') k$.
  Luego (como para $f > f', k >= 0$) $x - x' = 0$.

  Si $f < f'$ y $k >= 0$, como $2^(f') k' = 2^(f') floor((2^f k) / 2^(f'))$, de
  acuerdo con @col:mult-floor-quot:

    $ 2^f k - 2^(f') < 2^(f') floor((2^f k) / 2^(f')) <= 2^f k $
    $ x - 2^(f') < x' <= x $
    $ -2^(f') < x' - x <= 0 $
    $ 0 <= x - x' < 2^f' $

  Si $f < f'$ y $k < 0$, como $2^(f') k' = 2^(f') ceil((2^f k) / 2^(f'))$, de
  acuerdo con @col:mult-ceil-quot:

    $ 2^f k <= 2^(f') ceil((2^f k) / 2^(f')) < 2^f k + 2^(f') $
    $ x <= x' < x + 2^(f') $
    $ 0 <= x' - x < 2^(f') $
]

De @thm:conv-error se obtienen propiedades muy relevantes. En primer lugar,
si se converte de punto un fijo multiplicado por un coeficiente más grande
a uno más pequeño, no se pierde hay error y por tanto no se pierde información.
Por ejemplo $a = 1 dot.op 2^(-2) in bb(X)_(32,-2)$, al convertirlo a un punto fijo
con un bit más de precisión en la parte fraccionaria como $"conv"_(32,-3) (a)$
se puede representar perfectamente como $2 dot.op 2^(-3)$, pues el numerador
debe ser entero.

Cuando $f < f'$, es decir hay más bits en la parte fraccionaria del tipo de
origen que el de destino, hay un error (que está acotado) en el rango
$[0, 2^(-f'))$. De vuelta al ejemplo anterior, al convertirlo a uno con menos
_bits_ en el denominador como $"conv"_(32,-1)$, no hay forma de representar el
valor $1/4$ con un numerador entero y el denominador $2$. Así que la conversión
da $0 dot.op 2^(-1)$, que está a como mucho $2^(-1)$ unidades del valor real.

Otras propiedades importantes son que el número de _bits_ del tipo de punto
fijo no influye en la conversión ni en el error. Además el error solamente
depende del el exponente $f'$ del coeficiente del tipo al que se convierte,
no depende de cuál era el exponente $f$ del valor origen. Esto simplifica
muchísimo el análisis del error.

#corollary[
  Dado $x in bb(X)_(b,f)$: si $x >= 0$ entonces $0 <= "conv"_(b,f) (x) <= x$; y
  si $x < 0$ entonces $0 >= "conv"_(b, f) (x) >= x$.
] <cor:conv-order>

#theorem[
  Si $b' >= b + (f - f')$, dado $x in bb(X)_(b,f)$,
  $"conv"_(b',f') (x) in bb(X)_(b',f')$
] <thm:conv-bit-cond>

#proof[
  Dado el conjunto $bb(Z)_(b,f)$ el valor máximo del conjunto es
  $M = 2^f 2^(b - 1) - delta_(b,f)$ y el valor mínimo es $m = -2^f 2^(b-1)$.
  La conversión se hace a $bb(Z)_(b',f')$:

    $ m' = "conv"_(b',f') (m) = ceil(-2^(f-f') 2^(b - 1)) 2^(f') $
    $ M' = "conv"_(b',f') (M) = floor(2^(f-f') (2^(b - 1) - delta_(b,f))) 2^(f') $

  Para $m'$, si $f >= f'$ de acuerdo con @thm:conv-error, $m' = m$, entonces
  solo se puede representar $m'$ en $bb(Z)_(b',f')$ si
  $f - f' + b - 1 <= b' - 1$ pues el valor mínimo de dicho conjunto es
  $-2^(b' - 1)$. Se necesita que si $f >= f' => b' >= b + (f - f')$.

  Si por el contrario $f < f'$, se deben encontrar los valores que satisfagan
  que $m' >= -2^(b' - 1)$. Por @cor:conv-order sabemos que $0 >= m' >= m$,
  luego el valor de la conversión no es positivo (así que el límite superior
  se puede ignorar) y no es menor que el valor original $m$. Luego la
  restricción debe ser que $m <= m' => 2^(b - 1) 2^f <= 2^(b' - 1) 2^(f')$.
  Luego $b - 1 + f <= b' - 1 + f' => b' >= b + (f - f')$.

  Para $M'$, si $f >= f'$, de acuerdo con @thm:conv-error, $M' = M$, entonces
  solo se puede representar $M'$ en $bb(Z)_(b',f')$ si 

  $ M = &floor(2^(f-f') (2^(b - 1) - delta_(b,f))) 2^(f')   \
      = &floor(2^(f-f'+b-1) - 2^(f')) 2^(f')                \
      = & 2^(f+b-1) - 2^(f')                                \
      <=& (2^(b' - 1) - 1) 2^(f')                           \
       =& 2^(f' + b' - 1) - 2^(f')
  $
  $ => f + b - 1 <= f' + b' - 1 => b' >= b + (f - f') $

  Finalmente si $f < f'$
]

=== Reducciones
=== Media
=== Varianza
=== _Energy_
=== Transformada de Fourier (FFT)
==== Recursivo a iterativa
==== _Caché_ y $omega^k_n$
==== Consideraciones para punto fijo
==== Método de _Simpson_
==== _Welch_
==== Deformación dinámica del tiempo
==== Densidad espectral de potencia
==== Validación
==== Entrenamiento

== Despliegue
=== Compilación cruzada y _toolchains_
=== Módulos de Python

// === Probador de teoremas
// SPARK 2014 es un lenguaje de programación basado y compatible con un
// subconjunto del lenguaje de programación Ada 2012.
// /* TODO: BHIAwS Ch 2, p.18 */
// SPARK tiene como principal cometido demostrar utilizando probadores de teoremas
// ciertas características y garantías del código fuente: la ausencia de ciertos
// tipos de errores en tiempo de ejecución y de ciertas propiedades lógicas del
// programa.
// 
// Para ello hace uso de contratos. Por ejemplo la función división `"/"` en Ada
// se define implícitamente para el tipo entero (`Integer`). Sin embargo hay uno o
// dos posibles puntos de fallo: el primero, y más obvio, es que la división entre
// cero no está definida; el segundo, y solamente en ciertos computadores que por
// ejemplo codifican los valores enteros en complemento a dos, es la división
// entre $-1$ deborda no está definida cuando el numerador es $-2^(n-1)$ en un
// entero de $n$ bits, pues $(-2^(n-1))/(-1) = 2^(n-1)$ no se puede codificar en un
// entero de $n$ bits. En SPARK y en Ada se puede definir dicha propiedad como:
// 
// ```ada
// function "/" (Left, Right : in Integer) return Integer with
//    Pre => (Right /= 0 and then
//       (if Integer'First < -Integer'Last 
//          and then Left = Integer'First then Right /= -1))
//       or else raise Constraint_Error;
// ```
// 
// Al predicado que se debe cumplir antes de poder llamar una subrutina se llama
// precondición y es el que llama la función el que debe comprobarlo. La
// postcondición es el predicado que se asegura que se cumple después de haber
// llamado a una subrutina, y es la propia subrutina la que debe comprobar que
// este sea cierto.
// 
// La idea de SPARK es juntar contratos con el probador de teoremas. Si se pueden
// identificar todos los casos de fallo (excepciones) y las condiciones que los
// producen, se podría llegar a garantizar completamente su ausencia. Esto
// permitiría eliminar comprobaciones inútiles y mejorar el rendimiento del
// programa.  Así pues, se ha decido dar una definición matemática del problema.


