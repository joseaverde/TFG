#import "@preview/theorion:0.3.3": *
// #import cosmos.fancy: *
#import cosmos.rainbow: *
// #import cosmos.clouds: *
#show: show-theorion

#import "@preview/algorithmic:1.0.0"
#import algorithmic: algorithm
#import "@preview/cetz:0.3.4"
#import cetz.draw: *
#import "@preview/fletcher:0.5.7" as fletcher: diagram, node, edge, shapes
#import "utility.typ": *

#show math.equation.where(block: false): box

= Diseño e implementación <sec:4>
*TODO*: REFORMULAR

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

*TODO: Reformular con la parte de diseño y uniformizar los capítulos*

== Estudio de la solución final <sec:4-estudio-de-la-solución-final>
La implementación original del algoritmo de detección de ataques epilépticos
que se analiza en este proyecto (PaFESD) /* TODO: Referenciar */ se escribió
en Python 3.10. En este trabajo se analiza el impacto en el rendimiento de
utilizar otros lenguajes de programación, compiladores, técnicas y
herramientas, de cara a un análisis en el impacto energético y de seguridad
del programa.

Que la implementación original estuviera escrita en Python dificulta bastante
la compilación cruzada. Esto es especialmente difícil para dispositivos empotrados como la
ESP32-C3, /* TODO: Referenciar */  que tiene pocos megabytes de memoria disponibles para almacenar el
ejecutable, pues el tamaño de la suma de todas las dependencias superaba los
cientos de megabytes. Además uno de los requisitos fundamentales del proyecto
es que corriera en tiempo real. Por estas razones técnicas se ha optado por utilizar
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


== Convenciones matemáticas <sec:4-convenciones>
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
- $bb(M)_n = bb(Z) slash 2^n bb(Z)$ es el anillo de enteros módulo $2^n$.
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

Además la notación $S^+$, inspirada por la clausura de Kleene /*TODO: Citar*/, denota:

  $ S^+ = union.big_(i in bb(N)^+) S^i $

Además, si se se define $S^0 != {}$, se puede extender la notación de
$S^* = S^0 union S^+$. Estas notaciones son útiles para definir vectores.

Una muestra $m in bb(R)$ es un valor que ha sido leido por un sensor de
encefalograma en un instante de tiempo, el valor puede ser tanto negativo como
positivo como nulo. Una señal $S_r (t), S: bb(R)->bb(R)$ es una función que
asocia para un sensor cualquiera $r$ en un instante de tiempo $t$ una muestra.

Un sensor $r$ lee a una razón de $s in bb(N)^+$ muestras por segundo, a esa
constante se denomina _stride_. Así que en vez de trabajar sobre una señal
continua, se trabaja sobre una señal discreta como se muestra en la
@fig:signal.

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
)<fig:signal>

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

=== `max distance` <algorithm-max-distance>
`max_distance` es sin duda uno de los algoritmos más sencillos. Retorna la
diferencia entre el valor máximo y el valor mínimo de una época.
Sea $e in bb(R)^m$ una época de $m$ elementos, la entrada del algoritmo.
Obsérvese el diagrama de flujo siguiente (@fig:flujo-max-distance).

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
      $"máx"_v <- "máx"("máx"_v, e(i))$)],
      shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*4), name: <endloop>, align(center)[*5*: $i <- i + 1$])
    edge("l,u,u,r", "-|>")
    node((1, v-sep*2), name: <end>, align(center)[
      *6*: $"result"<-"máx"_v - "mín"_v$],
      shape: shapes.rect)
    edge(<loop.east>, <end.west>, "-|>", [No])
    edge("-|>")
    node((1, v-sep*3), align(center)[*7*: Fin], shape: shapes.pill)
  }),
  caption: [Algoritmo `max_distance`]
)<fig:flujo-max-distance>

==== Problemas  <algorithm-max-distance-problems>
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

#definition(title: [$"conv"_f (x)$])[
  *Conversión de punto fijo*: Dado un valor de punto fijo
  $x = 2^f k, x in bb(X)_(b,f), k in bb(I)_b$, se define la operación de
  conversión al tipo de punto fijo a
  $x' = 2^(f') k, x' in bb(X)_(b',f'), k' in bb(I)_(b')$ (para alguna
  $b' in bb(N)^+$ y se denota como $"conv"_(f') (x) = x'$ donde
  $"conv"_(f'): bb(X)_(b,n) -> bb(Q)$ y donde:

  $ k' = cases(floor(2^(f - f') k)", " & "si" k >= 0,
               ceil(2^(f - f') k)", "  & "si" k < 0) $

  Porque $2^f k = 2^(f') ((2^f) / 2^(f') k)$ y $k'$ aproxima dicho valor.
]<def:fixed-point-conversion>

#note-box[
  Como dice más adelante el @thm:conv-bit-cond, si se cumple que
  $b' >= b + (f - f')$ se puede demostrar que si
  $x in bb(X)_(b,f)$ entonces $"conv"_(f') (x) in bb(X)_(b',f')$. La función original
  no depende del número de _bits_ del tipo de punto fijo de retorno, pues
  para las demostraciones es necesario trabajar sobre $bb(Q)$ y luego ya se
  puede añadir la restricción.
]

/* FLOOR PROOFS */

#lemma[
  $floor(x) = n$ si y solo si $n <= x < n + 1$
] <lem:floor-range>

#corollary[
  Si $b > 0$, $a - b < b floor(a / b) <= a$.
] <col:mult-floor-quot>
#proof[
  Si $floor(a / b) = x$ entonces de acuerdo con el @lem:floor-range:

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
  Si $ceil(a / b) = x$ entonces de acuerdo con el @lem:ceil-range:

  $ x - 1 < a / b <= x $
  $ b (x - 1)  < a <= b x $
  $ b (ceil(a / b) - 1) < a <= b ceil(a / b) $
  $ b ceil(a / b) - b < a => b ceil(a / b) < a + b $
  $ a <= b ceil(a / b) < a + b $
]

/* CONVERSION THEOREM */

#theorem[
  Dados $x in bb(X)_(b,f)$ y $x' = "conv"_(f') (x) in bb(Q)$:
  si $f >= f'$ entonces $x = x'$; si $f < f'$ y luego $k >= 0$ se cumple que
  $0 <= x - x' < 2^(f')$ y si $f < f'$ y luego $k < 0$ se cumple que
  $0 <= x' - x < 2^f'$.
] <thm:conv-error>

#proof[
  Dados $x = 2^f k$ y $x' = 2^(f') k'$, donde $x' = "conv"_(f') (x)$.
  El valor del suelo o techo depende del valor de la diferencia $f - f'$.

- Si $f = f'$ y $k >= 0$, entonces $k' = floor(2^(f - f') k) = floor(k) = k$,
  porque $k in bb(I)_b$. Luego $x = x'$ y $x - x' = 0$.

- Si $f = f'$ y $k < 0$, entonces $k' = ceil(2^(f - f') k) = ceil(k) = k$,
  porque $k in bb(I)_b$. Luego $x = x'$ y $x - x' = 0$.

- Si $f > f'$ y $k >= 0$, entonces $k' = floor(2^(f - f') k) = 2^(f - f') k$.
  Luego $x' = 2^(f') k' = 2^(f') 2^(f - f') k = 2^f k = x$, por lo que
  $x - x' = 0$.

- Si $f > f'$ y $k < 0$, entonces $k' = ceil(2^(f - f') k) = 2^(f - f') k$.
  Luego (como para $f > f', k >= 0$) $x - x' = 0$.

- Si $f < f'$ y $k >= 0$, como $2^(f') k' = 2^(f') floor((2^f k) / 2^(f'))$, de
  acuerdo con el @col:mult-floor-quot:

    $ 2^f k - 2^(f') < 2^(f') floor((2^f k) / 2^(f')) <= 2^f k $
    $ x - 2^(f') < x' <= x $
    $ -2^(f') < x' - x <= 0 $
    $ 0 <= x - x' < 2^f' $

- Si $f < f'$ y $k < 0$, como $2^(f') k' = 2^(f') ceil((2^f k) / 2^(f'))$, de
  acuerdo con el @col:mult-ceil-quot:

    $ 2^f k <= 2^(f') ceil((2^f k) / 2^(f')) < 2^f k + 2^(f') $
    $ x <= x' < x + 2^(f') $
    $ 0 <= x' - x < 2^(f') $
]

Del @thm:conv-error se obtienen propiedades muy relevantes. En primer lugar,
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

  Para $m'$, si $f >= f'$ de acuerdo con el @thm:conv-error, $m' = m$, entonces
  solo se puede representar $m'$ en $bb(Z)_(b',f')$ si
  $f - f' + b - 1 <= b' - 1$ pues el valor mínimo de dicho conjunto es
  $-2^(b' - 1)$. Se necesita que si $f >= f' => b' >= b + (f - f')$.

  Si por el contrario $f < f'$, se deben encontrar los valores que satisfagan
  que $m' >= -2^(b' - 1)$. Por el @cor:conv-order sabemos que $0 >= m' >= m$,
  luego el valor de la conversión no es positivo (así que el límite superior
  se puede ignorar) y no es menor que el valor original $m$. Luego la
  restricción debe ser que $m <= m' => 2^(b - 1) 2^f <= 2^(b' - 1) 2^(f')$.
  Luego $b - 1 + f <= b' - 1 + f' => b' >= b + (f - f')$.

  Para $M'$, si $f >= f'$, de acuerdo con el @thm:conv-error, $M' = M$, entonces
  solo se puede representar $M'$ en $bb(Z)_(b',f')$ si 

  $ M = &floor(2^(f-f') (2^(b - 1) - delta_(b,f))) 2^(f')   \
      = &floor(2^(f-f'+b-1) - 2^(f')) 2^(f')                \
      = & 2^(f+b-1) - 2^(f')                                \
      <=& (2^(b' - 1) - 1) 2^(f')                           \
       =& 2^(f' + b' - 1) - 2^(f')
  $
  $ => f + b - 1 <= f' + b' - 1 => b' >= b + (f - f') $

  Finalmente si $f < f'$, de acuerdo con el @cor:conv-order $0 <= M' <= M$.
  Trabajamos con valores no negativos. Y como se vio arriba, la condición más
  fuerte de que $b' >= b + (f - f')$ se sigue manteniendo.
]

Para simplificar el problema, se va a utilizar valores de entrada de 32 _bits_
y la salida también será de 32 _bits_. Así que $b_e = b_s = 32$. Para
solucionar el problema que existía en la resta se debe convertir primero los
valores al tipo de retorno. De acuerdo con el @thm:conv-bit-cond
$b_s >= b_e + (f_e - f_s) => 32 >= 32 + (f_e - f_s) => f_s >= f_e$ para que
la conversión se pueda realizar.

Para que la resta se pueda realizar además es necesario que $f_e < f_s$.
Pues el valor se maximiza cuando $"máx"_v = (2^(31) - 1) 2^f_e$ y
$"mín"_v = -2^(31) 2^f_e$ entonces $"máx"_v - "mín"_v = 2^(32) - 1$. Nótese
que se pierde información como consecuencia del @thm:conv-error, se minimiza
la pérdida de información cuando más pequeño sea $f_s$. Así que $f_e + 1 = f_s$.

==== Precondiciones

- $m > 0$, el vector debe tener al menos un elemento.
- $m < "máx"{bb(I)_b}$, la longitud del vector debe ser menor que el máximo del
  número entero que se utilice para indexar.
- $f_s = f_e + 1$, para poder operar y minimizar la pérdida de información.

==== Postcondiciones

- El resultado es no negativo, pues $"máx"_v >= "mín"_v$.

=== Acumulación
Varios algoritmos tienen algún paso que consiste en suma una secuencia de
elementos. El algoritmo es similar al algoritmo `max-distance`
(@algorithm-max-distance) en que es una reducción. Dado un vector $v in bb(R)^m$
el algoritmo acumuación computa: $sum_(i=1)^m v(i)$, de acuerdo con el
siguiente diagrama de flujo:

#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((0,0), name: <A1>, [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((0,v-sep), align(center)[*2*:
      $"res" <- v(1)$ \
      $i <- 2$
      ], shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*2), name: <loop>, align(center)[*3*: ¿$i <= m$?], shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((0,v-sep*3), align(center)[
      *4*:
      $"res" <- "res" + v(i)$],
      shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*4), name: <endloop>, align(center)[*5*: $i <- i + 1$])
    edge("l,u,u,r", "-|>")
    node((1, v-sep*2), name: <end>, align(center)[
      *6*: Fin],
      shape: shapes.pill)
    edge(<loop.east>, <end.west>, "-|>", [No])
  }),
  caption: [Algoritmo «acumulación»]
)

==== Problemas <algorithm-accumulation-problems>
Se siguen viendo similaridades con `max-distance`, y se ve que comparte sus
dos primeros problemas @algorithm-max-distance-problems:

- En el paso *2*, $v(1)$ falla si $m = 0$.
- En el paso *5*, $i + 1$ falla si $i in bb(I)_b and i + 1 in.not bb(I)_b$,
  por ejemplo si $i in bb(I)_b$ y $m = 2^(b-1) - 1$
- Además en el paso *4*, se ve un problema similar al que tenía `max-distance`
  en su paso número *6*. Y es que la suma puede desbordar en punto fijo.

==== Análisis de punto fijo
Existen varias alternativas para solucionar este problema, la más sencilla es
sin duda utilizar un tipo de punto fijo más grande para el resultado,
consecuencia del @thm:accumulation.

#lemma[
  Si $a >= 2$ y $b >= 2$, entonces $a b >= a + b$
] <lem:product-greater-than-sum>

#theorem[
  Dados $i in bb(I)_B$ y $v(i) in bb(X)_(b,f) forall i = 1, 2, ..., m$,
  entonces el resultado debe estar en $bb(X)_(b+B-1,f)$.
] <thm:accumulation>

#proof[
  Si $i in bb(I)_B$ y $v(i) in bb(X)_(b,f) forall i = 1, 2, ..., m$. Sea la
  función de acumulación $g(v,m)=sum_(i=1)^(m) v(i)$, como $bb(X)_(b,f)$ y
  $bb(I)_B$ son finitos, el codominio de $g$ también es finito y tiene un valor
  máximo y mínimo que pertenece a $bb(X)_(b+B-1,f)$

  La función se maximiza cuando $v(i) = (2^(b-1) - 1) 2^f, forall i = 1, 2,
  ..., m$, el resultado entonces sería:

    $ "máx"{sum_(i=1)^m v(i)} = $
    $ = sum_(i=1)^(2^(B-1)-1) (2^(b-1) - 1) 2^f $
    $ = 2^f (2^(B-1) - 1) (2^(b-1) - 1) $
    $ = 2^f [2^(B+b-2) - 2^(B-1) - 2^(b-1) + 1] $

  Es necesario ver que:

    $ (2^(b+B-2) + 1) 2^f > (2^(B-1) + 2^(b-1)) 2^f <=> 2^(b+B-2) + 1 > 2^(B-1) + 2^(b-1) $

  Hay que tener en cuenta que $b, B in bb(N)^+$. Los casos son los siguientes:
  Cuando $b >= 2 and B >= 2$, como $2^(B-1) >= 2$ y $2^(b-1) >= 2$, de acuerdo
  con el @lem:product-greater-than-sum:

    $ 2^(b+B-2) >= 2^(B-1) 2^(b-1) => 2^(b+B-2) + 1 > 2^(B-1) 2^(b-1) $

  Por el contrario si $b = 1$, entonces $2^(b+B-2) + 1 = 2^(B-1) >= 1 +
  2^(B-1)$. Igualmente con $B = 1$, entonces $2^(b+B-2) + 1 = 2^(b-1) >=
  1+2^(b-1)$.

  Entonces:

    $ 0 <= (2^(b+B-2) + 1) 2^f - [2^(b-1) + 2^(B-1)] 2^f <= (2^(B+b-2) - 1) 2^f $
    $ "máx"{sum_(i=1)^m v(i)} = (2^(b+B-2) + 1) 2^f - [2^(b-1) + 2^(B-1)] 2^f in bb(X)_(B+b-1,f) $

  La función se minimiza cuando $v(i) = -2^(b-1) 2^f, forall i = 1, 2, ..., m$,
  el resultado entonces sería:

    $ "mín"{sum_(i=1)^m v(i)} = $
    $ = 2^f sum_(i=1)^(2^(B-1)-1) -2^(b-1) $
    $ = 2^f (2^(B-1)-1) (-2^(b-1)) $
    $ = -2^(B+b-2) 2^f + 2^(b-1) 2^f $
    $ [ 2^(b-1) 2^f <= 2^(B+b-2) 2^f <=> b - 1 <= B + b - 2 <=> B >= 1 ] $
    $ -2^(B+b-2) 2^f <= -2^(B+b-2) 2^f + 2^(b-1) 2^f <= 0 $
    $ "mín"{sum_(i=1)^m v(i)} = -2^(B+b-2) 2^f + 2^(b-1) 2^f in bb(X)_(B+b-1,f) $
]

Esto viene a decir que si se utiliza un punto fijo de $b$ bits y el tamaño
máximo se puede almacenar en una variable de tipo entero con signo de $B$ bits:
se necesitaría que el acumulador tuviera $b + B - 1$ bits para almacenar el
resultado, con la misma $f$ que el punto fijo de entrada.

==== SPARK <subsubsec:acc-spark>
A la hora de explicar esto a SPARK hay que utilizar una técnica que utiliza
un vector con las sumas parciales del vector de manera que
$v'(i) = sum_(j=1)^i v(i), i = 1, 2, ..., m$ @SPARKpartialSum.
Además, una condición adicional que se ha visto útil, es decir que los valores
de $v'(i)$ están acotados. Es decir,

  $ v(i) in [a, b], forall i = 1, 2, ..., m =>
   v'(i) = sum_(j=1)^i v(i) in [a i, b i], forall i = 1, 2, ..., m $

En SPARK se utilizan lo que se llaman funciones fantasma (_Ghost_), que son
subrutinas que no generan código y que solamente son utilizadas por el probador
de teoremas interno. En este caso se define la función `Generic_Accumulation`
se declararía como dice el @lst:generic-accumulation-spec.

#code(
  caption: [Especificación de la función fantasma `Generic_Accumulation`],
  tag: "lst:generic-accumulation-spec"
)[```adb
generic
   type Fixed_Type is delta <>;
   type Index_Type is range <>;
   type Array_Type is array (Index_Type range <>) of Fixed_Type;
   First : in Fixed_Type;
   Last  : in Fixed_Type;
function Generic_Accumulation (
   Item : in Array_Type)
   return Array_Type with
   Ghost    => True,
   Global   => null,
   Pre      => Item'Length > 0
      and then (for all X of Item => X in First .. Last),
   Post     => (
      declare  Result renames Generic_Accumulation'Result;
      begin    Result'First = Item'First
      and then Result'Length = Item'Length
      and then Result (Item'First) = Item (Item'First)
      and then (for all I in Item'First + 1 .. Item'Last =>
                  Result (I - 1) in Positive (I - Item'First) * First
                                 .. Positive (I - Item'First) * Last
                  and then Result (I) = Result (I - 1) + Item (I))
      and then Result (Item'Last) in Item'Length * First
                                  .. Item'Length * Last);
```]

La línea 10 del @lst:generic-accumulation-spec
(@lst:generic-accumulation-spec:10) dice que es una función fantasma, que no
genere código. En @lst:generic-accumulation-spec:11 mencionamos que es una
función que no modifica el estado global (en SPARK está prohibido si es una
función, no si es un procedimiento). A continuación hay dos precondiciones:

- La longitud debe ser estrictamente positiva
- Todos los elementos deben estar acotados en el mismo rango: $v(i) in ["first", "last"] forall v(i)$.

Finalmente la postcondición dice que:

- La longitud del resultado es igual a la del parámetro. Y además que comienzan
  en el mismo índice (en Ada o SPARK se puede indexar a partir de cualquier
  valor).
- Todos los elementos están acotados como se veía al principio en la
  @subsubsec:acc-spark.

Y se implementaría como se muestra en @lst:generic-accumulation-body.

#code(
  caption: [Implementación de la función fantasma `Generic_Accumulation`],
  tag: "lst:generic-accumulation-body"
)[```adb
function Generic_Accumulation (
   Item : in Array_Type)
   return Array_Type is
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
end Generic_Accumulation;
```]

Es preciso mencionar que es necesario especificar los predicados invariantes en
el bucle (`Loop_Invariant`), es decir qué propiedades no cambian de una
iteración a la siguiente. En este caso estamos diciendo que primero todos
los valores hasta el índice actual está acotados
(@lst:generic-accumulation-body:11); que su valor es la suma del anterior
resultado más el valor del vector en el índice actual
(@lst:generic-accumulation-body:13); y que, finalmente, esa suma sigue estando
acotada en un superconjunto (@lst:generic-accumulation-body:14). SPARK, no sabe
si se están modificando los elementos anteriores o no.

La demostración en SPARK es similar a la inducción matemática: sea $i in
bb(I)_B$ la variable de iteración, $v in bb(Z)_(b,f)^m$ el vector de entrada y
$v' in bb(Z)_(b+B,f)^m$ el vector de salida, de modo que
$v(i) in [a, b], forall i = 1, 2, ..., m$.


1. *Caso base*: $v(1) = v'(1)$ y $v(1) in [a, b]$
2. *Hipótesis inductiva*: Para $v'(k), k > 1$ suponemos $v'(k) in [k a, k b]$.
3. *Tesis inductiva*: Para $v'(k+1)$, si $v'(k) in [k a, k b]$, como
   $v'(k+1) = v'(k) + v(k)$ y como $v(k) in [a, b]$, entonces
   $v'(k) + v(k) in [a (k+1), b (k+1)]$, por lo que $v'(k+1) in [a (k+1), b(k+1)]$

La función fantasma no computa la acumulación como tal, debe ser otra función
la que haga uso de ella para definir dichos rangos. Suponiendo que hay un
tipo de punto fijo `Input_Type` ($bb(X)_(b,f)$), un tipo para indexar
`Index_Type` ($bb(I)_B$), un tipo de resultado `Result_Type`
($bb(X)_(b+B-1,f)$) y un tipo vector de elementos de tipo `Input_Type` que
indexa con `Index_Type >= 1` (véase el @lst:generic-accumulation-example).

#code(
  caption: [Ejemplo de uso de la función fantasma `Generic_Accumulation`],
  tag: "lst:generic-accumulation-example"
)[```adb
function Accumlate (Item : in Input_Type_Array) return Result_Type is
   subtype Constrained_Result is Result_Type
      range Result_Type (Input_Type'First) .. Result_Type (Input_Type'Last)
   type Result_Array is array (Index_Type range <>) of Result_Type;
   function Acc_Sum is
      new Lemmas.Generic_Accumulation (
      Fixed_Type => Result_Type,
      Index_Type => Index_Type,
      Array_Type => Result_Array,
      First      => Constrained_Result'First,
      Last       => Constrained_Result'Last);
   Mapped : constant Result_Array (Item'Range) :=
      [for I in Item'Range => Constrained_Result (Item (I))] with
      Ghost => True;
   Result : Result_Type := 0.0;
begin
   for Index in Item'Range loop
      Result := Result + Constrained_Result (Item (Index));
      pragma Loop_Invariant (Result = Acc_Sum (Mapped) (Index));
   end loop;
   return Result;
end Accumulate;
```]

Se debe «instanciar» la función que `Generic_Accumulation`, es decir es
genérica (sirve para distintos tipos de tipos) y por lo tanto está
parametrizada (@lst:generic-accumulation-example:5). Se define una constante
fantasma con el vector de entrada, pero esta vez contiene los valores con el
tipo del resultado. Finalmente en @lst:generic-accumulation-example:19 dice
que el resultado `Result` en la i-ésima posición tiene el mismo valor que el
valor en la i-ésima posición de `Generic_Accumulation`.

=== Media
El algoritmo de la media es básicamente el de la acumulación, pero dividiendo
entre el número de elementos:

  $ mu (v,m) = sum_(i=1)^m v(i)/m $

O en diagrama de flujo de la @fig:flujo-media.

#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((0,0), name: <A1>, [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((0,v-sep), align(center)[*2*:
      $"res" <- v(1)$ \
      $i <- 2$
      ], shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*2), name: <loop>, align(center)[*3*: ¿$i <= m$?], shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((0,v-sep*3), align(center)[
      *4*:
      $"res" <- "res" + v(i)$],
      shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*4), name: <endloop>, align(center)[*5*: $i <- i + 1$])
    edge("l,u,u,r", "-|>")
    node((1, v-sep*2), name: <end>, align(center)[
      *6*: $"res"<-"res"slash m$],
      shape: shapes.rect)
    edge(<loop.east>, <end.west>, "-|>", [No])
    edge("-|>")
    node((1, v-sep*3), align(center)[*7*: Fin], shape: shapes.pill)
  }),
  caption: [Algoritmo de la media]
) <fig:flujo-media>

==== Problemas <algorithm-mean-problems>

Se siguen manteniendo los problemas de la acumulación
como se puede ver en la @algorithm-accumulation-problems. Pero en el caso de
punto fijo el resultado, si $v(i) in bb(X)_(b,f), forall i = 1, 2, ..., m$,
también pertenece al mismo conjunto que los valores de entrada $bb(X)_(b,f)$,
según la demostración del @lem:product-greater-than-sum (esta vez con $m$ en
vez del valor máximo):

  $ "máx"_(m=m){sum_(i=1)^m v(i)} = m (2^(b-1) - 1) 2^f $
  $ "mín"_(m=m){sum_(i=1)^m v(i)} = -m 2^(b-1) 2^f $

Y por supuesto si se divide entre $m$ cualquiera de los dos para hacer la media
se obtiene el máximo y mínimo respectivamente del conjunto de los valores del
vector:

  $ ("máx"_(m=m){sum_(i=1)^m v(i)}) / m = (2^(b-1) - 1) 2^f $
  $ ("mín"_(m=m){sum_(i=1)^m v(i)}) / m = 2^(b-1) 2^f $

Sin embargo, nótese que esta condición no es cierta al trabajar con punto
flotante por problemas de precisión. Es bien conocido que trabajando con un
computado de punto flotante de 64 bits ($bb(F)_64$), por ejemplo, la suma
$0.1 + 0.2$ no da exactamente $0.3$, da $0.30000000000000004$; porque ni $0.1$
ni $0.2$ se pueden respresentar en binario con un número finito de dígitos
binarios: $0.1_(10) = 0.00overline(0011)_2$ y $0.2_(10) = 0.0overline(0011)$.
Luego no se puede hacer dicha suposición cuando se trabaja en cualquier
$bb(F)_b$.

=== Subconjunto uniforme de $bb(X)_(b,1-b)$ <uniform>
Para los siguientes algoritmos resulta bastante trabajar con un punto fijo
con un exponente $f$ del coeficiente arbitrario, pues muchos de ellos necesitan
utilizar multiplicaciones. Además, dado que hay un valor negativo más que
valores positivos también añade complicaciones. Así que se define $bb(U)_b$
como el subconjunto uniforme de $bb(X)_(b,1-b)$ o como conjunto uniforme de $b$
bits:

  $ bb(U)_b = bb(X)_(b,1-b) without {-1} $

#lemma[
  $abs(x) < 1, forall x in bb(U)_b, forall b > 1$
] <lem:uniform-less-than-one>

#theorem[
  Dados $x = p 2^(1-b), x in bb(U)_b$ e $y = q 2^f, y in bb(U)_(b')$, su
  producto $x y = p 2^(1-b) q 2^(1-b')$, también es uniforme
  $x y in bb(U)_(b+b'-1)$.
] <thm:uniform-product>

#proof[
  Dados $x = p 2^(1-b), x in bb(U)_b$ e $y = q 2^f, q in bb(U)_(b')$. Por
  definición $p in bb(I)_b without {-2^(b-1)}$ y
  $q in bb(I)_b' without {-2^(b'-1)}$. O que es lo mismo $abs(p) < 2^(b-1)$ y
  $abs(q) < 2^(b'-1)$. Eso quiere decir que:

    $ abs(p q) <= (2^(b'-1) - 1) (2^(b-1) - 1) < 2^(b-1) 2^(b'-1) = 2^(b+b'-2) $

  Luego $p q in bb(I)_(b+b'-1)$. Por definición $bb(X)_(b+b'-1,2-b-b')$ es el
  conjunto de la forma: ${ k 2^(2-b-b') : k in bb(I)_(b+b'-1) }$. Ya hemos
  visto que $p q in bb(I)_(b+b'-1)$, luego

    $ x y = p q 2^(2-b-b') in bb(X)_(b+b'-1,2-b-b') $

  La única diferencia entre el conjunto $bb(X)_(b+b'-1, 2-b-b')$ y el conjunto
  $bb(U)_(b+b'-1)$ es que $-1 in bb(X)_(b+b'-1,2-b-b')$ (cuando tiene la forma
  $-2^(b+b'- 1 - 1) 2^(2-b-b') = -1$) y que $-1 in.not bb(U)_(b+b'-1)$.

  Suponiendo que existiera un $x in bb(U)_b$ y un $y in bb(U)_(b')$ de forma
  que $x y = -1$, implicaría que $exists p in bb(I)_b$, $exists q in bb(I)_q$
  de forma que $p q = -2^(b+b'-2)$ para que $x y = p q 2^(2-b-b')
  = -2^(b+b'-2) 2^(2-b-b') = -1$. Sin embargo, $exists.not p in bb(I)_b$,
  $exists.not q in bb(I)_(b')$ de forma que $p q = -2^(b+b'-2)$ porque
  contradice que $abs(p q) < 2^(b+b'-2)$. Luego
  $exists.not x in bb(U)_b$, $exists.not y in bb(U)_(b')$ tales que $x y = -1$.
  Y por ende:

    $ x y in bb(X)_(b+b'-1,2-b-b') without {-1} $
    $ x y in bb(U)_(b+b'-1) $
]

#theorem(title: [Producto de enteros de computador])[
  Dados dos enteros $x in bb(I)_b$ e $y in bb(I)_(b')$. Su producto es un
  entero de $b + b' - 1$ bits: $x y in bb(I)_(b+b'-1)$.
] <thm:integer-product>

#proof[
  // Dados $x in bb(I)_b$ e $y in bb(I)_b'$. $x in [-2^(b-1)
]

#theorem(title: [Producto de punto fijo])[
  Dados $x = p 2^f, x in bb(X)_(b,f)$ e $y = q 2^(f'), y in bb(X)_(b',f')$, su
  producto $x y = p q 2^(f+f')$ es otro punto fijo: $x y in bb(X)_(b+b'-1,f+f')$.
]

#proof[
  Dados dos conjuntos de punto fijo $bb(X)_(b,f)$ y $bb(X)_(b',f')$. Por
  definición:

    $ bb(X)_(b,f) = { k 2^f : k in bb(I)_b } $
    $ bb(X)_(b',f') = { k 2^(f') : k in bb(I)_(b') } $

  Sea el conjunto $P$ el conjunto que contiene todos los posibles productos
  entre los elementos del primer conjunto por los elementos del segundo:

    $ P &= { x y : x in bb(X)_(b,f), y in bb(X)_(b',f') } \
        &= { p 2^f y : p in bb(I)_b, y in bb(X)_(b',f') } \
        &= { p 2^f q 2^(f') : p in bb(I)_b, q in bb(I)_(b') } \
        &= { p q 2^(f+f') : p in bb(I)_b, q in bb(I)_(b') } \
        &subset.eq { k 2^(f+f') : k in bb(I)_(b+b'-1) } #text([\[$k := p q$, @thm:integer-product\]]) \
        &= bb(X)_(b+b'-1,f+f')
    $

  Como $x y in P subset.eq bb(X)_(b+b'-1,f+f')$, se puede afirmar que
  $x y in bb(X)_(b+b'-1,f+f')$.
]

#definition(title: [Conversión del producto])[
  Dados $x in bb(X)_(b,f)$ e $y in bb(X)_(b',f')$ se denota como:

    $ x *_(b'',f'') y = "conv"_(b'',f'') (x y) $

  Y si $x in bb(U)_(b)$ e $y in bb(X)_(b')$, también se denota como:

    $ x *_(b'') y = "conv"_(b'',1-b'') (x y) $

  A la conversión del producto de ambos en otro punto fijo.
] 

#theorem(title: [Conversión del producto de uniformes es uniforme])[
  Si $x in bb(U)_b$ e $y in bb(U)_(b')$. Entonces $x *_(b'') y in bb(U)_(b'')$.
] <thm:uniform-conv-product>

#proof[
  Dados $x in bb(U)_b$ e $y in bb(U)_(b')$. Sea su producto $z = x y$, que
  según el @thm:uniform-product se sabe que $z in bb(U)_(b+b'-1)$. Finalmente
  aplicamos el @thm:conv-bit-cond con:

  - $b := b+b'-1$
  - $f := 1 - (b+b'-1) = 2-b-b'$
  - $b' := b''$
  - $f' := 1 - b''$

  Si es cierto que:

    $ b'' >=& (b + b' - 1) + [(2 - b - b') - (1 - b'')] \
           =& b + b' - 1 + 2 - b - b' - 1 + b'' \
           =& b''
    $

  (que siempre es cierto), significaba que
  $z in bb(X)_(b+b'-1,2-b-b')$, luego su conversión
  $"conv"_(b'',1-b'') (z) in bb(X)_(b'',1-b'')$.
  Es decir $x *_(b'') y in bb(X)_(b'', 1-b'')$. Lo único que es necesario
  determinar para terminar de demostrar este teorema es que
  $exists.not x in bb(U)_b, exists.not y in bb(U)_(b'), x *_(b'') y = -1$.

  Supongamos que $exists x = p 2^(1-b) in bb(U)_(b)$ y
  $exists y = q 2^(1-b') in bb(U)_(b')$, para los que $z = x *_(b'') y = -1$.
  Según la @def:fixed-point-conversion, $z = k 2^(1-b'')$, donde $k$ es:

    $ k = cases(floor(2^((2 - b - b') - (1 - b'')) p q)", " & "si" p q >= 0,
                ceil(2^((2 - b - b') - (1 - b'')) p q)", "  & "si" p q < 0) $

  Estamos intentando buscar una $k$ para que $z = -1 = k 2^(1-b'') =>
  k = -2^(b''-1) in bb(I)_(b'')$. Como $k < 0 => p q < 0$.

    $ k = ceil(2^((2 - b - b') - (1 - b'')) p q) = -2^(b''-1) $

  Como $abs(p) < 2^(b-1)$ y $abs(q) < 2^(b'-1)$, $abs(p q) < 2^(b+b'-2)$. Y
  como $p q < 0$ entonces $0 > p q > -2^(b+b'-2)$.-

    $ 0 &&> p q >&& -2^(b+b'-2) \
      0 &&> 2^((2-b-b') - (1-b'')) p q >&& 2^((2-b-b') - (1-b'')) (-2^(b+b'-2)) \
      0 &&> 2^((2-b-b') - (1-b'')) p q >&& -2^(b''-1)
    $

  Sabemos que $ceil(-2^(b''-1)) = -2^(b''-1)$ porque $b'' > 1, b'' in bb(N)$.
  Según el @lem:ceil-range:

    $ ceil(-2^(b''-1)) = -2^(b''-1) <=> -2^(b''-1) - 1 < -2^(b''-1) <=
    -2^(b''-1) $

  Como $-2^(b''-1) < 2^((2-b-b') - (b''-1)) p q$, se concluye que
  $ceil(2^((2-b-b') - (b''-1)) p q) > -2^(b''-1)$. Luego 
  $exists.not x in bb(U)_b, exists.not y in bb(U)_(b'), x *_(b'') y = -1$, y
  por tanto: $x *_(b'') y in bb(U)_(b'')$.
]

=== Uniformización de un vector $bb(X)_(b,f)^n, n in bb(N)^+$
Antes de definir en qué consiste uniformizar un vector. Es necesario definir
la división para el punto fijo. La división de dos números racionales:
$x = a/b$ e $y = p/q$, $x, y in bb(Q)$ y $a,b,p,q in bb(Q)$, es otro número
racional:

  $ x/y = (a/b)/(p/q) = (a q)/(b p) in bb(Q) $

Trabajando con números en punto fijo sigue siendo similar, dados dos números
en punto fijo $x = p 2^f, x in bb(X)_(b,f)$ e $y = q 2^(f') in bb(X)_(b',f')$.
Su cociente también es un número en punto fijo:

  $ x / y = (p 2^f) / (q 2^(f')) = p/q 2^(f-f') $

Sigue pareciendo un número en punto fijo multiplicado por $2^(f-f')$, la única
diferencia es que $p/q in.not bb(Z), forall p, q$, lo cual complica bastante
tratarlo como un punto fijo.

#definition(title: [División de punto fijo])[
  Dados dos números en punto fijo $x = p 2^f in bb(X)_(b,f)$ e
  $y = q 2^(f') in bb(X)_(b,f')$ con *$y != 0$*, se define la división de punto
  fijo de $x$ entre $y$ y se denota como $x div y$ a:

    $ x div y = cases(floor(p/q) 2^(f-f') & ", si " p/q >= 0,
                      ceil(p/q) 2^(f-f') & ", si " p/q < 0) in bb(Q) $
]<def:fixed-division>

#theorem[
  Dados dos números en punto fijo $x = p 2^f in bb(X)_(b,f)$ e
  $y = q 2^(f') in bb(X)_(b,f')$, entonces:
  
  $ x div y in cases(bb(X)_(b,(f-f')) & ", si " x != (2^(b-1) - 1)delta_(b,f)
                                           " e " y != -delta_(b',f'),
                     bb(X)_(b+1,(f-f')) & ", si no") $
] <thm:fixed-division-set>

#proof[
  Sean $x = p 2^f in bb(X)_(b,f)$ e $y = q 2^(f') in bb(X)_(b',f')$ con
  $y != 0$, por definición $p in bb(I)_b$ y $q in bb(I)_(b')$.
  Sea $z = x div y$.

  - Si $p/q$ = 0, entonces $x div y = floor(p/q) 2^(f-f') = 0$.
  - Si $p/q > 0$, entonces $x div y = floor(p/q) 2^(f-f')$. Es preciso ver que
    $abs(a/b) < abs(a)/(abs(b)+1), forall abs(b) > 0, forall a$, es decir,
    cuando aumenta el denominador en valor absoluto, el valor absoluto del
    cociente es menor. Lo que nos indica que la función $abs(a/b)$ se maximiza
    cuando $abs(b) = 1$ y decrece monótonamente a medida de que $abs(b)$
    aumenta.

    - Si $0 < q <= 2^(b'-1) - 1$ implica que $0 < p <= 2^(b-1) - 1$. Luego,
      cuando $q = 1$, el cociente $floor(p/q) = floor(p/1) = p$, lo que da el
      primer límite superior $2^(b-1)-1$. Pues cuando $q$ crece, el cociente
      decrece monótonamente.

    - Si $-2^(b'-1) <= q < 0$ implica que $-2^(b-1) <= p < 0$. Cuando $q = -1$,
      el cociente $floor(p/q) = floor(-p) = -p$ , es decir, igual que el caso
      anterior obtenemos un nuevo límite superior $2^(b-1)$.  Sin embargo, si
      $q != -1 and p != -2^(b-1)$, el límite superior es
      distinto pues:

      - Si $q = -2$ y $p = -2^(b-1)$, $floor(p/q) = floor((-2^(b-1))/(-2)) =
        floor(2^(b-2)) = 2^(b-2) <= 2^(b-1)-1$. Que maximiza la función.
      - Si $q = -1$ y $p = -2^(b-1)+1$, $floor(p/q) = 2^(b-1) - 1 <= 2^(b-1)
        -1 $. Que maximiza la función.

      Luego si $q = -1$ y $p = -2^(b-1)$ el límite superior es $2^(b-1)$, si
      no, el límite superior es $2^(b-1)-1$.

    Por lo que se concluye que si $q = -1$ y $p = -2^(b-1)$ entonces
    $x div y <= 2^(b-1)$, si no $x div y <= 2^(b-1) - 1$.

  - Si $p/q < 0$, entonces $x div y = floor(p / q) 2^(f-f')$, se sigue
    cumpliendo que $abs(a/b) < abs(a)/(abs(b)+1), forall abs(b) > 0, forall a$.

    - Si $-2^(b'-1) <= q < 0$ implica que $0 < p <= 2^(b-1) - 1$. La función
      $p/q$ se minimiza cuando $q = -1$ y $p = 2^(b-1)-1$, porque
      $ceil(p/q)=ceil((2^(b-1)-1)/(-1)) = -2^(b-1)+1$, que es el primer límite
      inferior.

    - Si $0 < q <= 2^(b'-1) - 1$ implica que $-2^(b-1) <= p < 0$. La función
      $p/q$ se minimiza cuando $q = 1$ y $p = -2^(b-1)$, porque
      $ceil(p/q)=ceil((-2^(b-1))/1) = -2^(b-1)$, que nos da el segundo límite
      inferior.

  De esto se concluye que si $p != -2^(b-1)$ y $q != -1$, entonces
  $floor(p / q) in bb(I)_b, p/q>=0$ y $ceil(p / q) in bb(I)_b, p/q<0$.
  Y si $p = -2^(b-1)$ y $q = -1$, entonces
  $floor(p/q) in bb(I)_b union {2^(b-1)} subset.eq bb(I)_(b+1), p/q >= 0$ y
  que $ceil(p / q) in bb(I)_b, p/q<0$.

  Luego $x div y in bb(X)_(b,(f-f'))$ si $x != (2^(b-1) - 1)delta_(b,f) $ e
  $y != -delta_(b',f')$, si no $x div y in bb(X)_(b+1,(f-f'))$.
]

Como consecuencia de la @def:fixed-division siempre se pierde información
cuando se divide por el redondeo dado por la función techo y la función suelo.
Una forma de minimizar dicho impacto es multiplicar primero el numerador y
luego dividir.

La idea es poder trabajar con valores escalados en un conjunto uniforme
$bb(U)_b$ sin perder precisión, a este proceso le vamos a denominar
uniformización. En primer lugar, la de un valor:

#definition(title: [Uniformización de un valor])[
  La uniformización de un vector convierte un valor en punto fijo
  $x = p 2^f in bb(X)_(b,f) without {-2^(b-1) 2^f}$ a otro conjunto uniforme
  $bb(U)_b$ sin perder precisión, es decir el numerador es el mismo, el
  denominador cambia para que sea uniforme. Se denota como $u = "unif"(x)$ y se
  define como:

    $ "unif"_(b,f): bb(X)_(b,f) -> bb(U)_b $
    $ "unif"_(b,f) (x) = p 2^(1-b) = x / (2^(f+b-1)) $
]

#definition(title: [Uniformización de un vector])[
  La uniformización de un vector convierte un vector $v in bb(X)_(b,f)^n$ a
  otro vector escalado $v' in bb(U)_b^n$ y se expande la definición de la
  función $"unif"$ para vectores:

  $ "unif"_(b,f): bb(X)_(b,f)^n -> bb(U)_b^n $
  $ v' = "unif"_(b,f) (v) $
  $ v'(i) = "unif"_(b,f) (v(i)), forall i = 1, 2, ... n $
] <def:unif-vector>

#theorem(title: [Uniformización es biyectiva])[
  La uniformización es inyectiva porque. Por contradicción, supongamos que no
  es inyectiva y que
  $exists x = p 2^f in bb(X)_(b,f) without {-2^(b-1) 2^f},
   exists y = q 2^f in bb(X)_(b,f) without {-2^(b-1) 2^f}$ con
  $x != y$, tales que:

  $   && "unif"_(b,f) (x)      &&=& "unif"_(b,f) (y)      \
   => && "unif"_(b,f) (p 2^f)  &&=& "unif"_(b,f) (q 2^f)  \
   => && p 2^(1-b)             &&=& q 2^(1-b)             \
   => && p                     &&=& q $

  Llegamos a una contradicción, así que es *inyectiva*.

  Finalmente es suprayectiva porque el codominio es el conjunto:

  $ =& {"unif"_(b,f) (x) : x in bb(X)_(b,f) without {-2^(b-1) 2^f}} & \
    =& {"unif"_(b,f) (p 2^f) : x in bb(I)_(b) without {-2^(b-1)}} &
      #text([[por definición]]) \
    =& {p 2^(1-b) : x in bb(I)_(b) without {-2^(b-1)}} & \
    =& {p 2^(1-b) : x in bb(I)_(b)} without {-2^(b-1) 2^(1-b)} & \
    =& {p 2^(1-b) : x in bb(I)_(b)} without {-1} & \
    =& bb(U)_b & #text([[por definición]])
  $

  Que cubre todo el conjunto $bb(U)_b$, así que la función es *suprayectiva*.

  Como la función es a la vez inyectiva y suprayectiva, se dice que la función
  es *biyectiva*.

]<thm:unif-biyectiva>

#corollary[
  Como la función $"unif"_(b,f)$ es biyectiva (@thm:unif-biyectiva), existe una
  función inversa $"unif"_(b,f)^(-1)$ que también es biyectiva.
] <cor:unif-inverse>

#definition(title: [Desuniformización de un valor])[
  La desuniformización es el proceso inverso a la uniformización del vector,
  pues la función uniformización es biyectiva (@thm:unif-biyectiva) y existe
  una función inversa que también es biyectiva (@cor:unif-inverse). Dado
  $u = p / 2^(1-b) in bb(U)_b$, se denota la desuniformización como
  $x = "unif"_(b,f)^(-1) (u)$ y se define como:

  $ "unif"_(b,f)^(-1) : bb(U)_b -> bb(X)_(b,f) $
  $ "unif"_(b,f) (u) = p 2^f = u 2^(f-b+1) $
]

#definition(title: [Desuniformización de un vector])[
  Se exitiende la definición de la desuniformización a vectores pues por
  razones similares al @thm:unif-biyectiva, la uniformización de un vector
  también es biyectiva y existe una función inversa. Dado un vector
  $v in bb(U)_b^n$ se denota la desuniformización como
  $v' = "unif"_(b,f)^(-1) (v)$ y se define como:

  $ "unif"_(b,f)^(-1) (v) : bb(U)_b^n -> bb(X)_(b,f)^n $
  $ v' = "unif"_(b,f)^(-1) (v) $
  $ v'(i) = "unif"_(b,f)^(-1) (v(i)), forall i = 1, 2, ..., n $
]

Un caso específico de división que se ha utilizado a lo largo del proyecto es
la división entre un número entero, cabe recordar que $bb(X)_(b,0) = bb(I)_b$
(@sec:4-convenciones). Luego se obtiene el @cor:fixed-whole-division.

#corollary[
  Del @thm:fixed-division-set se deduce que dados $x = p 2^f in bb(X)_(b,f)$ y
  $n in bb(I)_(b') = bb(X)_(b',0)$, la división de un número en punto fijo
  entre un número entero está en el mismo conjunto excepto el cuando
  $n = -1$ y $x = -2^(b-1) 2^f$:

  $ x div n in bb(X)_(b,f) ", si " x != -2^(b-1) 2^f " y " n != -1 $

] <cor:fixed-whole-division>


=== Varianza
La varianza se define como:

  $ "Var"(v, m) = sum_(i=1)^m (x - mu (v, m))^2 $

donde $v in bb(R)^m$. Se parece mucho a la media, es una acumulación, su
diagrama de flujo sigue siendo parecido como se puede ver en
@fig:flujo-varianza.

#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((0,0), name: <A1>, [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((0,v-sep), align(center)[*2*:
      $"res" <- v(1)$ \
      $i <- 2$ \
      $mu' <- mu (v, m)$
      ], shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*2), name: <loop>, align(center)[*3*: ¿$i <= m$?], shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((0,v-sep*3), align(center)[
      *4*:
      $"res" <- "res" + (v(i) - mu')^2$],
      shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*4), name: <endloop>, align(center)[*5*: $i <- i + 1$])
    edge("l,u,u,r", "-|>")
    edge(<loop.east>, <end.west>, "-|>", [No])
    node((1, v-sep*2), name: <end>, align(center)[*6*: Fin], shape: shapes.pill)
  }),
  caption: [Algoritmo de la varianza]
) <fig:flujo-varianza>

==== Problemas
Para punto flotante sigue habiendo los mismos problemas que para la acumulación
(@algorithm-accumulation-problems) en cuanto a indexado:

- El paso *2*, $v(1)$ falla si $m = 0$
- En el paso *5*, $v(i+1)$ falla.

Sin contar los numerosos problemas que acarrea utilizar punto flotante:

- En el paso *3* puede subdesbordar $v(i) - mu'$.
- En el paso *3*, además, puede desbordar el cuadrado $(v(i) - mu')^2$
- Es más, en el paso *3*, la suma también puede desbordar:
  $"res" + (v(i) - mu')^2$.

==== Soluciones
Cuando los números son uniformes @uniform tenemos propiedades interesantes.
Por ejemplo, en el supuesto en que $v(i) - mu'$ fuera uniforme, su cuadrado
también lo será según el @thm:uniform-conv-product.

Además como se vio en la función media es (@algorithm-mean-problems), su
resultado también está en el conjunto que los elementos del vector:

  $ mu: bb(X)_(b,f)^n -> bb(N)^+ -> bb(X)_b $

De esta manera si estamos trabajando en $bb(U)_b$, también se sigue cumpliendo:

  $ mu: bb(U)_b^n -> bb(N)^+ -> bb(U)_b $

Se decide entonces trabajar con $v in bb(U)_b^m, m > 0$ e $i in bb(I)_B$.
Entonces $mu' in bb(U)_b$. El problema es que:

  $ v(i) - mu' in (-2, 2) $

Y entonces:

  $ (v(i) - mu')^2 in (0, 4) $

Para solucionarlo, antes de restar se dividen entre dos ambos operandos, para
que:

  $ v(i) div 2 - mu' div 2 in (-1, 1) $

Y por tanto:

  $ (v(i) div 2 - mu' div 2)^2 in (-1, 1) $

Luego, al final del algoritmo, se multiplica por $4$ y se desuniformiza el
resultado y se obtiene el valor que se espera de la varianza, como se puede
ver en la @fig:flujo-varianza-final, del cual las variables son:

- $i in bb(I)_k$
- $v in bb(X)_(b,f)^n$
- $v' in bb(U)_b^n$ (Por @def:unif-vector).
- $"res" in bb(U)_(b+k-1)$ (Por el @thm:accumulation)

La variable $"res"$ almacena lo que es el cuarto de la varianza. En el sexto
paso del algoritmo se desuniformiza y se multiplica por 4 para obtener el
verdadero resultado de la varianza.

 $ "res" in bb(U)_(b+k-1), "res" in [] $
 $ "unif"_(b+k-1,f)^(-1)("res") $

#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((0,0), name: <A1>, [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((0,v-sep), align(center)[*2*:
      $v' = "unif"(v)$ \
      $"res" <- v'(1)$ \
      $i <- 2$ \
      $mu' <- mu (v', m)$
      ], shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*2), name: <loop>, align(center)[*3*: ¿$i <= m$?], shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((0,v-sep*3), align(center)[
      *4*:
      $"res" <- "res" + (v(i) div 2 - mu' div 2)^2$],
      shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*4), name: <endloop>, align(center)[*5*: $i <- i + 1$])
    edge("l,u,u,r", "-|>")
    edge(<loop.east>, <sol.west>, "-|>", [No])
    node((1, v-sep*2), name: <sol>, align(center)[*6*:
      $"result" <- 4 *_(b',f') "unif"_(b+k-1,f)^(-1)("res" div n)$
      ], shape: shapes.pill)
    edge("-|>")
    node((1, v-sep*3), name: <end>, align(center)[*7*: Fin], shape: shapes.pill)
  }),
  caption: [Algoritmo de la varianza]
) <fig:flujo-varianza-final>

=== _Energy_
==== Problemas
==== Análisis de punto fijo
=== Transformada de Fourier (FFT)
==== Recursivo a iterativa
==== _Caché_ y $omega^k_n$
==== Consideraciones para punto fijo
=== Método de _Simpson_
=== _Welch_
=== Deformación dinámica del tiempo
=== Densidad espectral de potencia
