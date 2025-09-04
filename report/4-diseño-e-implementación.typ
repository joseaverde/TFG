#import "@preview/theorion:0.3.3": *
// #import cosmos.fancy: *
#import cosmos.rainbow: *
// #import cosmos.clouds: *
#show: show-theorion

#import "@preview/algorithmic:1.0.0"
#import algorithmic: algorithm
#import "@preview/cetz:0.3.4" as cetz
#import "@preview/fletcher:0.5.7" as fletcher: diagram, node, edge, shapes
#import "utility.typ": *

#show math.equation.where(block: false): box

= Diseño e implementación <sec:4>
En este capítulo se explican las decisiones de diseño que se han tomado para
implementar cada una de las partes que componen el algoritmo de detección de
ataques epilépticos _Patterns Augmented by Features Epileptic Seizure
Detection_. Se hace un análisis matemático de cada uno de los algoritmos que
lo componen y se estudia cómo se puede mejorar y cómo se pueden solucionar los
errores del mismo.

La parte de implementación se introduce con una lista de distintas convenciones
matemáticas que se usan a lo largo del capítulo. En ella se ofrece una
descripción del algoritmo de manera global utilizando dichas convenciones y
termina con un análisis de todos los algoritmos que se consideran relevantes
para el desarrollo del proyecto.

== Estudio de la solución final <sec:4-estudio-de-la-solución-final>
La implementación original del algoritmo de detección de ataques epilépticos
que se analiza en este proyecto (PaFESD) @PPMC-DAC se escribió
en Python 3.10. En este trabajo se analiza el impacto en el rendimiento al
utilizar otros lenguajes de programación, compiladores, técnicas y
herramientas, de cara a un análisis en el impacto energético y de seguridad
del programa.

Que la implementación original estuviera escrita en Python dificulta bastante
la compilación cruzada. Esto es especialmente difícil para dispositivos
empotrados como la ESP32C3, que tiene pocos megabytes de memoria disponibles
para almacenar el ejecutable, pues el tamaño de la suma de todas las
dependencias superaba los cientos de megabytes. Además uno de los requisitos
fundamentales del proyecto es que corriera en tiempo real. Por estas razones
técnicas se ha optado por utilizar otros lenguajes de programación.

Los lenguajes usados y las representaciones de valores numéricos utilizadas son
los siguientes: C++ 20 con punto flotante de simple precisión, C++ 20 con punto
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

- $bb(B) = {bfalse, btrue}$ es el conjunto de los valores lógicos falso
  ($bfalse$) y verdadero ($btrue$).
- $bb(Z) = {0, 1, -1, 2, -2, ...}$ es el conjunto de los números enteros.
- $bb(N) = {0, 1, 2, ...}$ es el conjunto de los números enteros no negativos.
- $bb(N)^+ = {1, 2, 3, ...}$ es el conjunto de los números enteros positivos.
- $bb(Q)$ es el conjunto de los números racionales.
- $bb(R)$ es el conjunto de los números reales.
- $bb(C)$ es el conjunto de los números complejos y $j = sqrt(-1)$ es la unidad
  imaginaria.

Como la implementación en el computador se trabaja con números con un tamaño
fijo de bits, a continuación se definen ciertos conjuntos interesantes para
ello. Se supone que los números enteros y en punto fijo se codifican en
complemento a dos:

- $bb(I)_n = bb(Z) inter [-2^(n-1), 2^(n-1)-1], n in bb(Z), n > 1$ es el
  conjunto de enteros de computador de $n$ bits codificados en complemento a
  dos.
- $bb(M)_n = bb(Z) slash 2^n bb(Z)$ es el anillo de enteros módulo $2^n$.
- $bb(X)_(b,f) = { 2^f x : x in bb(I)_b }, f in bb(Z)$ es el conjunto de números
  racionales en punto fijo binario de $b$ bits multiplicados por $2^f$.
  Nótese que $bb(X)_(b,0) = bb(I)_b$. Se denota además el valor más pequeño no
  nulo en valor absoluto del conjunto como $delta_(b,f) = 2^(-f)$.
- $cal(F)_32$ es el conjunto de valores codificados en el estándar de coma
  flotante IEEE 754 de simple precisión.
- $cal(F)_64$ es el conjunto de valores codificados en el estándar de coma
  flotante IEEE 754 de doble precisión.

Un vector $v in S^n$ es una secuencia de $n$ elementos de un conjunto $S$
cualquiera: $v = (v_1, v_2, ..., v_n)$. Se denota por $v(i)$ o $v_(i-1)$ el
i-ésimo elemento del vector $v$; donde $v_0$ o $v(1)$ es el primer elemento y
$v_(n-1)$ o $v(n)$ es el último, excepto cuando se diga lo contrario. No están
definidos los elementos $v(k), k <= 0 or k > n$.

  $ S^1 = S $
  $ S^n = S^(n-1) times S $

Además la notación $S^+$, inspirada por la clausura de Kleene, denota:

  $ S^+ = union.big_(i in bb(N)^+) S^i $

Además, si se se define $S^0 != {}$, se puede extender la notación de
$S^* = S^0 union S^+$. Estas notaciones son útiles para definir vectores.

Una muestra $m in bb(R)$ es un valor que ha sido leído por un sensor de
encefalograma en un instante de tiempo, el valor puede ser tanto negativo, como
positivo, como nulo. Una señal $S_r (t), S: bb(R)->bb(R)$ es una función que
asocia para un sensor cualquiera $r$ en un instante de tiempo $t$ una muestra.

Un sensor $r$ lee a una razón de $s in bb(N)^+$ muestras por segundo. A esa
constante se denomina _stride_. Así que en vez de trabajar sobre una señal
continua, se trabaja sobre una señal discreta como se muestra en la
@fig:signal.

/* *** SIGNAL IMAGE *** */

#figure(
  cetz.canvas({
    import cetz.draw: *
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

=== Conversión entre tipos en punto fijo binario
#definition(title: [$"conv"_f (x)$])[
  *Conversión de punto fijo*: Dado un valor en punto fijo
  $x = 2^f k$ con $x in bb(X)_(b,f)$ y con $k in bb(I)_b$, se denomina
  operación
  de conversión de punto fijo a la aplicación  $x' = k' 2^(f') in bb(Q)$ que
  aproxima el valor $x$ en un supuesto punto fijo que está multiplicado por el
  factor $2^(f')$.

  Sea $x' = 2^(f') k$ con $x' in bb(Q)$ y con $k' in bb(Z)$ el valor al que se
  quiere convertir $x$. Se denota como $"conv"_(f') (x) = x'$ donde
  $"conv"_(f'): bb(X)_(b,n) -> bb(Q)$ y donde:

  $ k' = cases(floor(2^(f - f') k)", " & "si" k >= 0,
               ceil(2^(f - f') k)", "  & "si" k < 0) $

  Porque $2^f k = 2^(f') ((2^f) / 2^(f') k)$ y $k'$ aproxima dicho valor.
]<def:fixed-point-conversion>

#note-box[
  Como dice más adelante el @thm:conv-bit-cond, si se cumple que
  $b' >= b + (f - f')$ se puede demostrar que si $x in bb(X)_(b,f)$ entonces se
  cumple que $"conv"_(f') (x) in bb(X)_(b',f')$. La función original no depende
  del número de bits del tipo de punto fijo de retorno, pues para las
  demostraciones es necesario trabajar sobre $bb(Q)$ y luego ya se puede añadir
  la restricción de que pertenzca o no a $bb(X)_(b',f')$.
]

/* FLOOR PROOFS */

#lemma[
  $floor(x) = n$ si y solo si $n <= x < n + 1$
] <lem:floor-range>

#corollary[
  Si $b > 0$ entonces, $a - b < b floor(a / b) <= a$.
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
  Si $b > 0$ entonces, $a <= b ceil(a / b) < a + b$
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
  Sean $x in bb(X)_(b,f)$ y $x' = "conv"_(f') (x) in bb(Q)$.
  Si $f >= f'$ entonces $x = x'$. Si $f < f'$ y $k >= 0$ se cumple que
  $0 <= x - x' < 2^(f')$, pero si $k < 0$ se cumple que $0 <= x' - x < 2^f'$.
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
si se convierte de un punto fijo multiplicado por un coeficiente más grande
a uno con uno más pequeño, no hay error y por tanto no se pierde información.
Por ejemplo, si consideramos $a = 1 dot.op 2^(-2) in bb(X)_(32,-2)$, al
convertirlo a un punto fijo con un bit más de precisión en la parte
fraccionaria como $"conv"_(32,-3) (a)$ se puede representar perfectamente como
$2 dot.op 2^(-3)$, pues el numerador debe ser entero.

Cuando $f < f'$, es decir, hay más bits en la parte fraccionaria del tipo de
origen que el de destino, hay un error (que está acotado) en el rango
$[0, 2^(-f'))$. De vuelta al ejemplo anterior, al convertirlo a uno con menos
bits en el denominador como $"conv"_(32,-1)$, no hay forma de representar el
valor $1/4$ con un numerador entero y el denominador $2$. Así que la conversión
da $0 dot.op 2^(-1)$, que está a como mucho $2^(-1)$ unidades del valor real.

Otras propiedades importantes son que el número de bits del tipo de punto
fijo no influye en la conversión ni en el error. Además el error solamente
depende del exponente $f'$ del coeficiente del tipo al que se convierte,
no depende de cuál era el exponente $f$ del coeficiente del valor origen. Esto
simplifica considerablemente el análisis del error.

#corollary[
  Dado $x in bb(X)_(b,f)$, su valor convertido $"conv"_(b,f)$ en valor absoluto
  no puede ser mayor que el valor absoluto de $x$. Es decir, si $x >= 0$
  entonces $0 <= "conv"_(b,f) (x) <= x$; y si $x < 0$ entonces se cumple que
  su valor convertido también es un punto fijo: $0 >= "conv"_(b, f) (x) >= x$.
] <cor:conv-order>

#theorem[
  Si $b' >= b + (f - f')$ entonces, dado un $x in bb(X)_(b,f)$, se tiene que
  $"conv"_(b',f') (x) in bb(X)_(b',f')$
] <thm:conv-bit-cond>

#proof[

  Dado el conjunto $bb(X)_(b,f)$, el valor máximo del conjunto es
  $M = 2^f 2^(b - 1) - delta_(b,f)$ y el valor mínimo es $m = -2^f 2^(b-1)$.
  La conversión se hace a $bb(X)_(b',f')$:

    $ m' = "conv"_(b',f') (m) = ceil(-2^(f-f') 2^(b - 1)) 2^(f') $
    $ M' = "conv"_(b',f') (M) = floor(2^(f-f') (2^(b - 1) - delta_(b,f))) 2^(f') $

  Para $m'$, si $f >= f'$ de acuerdo con el @thm:conv-error, $m' = m$, entonces
  solo se puede representar $m'$ en $bb(X)_(b',f')$ si
  $f - f' + b - 1 <= b' - 1$ pues el valor mínimo de dicho conjunto es
  $-2^(b' - 1)$. Se necesita que si $f >= f'$ entonces $b' >= b + (f - f')$.

  Si por el contrario $f < f'$, se deben encontrar los valores que satisfagan
  que $m' >= -2^(b' - 1)$. Por el @cor:conv-order se sabe que $0 >= m' >= m$,
  luego el valor de la conversión no es positivo (así que el límite superior
  se puede ignorar) y no es menor que el valor original $m$. Luego la
  restricción debe ser que $m <= m' => 2^(b - 1) 2^f <= 2^(b' - 1) 2^(f')$.
  Luego $b - 1 + f <= b' - 1 + f' => b' >= b + (f - f')$.

  Para $M'$, si $f >= f'$, de acuerdo con el @thm:conv-error, $M' = M$, entonces
  solo se puede representar $M'$ en $bb(X)_(b',f')$ si 

  $ M = &floor(2^(f-f') (2^(b - 1) - delta_(b,f))) 2^(f')   \
      = &floor(2^(f-f'+b-1) - 2^(f')) 2^(f')                \
      = & 2^(f+b-1) - 2^(f')                                \
      <=& (2^(b' - 1) - 1) 2^(f')                           \
       =& 2^(f' + b' - 1) - 2^(f')
  $
  $ => f + b - 1 <= f' + b' - 1 => b' >= b + (f - f') $

  Finalmente, si $f < f'$, de acuerdo con el @cor:conv-order $0 <= M' <= M$.
  Se trabaja con valores no negativos y, como se vio arriba, la condición más
  fuerte de que $b' >= b + (f - f')$ se sigue manteniendo.
]

=== Subconjunto uniforme de $bb(X)_(b,1-b)$ <uniform>
Para los siguientes algoritmos resulta bastante útil trabajar con un punto fijo
que esté en el rango $(-1, 1)$, pues se utilizan muchas multiplicaciones.
Además, dado que hay un valor negativo más que valores positivos también añade
complicaciones. Así, se define $bb(U)_b$ como el subconjunto uniforme de
$bb(X)_(b,1-b)$ o como conjunto uniforme de $b$ bits:

  $ bb(U)_b = bb(X)_(b,1-b) without {-1} $

#lemma[
  $abs(x) < 1 forall x in bb(U)_b, forall b > 1$
] <lem:uniform-less-than-one>

#theorem[
  Dados $x = p 2^(1-b)$ con $x in bb(U)_b$ e $y = q 2^f$ con $y in bb(U)_(b')$,
  se cumple que su producto $x y = p 2^(1-b) q 2^(1-b')$ también es uniforme:
  $x y in bb(U)_(b+b'-1)$.
] <thm:uniform-product>

#proof[

  Dados $x = p 2^(1-b)$ con $x in bb(U)_b$ e $y = q 2^f$ con $y in bb(U)_(b')$.
  Por definición $p in bb(I)_b without {-2^(b-1)}$ y
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
  que $x y = -1$, implicaría que $exists p in bb(I)_b$, $exists q in bb(I)_(b')$
  de forma que $p q = -2^(b+b'-2)$ para que $x y = p q 2^(2-b-b')
  = -2^(b+b'-2) 2^(2-b-b') = -1$. Sin embargo, $exists.not p in bb(I)_b$,
  $exists.not q in bb(I)_(b')$ de forma que $p q = -2^(b+b'-2)$, porque
  contradice que $abs(p q) < 2^(b+b'-2)$. Luego
  $exists.not x in bb(U)_b$, $exists.not y in bb(U)_(b')$ tales que $x y = -1$.
  Y por ende:

    $ x y in bb(X)_(b+b'-1,2-b-b') without {-1} $
    $ x y in bb(U)_(b+b'-1) $
]

#lemma(title: [Producto de enteros de computador])[
  El producto de dos $x in bb(I)_b$ e $y in bb(I)_(b')$ es un entero de
  $b + b' - 1$ bits: $x y in bb(I)_(b+b'-1)$.
] <thm:integer-product>

#theorem(title: [Producto de punto fijo])[
  Dados $x = p 2^f$ con $x in bb(X)_(b,f)$ e $y = q 2^(f')$ con
  $y in bb(X)_(b',f')$, su producto $x y = p q 2^(f+f')$ es otro punto fijo:
  $x y in bb(X)_(b+b'-1,f+f')$.
] <thm:fixed-product>

#proof[

  Dados dos conjuntos de punto fijo $bb(X)_(b,f)$ y $bb(X)_(b',f')$. Por
  definición:

    $ bb(X)_(b,f) = { k 2^f : k in bb(I)_b } $
    $ bb(X)_(b',f') = { k 2^(f') : k in bb(I)_(b') } $

  Sea $P$ el conjunto que contiene todos los posibles productos
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
  Dados $x in bb(X)_(b,f)$ e $y in bb(X)_(b',f')$, se denomina «conversión del
  producto de ambos en otro punto fijo» a la conversión ($"conv"_(b,f)$) del
  producto de $x$ e $y$, y se denota como

    $ x *_(b'',f'') y = "conv"_(b'',f'') (x y) $

  Por otro lado, si $x in bb(U)_(b)$ e $y in bb(U)_(b')$, denotaremos

    $ x *_(b'') y = "conv"_(b'',1-b'') (x y) $

  a la conversión del producto de ambos en otro punto fijo, pero uniforme.
] 

// NOTE: Se define/denomina/llama ___ como ____ y se denota como ____

#theorem(title: [Conversión del producto de uniformes es uniforme])[
  Si $x in bb(U)_b$ e $y in bb(U)_(b')$ entonces $x *_(b'') y in bb(U)_(b'')$.
] <thm:uniform-conv-product>

#proof[

  Dados $x in bb(U)_b$ e $y in bb(U)_(b')$. Sea su producto $z = x y$, que
  según el @thm:uniform-product se sabe que $z in bb(U)_(b+b'-1)$, se aplica
  el @thm:conv-bit-cond con:

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
  $"conv"_(b'',1-b'') (z) in bb(X)_(b'',1-b'')$,
  es decir, $x *_(b'') y in bb(X)_(b'', 1-b'')$. Lo único que es necesario
  determinar para terminar de demostrar este teorema es que
  $exists.not x in bb(U)_b, exists.not y in bb(U)_(b')$ tales que
  $x *_(b'') y = -1$.

  Supongamos que sí $exists x = p 2^(1-b) in bb(U)_(b)$ y sí
  $exists y = q 2^(1-b') in bb(U)_(b')$, para los que $z = x *_(b'') y = -1$.
  Según la @def:fixed-point-conversion, $z = k 2^(1-b'')$, donde $k$ es:

    $ k = cases(floor(2^((2 - b - b') - (1 - b'')) p q)", " & "si" p q >= 0,
                ceil(2^((2 - b - b') - (1 - b'')) p q)", "  & "si" p q < 0) $

  Se busca una $k$ para que $z = -1 = k 2^(1-b'')$ implique que
  $k = -2^(b''-1) in bb(I)_(b'')$. Como $k < 0 => p q < 0$.

    $ k = ceil(2^((2 - b - b') - (1 - b'')) p q) = -2^(b''-1) $

  Como $abs(p) < 2^(b-1)$ y $abs(q) < 2^(b'-1)$, $abs(p q) < 2^(b+b'-2)$. Y
  como $p q < 0$ entonces $0 > p q > -2^(b+b'-2)$.-

    $ 0 &&> p q >&& -2^(b+b'-2) \
      0 &&> 2^((2-b-b') - (1-b'')) p q >&& 2^((2-b-b') - (1-b'')) (-2^(b+b'-2)) \
      0 &&> 2^((2-b-b') - (1-b'')) p q >&& -2^(b''-1)
    $

  Se sabe que $ceil(-2^(b''-1)) = -2^(b''-1)$, porque $b'' > 1, b'' in bb(N)$.
  Según el @lem:ceil-range:

    $ ceil(-2^(b''-1)) = -2^(b''-1) <=> -2^(b''-1) - 1 < -2^(b''-1) <=
    -2^(b''-1) $

  Como $-2^(b''-1) < 2^((2-b-b') - (b''-1)) p q$, se concluye que
  $ceil(2^((2-b-b') - (b''-1)) p q) > -2^(b''-1)$. Luego 
  $exists.not x in bb(U)_b, exists.not y in bb(U)_(b'), x *_(b'') y = -1$, y
  por tanto: $x *_(b'') y in bb(U)_(b'')$.
]

=== Uniformización de un vector $bb(X)_(b,f)^n, n in bb(N)^+$
Antes de definir en qué consiste uniformizar un vector, es necesario definir
la división para el punto fijo. La división de dos números racionales:
$x = a/b$ e $y = p/q$, $x, y in bb(Q)$ y $a,b,p,q in bb(Q)$, es el siguiente
número
racional:

  $ x/y = (a/b)/(p/q) = (a q)/(b p) in bb(Q) $

Trabajar con números en punto fijo es similar: dados dos números
en punto fijo $x = p 2^f in bb(X)_(b,f)$ e $y = q 2^(f') in bb(X)_(b',f')$,
su cociente también es un número en punto fijo:

  $ x / y = (p 2^f) / (q 2^(f')) = p/q 2^(f-f') $

Sigue pareciendo un número en punto fijo multiplicado por $2^(f-f')$, la única
diferencia es que $exists p exists q, p/q in.not bb(Z)$, lo cual complica bastante
tratarlo como un punto fijo.

#definition(title: [División de punto fijo])[
  Sean $x = p 2^f in bb(X)_(b,f)$ e $y = q 2^(f') in bb(X)_(b,f')$ con
  *$y != 0$* dos números en punto fijo. Se define la división de punto
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
      anterior se obtiene un nuevo límite superior $2^(b-1)$.  Sin embargo, si
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
$bb(U)_b$ sin perder precisión. A este proceso le vamos a denominar
uniformización.

#definition(title: [Uniformización de un valor])[
  La uniformización de un vector convierte un valor en punto fijo
  $x = p 2^f in bb(X)_(b,f) without {-2^(b-1) 2^f}$ a otro conjunto uniforme
  $bb(U)_b$ sin perder precisión, es decir, el numerador es el mismo, el
  denominador cambia para que sea uniforme. Se denota como $u = "unif"(x)$ y se
  define como:

    $ "unif"_(b,f): bb(X)_(b,f) -> bb(U)_b $
    $ "unif"_(b,f) (x) = p 2^(1-b) = x / (2^(f+b-1)) $
] <def:unif-valor>

#definition(title: [Uniformización de un vector])[
  La uniformización de un vector convierte un vector $v in bb(X)_(b,f)^n$ a
  otro vector escalado $v' in bb(U)_b^n$ y se expande la definición de la
  función _unif_ para vectores:

  $ "unif"_(b,f): bb(X)_(b,f)^n -> bb(U)_b^n $
  $ v' = "unif"_(b,f) (v) $
  $ v'(i) = "unif"_(b,f) (v(i)), forall i = 1, 2, ... n $
] <def:unif-vector>

#theorem(title: [Uniformización es biyectiva])[
  La función $"unif"_(b,f)$ es biyectiva.
]<thm:unif-biyectiva>

#proof[

  La uniformización es inyectiva porque, por contradicción, supongamos que no
  es inyectiva y que
  $exists x = p 2^f in bb(X)_(b,f) without {-2^(b-1) 2^f},
   exists y = q 2^f in bb(X)_(b,f) without {-2^(b-1) 2^f}$ con
  $x != y$, tales que:

  $   && "unif"_(b,f) (x)      &&=& "unif"_(b,f) (y)      \
   => && "unif"_(b,f) (p 2^f)  &&=& "unif"_(b,f) (q 2^f)  \
   => && p 2^(1-b)             &&=& q 2^(1-b)             \
   => && p                     &&=& q $

  Se llega a una contradicción, así que es *inyectiva*.

  Finalmente, es suprayectiva porque la imagen de la función es el conjunto:

  $ =& {"unif"_(b,f) (x) : x in bb(X)_(b,f) without {-2^(b-1) 2^f}} & \
    =& {"unif"_(b,f) (p 2^f) : x in bb(I)_(b) without {-2^(b-1)}} &
      #text([[por definición]]) \
    =& {p 2^(1-b) : x in bb(I)_(b) without {-2^(b-1)}} & \
    =& {p 2^(1-b) : x in bb(I)_(b)} without {-2^(b-1) 2^(1-b)} & \
    =& {p 2^(1-b) : x in bb(I)_(b)} without {-1} & \
    =& bb(U)_b & #text([[por definición]])
  $

  Que cubre todo el codominio $bb(U)_b$, así que la función es *suprayectiva*.

  Como la función es a la vez inyectiva y suprayectiva, se dice que la función
  es *biyectiva*.

]

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
] <def:desunif-valor>

#definition(title: [Desuniformización de un vector])[
  Se extiende la definición de la desuniformización a vectores pues por
  razones similares al @thm:unif-biyectiva, la uniformización de un vector
  también es biyectiva y existe una función inversa. Dado un vector
  $v in bb(U)_b^n$ se denota la desuniformización como
  $v' = "unif"_(b,f)^(-1) (v)$ y se define como:

  $ "unif"_(b,f)^(-1) (v) : bb(U)_b^n -> bb(X)_(b,f)^n $
  $ v' = "unif"_(b,f)^(-1) (v) $
  $ v'(i) = "unif"_(b,f)^(-1) (v(i)), forall i = 1, 2, ..., n $
] <def:desunif-vector>

Un caso específico de división que se ha utilizado a lo largo del proyecto es
la división entre un número entero, cabe recordar que $bb(X)_(b,0) = bb(I)_b$
(@sec:4-convenciones). Luego se obtiene el @cor:fixed-whole-division.

#corollary[
  Del @thm:fixed-division-set se deduce que dados $x = p 2^f in bb(X)_(b,f)$ y
  $n in bb(I)_(b') = bb(X)_(b',0)$, la división de un número en punto fijo
  entre un número entero está en el mismo conjunto excepto cuando
  $n = -1$ y $x = -2^(b-1) 2^f$:

  $ x div n in bb(X)_(b,f) ", si " x != -2^(b-1) 2^f " y " n != -1 $

] <cor:fixed-whole-division>

=== Resumen del algoritmo
El algoritmo a implementar es el algoritmo 4 (_validation phase_) del artículo
en que se basa el proyecto @PaFESD. Para determinar si una época pertenece o no
a un ataque epiléptico se computan lo que el artículo llama _features_ (o
características) que son cinco funciones matemáticas: `max_distance`, `energy`,
`psd_1`, `psd_2` y `psd_3`. Si las características de una época están en
ciertos rangos determinados por el modelo, la época no es un artefacto y la
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


#pagebreak(weak:true)
== Algoritmos
/* ==== M A X _ D I S T A N C E ============================================ */
=== _Max distance_ <algorithm-max-distance>
_Max distance_ es sin duda uno de los algoritmos más sencillos. Retorna la
distancia pico a pico de una señal, es decir, la diferencia entre el valor
máximo y el valor mínimo de una época.  Sea $e in bb(R)^m$ una época de $m$
elementos, la entrada del algoritmo.
Obsérvese el diagrama de flujo siguiente (@fig:flujo-max-distance).

#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((0,0), name: <A1>, [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((0,v-sep), align(center)[*2*: \
      $ "mín"_v <-& e(1) \
        "máx"_v <-& e(1) \
        i       <-& 2 $
      ], shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*2), name: <loop>, align(center)[*3*: ¿$i <= m$?], shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((0,v-sep*3), align(center)[
      *4*: \
      $ "mín"_v <-& "mín"("mín"_v, e(i)) \
        "máx"_v <-& "máx"("máx"_v, e(i)) $],
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

La función en punto fijo depende, además de la época, en otros dos conjuntos:
El de entrada $bb(X)_(b_e,f_e)$ y el de salida $bb(X)_(b_s,f_s)$. De manera
que la época será un vector de $m$ elementos del primer conjunto:
$e in bb(X)_(b_e,f_e)^m$. Las variables $"mín"_v, "máx"_v in bb(X)_(b_e,f_e)$,
también pertenecen al conjunto de entrada. $i in bb(I)_b$ para un $b > 1$
cualquiera.

==== Soluciones

Para simplificar el problema, se va a utilizar valores de entrada de 32 bits
y la salida también será de 32 bits. Así que $b_e = b_s = 32$. Para
solucionar el problema que existía en la resta se debe convertir primero los
valores al tipo de retorno. De acuerdo con el @thm:conv-bit-cond
$b_s >= b_e + (f_e - f_s) => 32 >= 32 + (f_e - f_s) => f_s >= f_e$ para que
la conversión se pueda realizar.

Para que la resta se pueda realizar además es necesario que $f_e < f_s$.
Pues el valor se maximiza cuando $"máx"_v = (2^(31) - 1) 2^f_e$ y
$"mín"_v = -2^(31) 2^f_e$ entonces $"máx"_v - "mín"_v = 2^(32) - 1$. Nótese
que se pierde información como consecuencia del @thm:conv-error, se minimiza
la pérdida de información cuando más pequeño sea $f_s$. Así que $f_e + 1 = f_s$.
Las *precondiciones* son:

- $m > 0$, el vector debe tener al menos un elemento.
- $m < "máx"{bb(I)_b}$, la longitud del vector debe ser menor que el máximo del
  número entero que se utilice para indexar.
- $f_s = f_e + 1$, para poder operar y minimizar la pérdida de información.

Y las *postcondiciones* son:
- El resultado es no negativo, pues $"máx"_v >= "mín"_v$.

/* ==== A C U M U L A C I Ó N ============================================== */

#pagebreak(weak:true)
=== Acumulación <sec:acumulación>
Varios algoritmos tienen algún paso que consiste en suma una secuencia de
elementos. El algoritmo es similar al algoritmo _max distance_
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
Se siguen viendo similitudes con _max distance_, y se ve que comparte sus
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
  Si $a >= 2$ y $b >= 2$ entonces $a b >= a + b$
] <lem:product-greater-than-sum>

#theorem[
  Dados $i in bb(I)_B$ y $v(i) in bb(X)_(b,f) forall i = 1, 2, ..., m$,
  entonces se tiene que $sum_(k=1)^i v(k)$ está en $bb(X)_(b+B-1,f)$ y
  está en el intervalo $[-i 2^(b - 1) 2^f, i (2^(b - 1) - 1) 2^f]$
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
máximo se puede almacenar en una variable de tipo entero con signo de $B$ bits,
se necesitaría que el acumulador tuviera $b + B - 1$ bits para almacenar el
resultado, con la misma $f$ que el punto fijo de entrada.

==== SPARK <subsubsec:acc-spark>
A la hora de explicar esto a SPARK hay que utilizar una técnica que utiliza
un vector con las sumas parciales del vector de manera que
$v'(i) = sum_(j=1)^i v(i), i = 1, 2, ..., m$ @SPARKpartialSum.
Además, una condición adicional que se ha visto útil, es decir, que los valores
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

Finalmente, la postcondición dice que,

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
el bucle (`Loop_Invariant`), es decir, qué propiedades no cambian de una
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

Se debe «instanciar» la función que `Generic_Accumulation`, es decir, es
genérica (sirve para distintos tipos de tipos) y por lo tanto está
parametrizada (@lst:generic-accumulation-example:5). Se define una constante
fantasma con el vector de entrada, pero esta vez contiene los valores con el
tipo del resultado. Finalmente, en @lst:generic-accumulation-example:19 dice
que el resultado `Result` en la i-ésima posición tiene el mismo valor que el
valor en la i-ésima posición de `Generic_Accumulation`.

#pagebreak(weak:true)
/* ==== M E D I A ========================================================== */
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
      *6*: $"res"<-"res" div m$],
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
según la demostración del @thm:accumulation (esta vez con $m$ en vez del valor
máximo):

  $ "máx"_(m=m){sum_(i=1)^m v(i)} = m (2^(b-1) - 1) 2^f $
  $ "mín"_(m=m){sum_(i=1)^m v(i)} = -m 2^(b-1) 2^f $

Y por supuesto si se divide entre $m$ cualquiera de los dos para hacer la media
se obtiene el máximo y mínimo respectivamente del conjunto de los valores del
vector:

  $ "máx"_(m=m){sum_(i=1)^m v(i)} div m = (2^(b-1) - 1) 2^f $
  $ "mín"_(m=m){sum_(i=1)^m v(i)} div m = 2^(b-1) 2^f $

Sin embargo, nótese que esta condición no es cierta al trabajar con punto
flotante por problemas de precisión. Es bien conocido que trabajando con un
computado de punto flotante de 64 bits ($cal(F)_64$), por ejemplo, la suma
$0.1 + 0.2$ no da exactamente $0.3$, da $0.30000000000000004$; porque ni $0.1$
ni $0.2$ se pueden respresentar en binario con un número finito de dígitos
binarios: $0.1_(10) = 0.00overline(0011)_2$ y $0.2_(10) = 0.0overline(0011)$.
Luego no se puede hacer dicha suposición cuando se trabaja en cualquier
$cal(F)_b$.


==== Soluciones <algorithm-mean-solutions>
Con punto flotante siempre se trabaja con cierto error, así que hay poco que se
pueda hacer. Sin embargo, con punto fijo se puede aplicar los conceptos que se
utilizan para implementar la acumulación y se ve que si

- $i in bb(I)_k$
- $v in bb(X)_(b,f)^m$

Entonces la variable para acumular _res_ como dice @thm:accumulation debe
pertenecer a $bb(X)_(b+k-1,f)$. La división del final es una división de punto
fijo que pierde precisión pues se divide entre $m in bb(N)^+$
como dice el @thm:fixed-division-set. Como $m > 1$, se sabe que:

$ "res" div m in bb(X)_(b+k-1,f) $

y que está acotado en $[-m 2^(b - 1) 2^f, m (2^(b - 1) - 1]$, porque

  $ "máx"_(m=m){sum_(i=1)^m v(i)} = m (2^(b-1) - 1) 2^f $
  $ "mín"_(m=m){sum_(i=1)^m v(i)} = -m 2^(b-1) 2^f $

Aplicando la @def:fixed-division con $m in bb(N)^+$ para 

  $ (m (2^(b-1) - 1) 2^f) div m = floor((m 2^(b-1)-1)/m) 2^f = 2^(b-1)-1 $
  $ (-m 2^(b-1) 2^f) div m = ceil((-m 2^(b-1))/m) 2^f = -2^(b-1) $

Luego $"res" div m in [-2^(b-1), 2^(b-1)-1]$ y además $"res" div m in bb(X)_(b,f)$


#pagebreak(weak:true)
/* ==== V A R I A N Z A ==================================================== */
=== Varianza <sec:algorithm-variance>
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

==== Problemas <algorithm-variance-problems>
Para punto flotante sigue habiendo los mismos problemas que para la acumulación
(@algorithm-accumulation-problems) en cuanto a indexado:

- El paso *2*, $v(1)$ falla si $m = 0$
- En el paso *5*, $v(i+1)$ falla.

Sin contar los numerosos problemas que acarrea utilizar punto flotante:

- En el paso *3* puede subdesbordar $v(i) - mu'$.
- En el paso *3*, además, puede desbordar el cuadrado $(v(i) - mu')^2$
- Es más, en el paso *3*, la suma también puede desbordar:
  $"res" + (v(i) - mu')^2$.

==== Soluciones <algorithm-variance-solutions>
Cuando los números son uniformes (@uniform) se derivan propiedades interesantes.
Por ejemplo, en el supuesto en que $v(i) - mu'$ fuera uniforme, su cuadrado
también lo será según el @thm:uniform-conv-product.

Además, como se vio en la función media (@algorithm-mean-problems), su
resultado también está en el mismo conjunto que los elementos del vector:

  $ mu: bb(X)_(b,f)^m -> bb(N)^+ -> bb(X)_b $

De esta manera si estamos trabajando en $bb(U)_b$, también se sigue cumpliendo:

  $ mu: bb(U)_b^m -> bb(N)^+ -> bb(U)_b $

Se decide entonces trabajar con $v in bb(U)_b^m, m > 0$ e $i in bb(I)_B$,
entonces $mu' in bb(U)_b$. El problema es que:

  $ v(i) - mu' in (-2, 2) $

Y entonces:

  $ (v(i) - mu')^2 in (0, 4) $

Para solucionarlo, antes de restar se dividen entre dos ambos operandos, para
que:

  $ v(i) div 2 - mu' div 2 in (-1, 1) $

Y por tanto:

  $ (v(i) div 2 - mu' div 2)^2 in (0, 1) $

En este caso se va a definir la función «cuarto de la varianza», que computa
la cuarta parte de la varianza y sin desuniformizar. Esto se debe a que varios
algoritmos utilizan esta función y aprovechan que está dividida entre cuatro
para aplicar optimizaciones. El diagrama de flujo se muestra en la
@fig:flujo-varianza-final, del cual las variables son:

- $i in bb(I)_k$
- $v in bb(X)_(b,f)^m$
- $v' in bb(U)_b^m$ (Por @def:unif-vector).
- $"res" in bb(U)_(b+k-1)$ (Por el @thm:accumulation)

La variable _res_ almacena lo que es el cuarto de la varianza. En el sexto
paso del algoritmo se divide entre el número de elementos similar a como lo
hace la media (@algorithm-mean-solutions):

  $ "res" in& [0, m (2^(b-1)-1) 2^(1-b)] \
    "res" div m in& [0, (2^(b-1)-1) 2^(1-b)] $

Como $"res" div m in [0, 1)$ es uniforme, puede convertirse a cualquier tipo
en punto fijo binario uniforme.

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
      $"result" <- "res" div n$
      ], shape: shapes.rect)
    edge("-|>")
    node((1, v-sep*3), name: <end>, align(center)[*7*: Fin], shape: shapes.pill)
  }),
  caption: [Algoritmo de la cuarto de la varianza]
) <fig:flujo-varianza-final>

#pagebreak(weak:true)
/* ==== E N E R G Y ======================================================== */
=== _Energy_ <sec:energy>
La función _energy_ la define la implementación de referencia como la propia
varianza, que su vez se define en la @sec:algorithm-variance. En este caso se
extiende el algoritmo
del cuarto de la varianza para desuniformizar el resultado y multiplicarlo por
cuatro. Nótese que con la implementación de punto flotante no hay problema
porque no calcula el cuarto de la varianza, solo la de punto fijo.

En este caso no es necesario definir el diagrama de flujo, pues es una
única expresión dada por partes:

  $ q = "conv"_(b,1-b) ("quarter_variance"(v)), v in bb(U)_b $

$q$ es el cuarto de la varianza, se ha convertido a un tipo en punto fijo
uniforme de $b$ bits, una operación que puede hacer que se pierda
información, pero por razones prácticas se va a ignorar.

Este valor no es el real, además de estar dividido entre cuatro, está
uniformizado *dos* veces. Porque en un punto del cuarto de la varianza se hizo
el cuadrado del valor uniformizado:

  $ (v(i) div 2 - mu' div 2)^2 in (-1, 1) $

Por la @def:unif-valor el valor $v(i)$ que está uniformizado está implícitamente
multiplicado por $1 / 2^(f+b-1)$, de igual manera la media $mu'$ también está
implícitamente multiplicada por el mismo valor. Suponiendo
$mu' = mu'' / 2^(f+b-1)$ y que $v(i) = v''(i) / 2^(f+b-1)$, la expresión:

  $ (v''(i) / 2^(f+b-1) div 2 - mu'' / 2^(f+b-1) div 2) ^ 2 $
  $ (v''(i) div 2 - mu'' div 2) ^ 2  / (2^(f+b-1))^2 $

Está uniformizada dos veces, así que hay que aplicar la desuniformización
dos veces.

  $ q'' = "unif"_(b,f)^(-1) ("unif"_(b,f)^(-1) (q)) in bb(X)_(b,f) $

Finalmente, el resultado hay que multiplicarlo por cuatro, es decir, son
necesarios dos bits addicionales para almacenar el valor. Se puede ver que
como $4 in bb(X)_(3,0)$ y según el @thm:conv-bit-cond:

  $ 4 q'' in bb(X)_(b+3-1,f+0) = bb(X)_(b+2,f) $

Eso quiere decir que si $v in bb(X)_(b,f)^m$, entonces el tipo del resultado
debe ser convertible desde $bb(X)_(b+2,f)$, pues:

  $ "energy"(v) = 4 q'' in bb(X)_(b+2,f) $

Es decir, si $bb(X)_(b',f')$ es el tipo del resultado, según el
@thm:conv-bit-cond, se debe cumplir que $b' >= b + 2 + (f - f')$ para que
$"conv"_(b',f') ("energy"(v)) in bb(X)_(b',f')$.

/* ==== S I M P S O N ====================================================== */
#pagebreak(weak:true)
=== Regla de _Simpson_ <sec:simpson>
==== Regla de Simpson para datos irregularmente espaciados
La regla de Simpson, en honor de Thomas Simpson,  es un método para aproximar
integrales; existe un caso específico para integrar datos irregularmente
espaciados @simpson.

Sean el límite de integración inferior $a in bb(R)$ y el límite de integración
superior $b in bb(R)$ con $b > a$. Dados $N$ subintervalos de anchuras $h_k$,
la regla de Simpson compuesta viene dada por la expresión:


  $ integral_a^b f (x) d x approx phi +
    sum_(i=0)^(floor(N/2)-1) (h_(2 i) + h_(2 i+1))/6
    [(2 - h_(2 i+1)/h_(2 i)) f_(2 i)
    + ((h_(2 i) + h_(2 i + 1))^2) / (h_(2 i) h_(2 i +1))  f_(2 i + 1)
    + (2 - h_(2 i) / h_(2 i + 1)) f_(2 i + 2)] $

donde

  $ f_k = f (a + sum_(i=0)^(k-1) h_i) $

y donde $phi$ depende de la paridad de $N$, si hay $N$ intervalos, hay $N + 1$
anchuras,

  $ phi = cases(alpha f_N + beta f_(N - 1) - eta f_(N-2)
                &", si " N equiv 1 ("mód" 2),
                0 & ", si no") $

donde

  $ alpha =& (2 h_(N-1)^2 + 3 h_(N-1) h_(N-2)) / (6 (h_(N-2) + h_(N - 1))) \
    beta  =& (h_(N-1)^2 + 3 h_(N-1) h_(N-2)) / (6 h_(N-2)) \
    eta   =& (h_(N-1)^3) / (6 h_(N-2) (h_(N-2) + h_(N-1)))
  $

==== Regla de Simpson para datos uniformememente espaciados
Los datos de entrada están igualmente espaciados, es decir,
$h_k = h_j forall k, j = 1, 2, ... N - 1$, por lo que la expresión se puede
simplificar y por lo tanto se puede mejorar el tiempo de ejecución del
algoritmo.

  $ integral_a^b f (x) d x approx phi +
    sum_(i=0)^(floor(N/2)-1) (h + h)/6

    [(2 - h/h) f_(2 i)
    + ((h + h)^2) / (h h)  f_(2 i + 1)
    + (2 - h / h) f_(2 i + 2)] $

  $ integral_a^b f (x) d x approx phi +
    h/3 sum_(i=0)^(floor(N/2)-1)
    [f_(2 i) + 4  f_(2 i + 1) + f_(2 i + 2)] $

También se puede simplificar la expresión que da $phi$, porque

  $ alpha =& (2 h^2 + 3 h h) / (6 (h + h)) &&= 5 / 12 h \
    beta  =& (h^2 + 3 h h) / (6 h) &&= 2 / 3 h \
    eta   =& h^3 / (6 h (h + h)) &&= 1 / 12 h $

luego

  $ phi = cases(h/12 (5 f_N + 8 f_(N-1) - 1 f_(N-2))
                &", si " N equiv 1 ("mód" 2),
                0 & ", si no") $

Además, los valores $f_k$ vienen dados por el vector de valores uniformememente
espaciados de entrada $v (k+1) = f_k$, $v in bb(R)^(N-1)$.

==== Algoritmo
El algoritmo es similar al de una acumulación. Dado un vector de $m$ elementos
$v in bb(Q)^m$ con elementos igualmente espaciados en $h in bb(Q)$ unidades,
se deduce el algoritmo de la @fig:algorithm-simpson. Nótese que el paso *7*
comprueba que $m$ es par en vez de impar, porque si hay $m$ valores hay $m - 1$
intervalos.

#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((0,0), name: <A1>, [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((0,v-sep), align(center)[*2*:
        $"result" <- 0$ \
        $"i" <- 3$
      ], shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*2), name: <loop>, align(center)[*3*: ¿$i <= m$?], shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((0,v-sep*3), align(center)[
      *4*:
      $"res" <- "res" + v(i-2) +$\ $+ 4 v(i-1) + v(i)$],
      shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*4), name: <endloop>, align(center)[*5*: $i <- i + 2$])
    edge("l,u,u,r", "-|>")
    edge(<loop.east>, <sol.west>, "-|>", [No])
    node((1, v-sep*2), name: <sol>, align(center)[*6*:
      $"res" <- "res" dot.c h / 3$
      ], shape: shapes.rect)
    edge("-|>")
    node((1,v-sep*3), align(center)[*7*:
        $n equiv 0 ("mód" 2)$
      ], name: <parity>, shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((1, v-sep*4), name: <end>, align(center)[*9*: Fin], shape: shapes.pill)
    node((2, v-sep*3), name: <phi>, align(center)[
      *8*: $"res" <- "res" + h/12 [5 v(n)+$\ $ + 8 v(n-1) - v(n-2)]$],
      shape: shapes.rect)

    edge(<parity.east>, <phi.west>, "-|>", [No])
    edge(<phi.south>, <end.east>, "-|>")
  }),
  caption: [Regla de Simpson para datos uniformemente espaciados]
) <fig:algorithm-simpson>

==== Problemas
Nótese en la @fig:algorithm-simpson que en vez de indexar en el paso *4* con
$v(i)$, $v(i+1)$ y $v(i+2)$, se empeiza a contar en $3$ y se indexa en
$v(i-2)$, $v(i-1)$ y $v(i)$ respectivamente. Esto se debe a que comprobar que
no desborda en el límite superior del vector es más complicado que empezar
después. Se pueden ver las propiedades del bucle utilizando inducción:

1. *Caso base*: Al inicio del bucle $i = 3$, y el indexado del vector está
   definido para $v(i-2) = v(1)$, $v(i-1) = v(2)$ y $v(i) = v(3)$. Porque
   además se sabe que $i <= m$.
2. *Hipótesis inductiva*: En la $n"-ésima"$ iteración, $i = 3 + 2 n <= m$ y los
   valores $v(i-2) = v(2 n + 1)$, $v(i-1) = v(2 n + 2)$ y $v(i) = v(2 n + 3)$
   suponemos que están definidos.
3. *Tesis inductiva*: Para la $n+1"-ésima"$ iteración, suponiendo que $i <= m$
   (si no, habría salido del bucle en la bifurcación del paso *3*),
   $i = 3 + 2 (n + 1) = 5 + 2 n <= m$. Como $i <= m$, está definido
   $v(i) = v(5 + 2 n)$, y por lo tanto están definidos $v(i-1) = v(4 + 2 n)$
   y $v(i-2) = v(3 + 2 n)$, porque $0 < 3 + 2 n < 4 + 2 n < 5 + 2 n <= m $.

Los problemas están en los pasos:

- *Paso 5*: $i in bb(I)_B$ puede desbordar, eso hace necesario que
  $m <= 2^(B-1) - 3$.
- *Paso 8*: Si $m < 3$, no se puede indexar el vector, eso hace necesario que
  $m >= 3$.

Que como se ve se puede solucionar con la precondición $3 <= m <= 2^(B-1) - 3$.

==== Análisis de punto fijo
En punto fijo puede desbordar en el *paso 4* y en el *paso 8* de la
@fig:algorithm-simpson, hay que replantear el problema como una reducción
utilizando la acumulación mostrada en la @sec:acumulación, la cual sí está
demostrada.

Lo primero es reescribir la ecuación en dos sumas, una de los elementos pares
y otra de los elementos impares, ya que se puede ver que
$2 i equiv 0 ("mód" 2)$, $2 i + 2 equiv 0 ("mód" 2)$ y
$2 i + 1 equiv.not 0 ("mód" 2)$  de la siguiente manera.

  $ integral_a^b f (x) d x approx&
    phi + h/3 sum_(i=0)^(floor(N/2)-1) [f_(2 i) + 4  f_(2 i + 1) + f_(2 i + 2)] \
    =& phi + h/3 [ sum_(i=0)^(floor(N/2)-1) f_(2 i)
                 + 4 sum_(i=0)^(floor(N/2)-1) f_(2 i + 1)
                 + sum_(i=0)^(floor(N/2)-1)f_(2 i + 2) ] \
    =& phi + h/3 [ sum_(i=1)^(floor(N/2)-1) f_(2 i) + f_0
                 + 4 sum_(i=0)^(floor(N/2)-1) f_(2 i + 1)
                 + sum_(i=1)^(floor(N/2))f_(2 i)
                 ] \
    =& phi + h/3 [ sum_(i=1)^(floor(N/2)-1) f_(2 i) + f_0
                 + 4 sum_(i=0)^(floor(N/2)-1) f_(2 i + 1)
                 + sum_(i=1)^(floor(N/2)-1)f_(2 i) + f_(N')
                 ] \
  $


donde $N'$ depende de la paridad de $N$, si $N$ es par $N' = N$, si no
$N' = N - 1$, la expresión que queda es la siguiente:

  $ integral_a^b f (x) d x approx phi + h/3 [ 2 sum_(i=1)^(floor(N/2)-1) f_(2 i)
                 + 4 sum_(i=0)^(floor(N/2)-1) f_(2 i + 1) + f_0 + f_(N')] $

Para simplificar más adelante las demostraciones, es preciso que ambos
sumatorios empiecen y terminen en el mismo valor:

  $ integral_a^b f (x) d x approx phi + h/3 [ 2 sum_(i=1)^(floor(N/2)-1) f_(2 i)
                 + 4 sum_(i=1)^(floor(N/2)-1) f_(2 i - 1) + f_0 + f_(N') + 4 f_(N'')] $

donde $N''$ depende de la paridad de $N$, si $N$ es impar $N'' = N$, si no,
$N' = N - 1$.

Cambiando los índices $f_k$ por el vector $v in bb(X)_(b,f), v(k+1) = f_k$ que
indexa a partir de $1$, $n = N + 1$, $n' = N' + 1$, $n'' = N'' + 1$:

  $ integral_a^b f (x) d x approx phi + h/3 [ 2 sum_(i=1)^(floor(N/2)-1) v(2 i + 1)
                 + 4 sum_(i=1)^(floor(N/2)-1) v(2 i) + v(1) + v(n') + 4 v(n'')] $

Se puede ver que ambas sumas tienen la misma cantidad de sumandos, sea $s$ el
número de sumandos de ambas sumas que viene dado por la expresión:

  $ s = floor(N/2-1) - 1 + 1 = floor((n - 1)/2 - 1) = floor((n - 3) / 2) $

Sea $o in bb(X)_(b,f)^s$ y $e in bb(X)_(b,f)^s$ los vectores que contienen los
elementos en posiciones (contando desde 1) impares (excepto el primer y último
elementos impares) y pares (excepto el último elemento par) de $v$
respectivamente.

  $ o(i) =& v(2 i + 1), i = 1, 2, ..., floor(N / 2 - 1) \
    e(i) =& v(2 i), i = 1, 2, ..., floor(N / 2 - 1) $

De acuerdo con el @thm:accumulation como $s in bb(I)_B$, entonces
$sum_(i=1)^s o(i),$ $sum_(i=1)^s e(i) in bb(X)_(B+b-1,f)$ y además
$sum_(i=1)^s o(i)$, $sum_(i=1)^s e(i) in [-s 2^(b-1) 2^f, s (2^(b-1)-1) 2^f]$.
Además $2 sum_(i=1)^s o(i) in bb(X)_(B+b,f)$ y $2 sum_(i=1)^s o(i) in
[-2 s 2^(b-1) 2^f, 2 s (2^(b-1)-1) 2^f]$, según el @thm:conv-bit-cond con
$2 in bb(X)_(2,0)$; y de la misma manera $4 sum_(i=2)^s e(i) in
bb(X)_(B+b+1,f)$ y
$4 sum_(i=1)^s e(i) in [-4 s 2^(b-1) 2^f, 4 s (2^(b-1)-1) 2^f]$ por el mismo
@thm:conv-bit-cond con $4 in bb(X)_(3,0)$.

La suma de ambas sumas también está acotada:

  $  2 sum_(i=1)^(floor(N/2)-1) v(2 i + 1)
                 + 4 sum_(i=1)^(floor(N/2)-1) v(2 i) =
  2 sum_(i=1)^s o(i) + 4 sum_(i=1)^s e(i)
        in [-6 s 2^(b-1) 2^f, 6 s (2^(b-1)-1) 2^f] $

Los sumandos que faltan son $v(1) + v(n') + 4 v(n'')$, como
$v in bb(X)_(b,f)^m$, se puede ver las suma de esos tres valores como si fuera
$6 x$ para algún $x in bb(X)_(b,f)$, como $6 in bb(X)_(3,0)$ de acuerdo con el
@thm:conv-bit-cond:

  $ v(1) + v(n') + 4 v(n'') in [-6 dot.c 2^(b-1) 2^f, 6 (2^(b-1)-1) 2^f] $

Luego la suma

  $ 2 sum_(i=1)^s o(i) + 4 sum_(i=1)^s e(i) + v(1) + v(n') + 4 v(n'')
        in [-6 (s + 1) 2^(b-1) 2^f, 6 (s + 1) (2^(b-1)-1) 2^f] $

también está acotada. Si se divide el resultado entre $3 in bb(X)_(2,0)$
también sigue estando acotado:

  $ sigma = [2 sum_(i=1)^s o(i) + 4 sum_(i=1)^s e(i) + v(1) + v(n') + 4 v(n'')] div 3 \
        in [-2 (s + 1) 2^(b-1) 2^f, 2 (s + 1) (2^(b-1)-1) 2^f] $

Queda por añadir $phi$, que también está acotada:

  $ phi = cases(h/12 (5 v(n) + 8 v(n-1) - v(n-2))
                &", si " n equiv 0 ("mód" 2),
                0 & ", si no") $

El problema es que está multiplicada por $h$ y dificulta el análisis del punto
fijo. Así que es preferible trabajar con $phi'=phi/h$ y luego multiplicar por
$h$:


  $ integral_a^b f (x) d x approx h {phi' + 1/3 [ 2 sum_(i=1)^(floor(N/2)-1) v(2 i + 1)
                 + 4 sum_(i=1)^(floor(N/2)-1) v(2 i) + v(1) + v(n') + 4
                   v(n'')]} $

donde

  $ phi' = cases(1/12 (5 v(n) + 8 v(n-1) - v(n-2))
                 &", si " n equiv 0 ("mód" 2),
                 0 & ", si no") $

Por razones similares a la suma de los últimos sumandos, se puede ver que
$(5 v(n) + 8 v(n-1) - v(n-2) in [-14 dot.c 2^(b-1) 2^f, 14 (2^(b-1)-1) 2^f]$.
Y por tanto $(5 v(n) + 8 v(n-1) - v(n-2) div 12 in [-2 dot.c 2^(b-1) 2^f, 2
(2^(b-1)-1) 2^f]$. Nótese que no se ha decidido utilizar $14/12$ como factor y
se ha optado por $2$, porque simplifica después añadir $phi'$ al sumando.
Además, obviamente $0 in [-2 dot.c 2^(b-1) 2^f, 2 (2^(b-1)-1) 2^f]$. De ahí:

  $ sigma + phi' in [-2 (s + 2) 2^(b-1) 2^f, 2 (s + 2) (2^(b-1)-1) 2^f] $

Lo único que faltaría sería multiplicar $sigma + phi'$ por $h$. Sin embargo,
el producto complica demasiado la demostración, así que se ha decidido utilizar
un producto saturado, es decir:

  $ "sat_mult"(x, y, l, h) = cases(h & ", si " x y > h,
                                   l & ", si " x y < l,
                                   x y & ", si no") $

Ya que en la práctica si el resultado de Simpson es demasiado grande, supera el
valor del _batch_ de la densidad espectral de potencia y ya clasificaría como
«no ataque». El algoritmo definitivo se ve en la @fig:algorithm-simpson-fixed.

#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((0,0), name: <A1>, [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((0,v-sep), align(center)[*2*: \
        $"i" <-& 1 in bb(I)_B  \
         "s" <-& floor((m-3)/2) in bb(I)_B  \
         e <-& 0 in bb(X)_(b+B-1,f)  \
         o <-& 0 in bb(X)_(b+B-1,f)$
      ], shape: shapes.rect)
    edge("-|>")
    node((0,v-sep*2), name: <loop>, align(center)[*3*: ¿$i <= s$?], shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((0,v-sep*3), align(center)[
      *4*: \
      $e <-& e + v(2 i) \
       o <-& o + v(2 i + 1) \
       i <-& i + 1$],
      shape: shapes.rect)
    edge("l,u,r", "-|>")
    edge(<loop.east>, <sol.west>, "-|>", [No])
    node((1, v-sep*2), name: <sol>, align(center)[*5*:\
      $e   <-& 4 dot.c e \
       o   <-& 2 dot.c o \
       t   <-& v(m) in bb(X)_(b+3,f) \
       phi <-& 0 in bb(X)_(b+2,f) $
      ], shape: shapes.rect)
    edge("-|>")
    node((1,v-sep*3), align(center)[*6*:
        $n equiv 0 ("mód" 2)$
      ], name: <parity>, shape: shapes.parallelogram)
    edge("-|>", [Sí])
    node((1,v-sep*4), align(center)[*7*: \
        $t   <-& 4 v(m) + v(m - 1) \
         f   in& bb(X)_(b+5,f) \
         f   <-& 5 v(m) + 8 v(m-1) - v(m-2) \
         phi <-& f div 12$
      ], name: <cierto>, shape: shapes.rect)
    node((2,v-sep*4), align(center)[*8*:
        $t   <-& v(m) + 4 v(m - 1) $
      ], name: <falso>, shape: shapes.rect)
    edge(<parity.east>, <falso.north>, "-|>", [No])

    node((1.5,v-sep*5), align(center)[*9*: \
      $"r" <-& (e + o) div 3 + t + phi in bb(X)_(b+B+4,f)  \
       "result" <-& "sat_mult"(r, h, -2^(b'-1)2^(f'), (2^(b'-1)-1)2^(f'))$
    ], name: <endif>, shape: shapes.rect)
    edge("-|>")
    node((1.5,v-sep*6), align(center)[*10*: Fin], name: <fin>, shape: shapes.pill)
    edge(<cierto.south>, <endif.north>, "-|>")
    edge(<falso.south>, <endif.north>, "-|>")
  }),
  caption: [Regla de Simpson para datos uniformemente espaciados para punto fijo]
) <fig:algorithm-simpson-fixed>




/* ==== T R A N S F O R M A D A _ D E _ F O U R I E R ====================== */
#pagebreak(weak:true)
=== Transformada rápida de Fourier (FFT)
==== Transformada de Fourier discreta
Dado un vector $x in bb(C)^m$ de $m$ números complejos
$x_1, x_2, ..., x_m in bb(C)$, la transformada discreta de Fourier (DFT) se define
como un vector $X in bb(C)^m$ de $m$ números complejos
$X_1, X_2, ..., X_m in bb(C)$ que vienen dados por la fórmula:

  $ X_k = sum_(n=0)^(m-1) x_(n+1) e^((-2 pi j)/m k n) $

donde $j = sqrt(-1) in bb(C)$ es la unidad imaginaria.

==== Algoritmo de Cooley-Tukey -- Transformada rápida de Fourier
Es un algoritmo del tipo «divide y vencerás» que se pude combinar el resultado
de la transformada de Fourier del vector de los elementos en posiciones pares y
los de posiciones impares. Sin embargo, solo funciona para vectores con $m$
elementos donde $m = 2^k$ para algún $k in bb(N)$, es decir, potencias de dos.
Se deduce de la siguiente forma:

  $ X_k =& sum_(n=0)^(m-1) x_(n+1) e^((-2 pi j)/m k n) \
        =& sum_(n=0)^(m/2-1) x_(2 n) e^((-2 pi j)/m k (2 n))
          +  sum_(n=0)^(m/2-1) x_(2 n + 1) e^((-2 pi j)/m k (2 n + 1)) \
        =& sum_(n=0)^(m/2-1) x_(2 n) e^((-2 pi j)/m k (2 n))
          + e^((-2 pi j)/m k)
            sum_(n=0)^(m/2-1) x_(2 n + 1) e^((-2 pi j)/m k (2 n))
        =& E_k + e^((-2 pi j) / m k) O_k
  $

También se puede llegar a demostrar que:

  $ X_(k+N/2) = E_k - e^((-2 pi j) / m k) O_k $

por la periodicidad de la exponencial compleja.

El algoritmo se puede describir de manera recursiva:

  $ "FFT"(v,m) = cases("DFT"(v) & ", si " m equiv.not 0 ("mód" 2),
      cases(v_k =& "FFT"("pares"(v), m/2) (k) + e^((-2 pi j)/m k)
                       "FFT"("impares"(v), m/2) (k),
            v_(k+m/2) =& "FFT"("pares"(v), m/2) (k) - e^((-2 pi j)/m k)
                         "FFT"("impares"(v), m/2) (k)
                      ) k < m/2 & ", si no") $

Donde $"pares"(v)$ e $"impares"(v)$ son dos funciones que retornan dos vectores
con los elementos del vector $v$ en las posiciones pares e impares
resepectivamente.

==== Recursiva a iterativa
El algoritmo se define de manera recursiva, pero tiene inconvenientes. En
primer lugar, cada vez que se llama a la función el tamaño de la pila de
memoria del sistema incrementa pues necesita espacio adicional para almancenar
los resultados parciales. Esto podría provocar que desborde la pila cuando
recurra mucho, especialmente en un dispositivo empotrado.

El segundo problema es con SPARK, es complicado para el probador de teoremas
hacer demostraciones de funciones recursivas. Y mucho menos con una función
tan compleja como es la de la transformada de Fourier. Así que es preciso
«iterativizar» el algoritmo.

Para deducir cómo se podría implementar lo primero es definir los parámetros:

- $m = p 2^q > 1$, es la longitud del vector de entrada y salida, donde
  $p equiv.not 0 ("mód" 2)$, $p in bb(N)^+$ y $q in bb(N)$.
- $v = (v_0, v_1, ..., v_(m-1)), v in bb(C)^m$ es el vector de entrada.
- $w = (w_0, w_1, ..., w_(m-1)), w in bb(C)^m$ es el vector de salida.

Para simular recursión utilizando iteración, primero se ejecuta el caso
base y luego se ejecuta un bucle. Hasta ahora el algoritmo tiene la forma del
diagrama de flujo de la @fig:fft-recursive.

#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((1,0), name: <start>, [*1*: Inicio FFT], shape: shapes.pill)
    edge("-|>")
    node((1,v-sep*1), name: <branch>, align(center)[*2*: ¿$m equiv 0 ("mód" 2$)?],
      shape: shapes.parallelogram)
    edge("-|>", [No])
    node((2,v-sep*2), align(center)[*3*: $w <- "DFT"(v,m)$])
    edge("-|>")
    node((2, v-sep*3), align(center)[*4*: Fin], shape: shapes.pill)
    edge(<branch.south>, <rec.north>, "-|>", [Sí])
    node((1,v-sep*2), align(center)[*5*:\
      $E <-& "FFT"("pares"(v), m/2) \
       O <-& "FFT"("impares"(v), m/2)$], name: <rec>)
    edge("l,u,u,r", "-|>", [ #h(-2.2cm) Recursión])
    edge("-|>")
    node((1, v-sep*3), align(center)[*6*: Combinar $E$ y $O$])
    edge("r", "-|>")


  }),
  caption: [Diagrama de flujo de la transformada rápida de Fourier recursiva]
) <fig:fft-recursive>

En el paso recursivo se ve que el vector se divide en dos vectores de la mitad
de tamaño, uno con los elementos en posiciones pares (empezando por cero) y
posiciones impares. En cada paso recursivo siempre que sea múltiplo de dos,
se divide en pares e impares.

- En la primera iteración se divide $v$ en dos vectores $v_0$ y $v_1$:
  $v_0_i = v_(2 i)$ y $v_1_i = v_(2 i + 1)$, es decir, del vector $v$ los
  elementos en posiciones $i equiv 0 ("mód" 2)$ e $i equiv 1 ("mód" 2)$
  respectivamente.

- En la segunda iteración (si siguen siendo múltiplos de dos) se dividen de
  nuevo ambos vectores $v_0$ y $v_1$ en pares e impares:
  de $v_0$ salen $v_(0,0)$, $v_(0,1)$ y de $v_1$ salen $v_(1,0)$ y $v_(1,1)$.
  - $v_(0,0)$ y $v_(0,1)$ respecto a $v_0$ están en posiciones
    $i equiv 0 ("mód" 2)$ e $i equiv 1 ("mód" 2)$ respectivamente, pero en
    relación al vector $v$ original están en posiciones
    $i equiv 0 ("mód" 2)$ e $i equiv 2 ("mód" 2)$ respectivamente.
  - Del mismo $v_(1,0)$ y y $v_(1,1)$ respecto a $v_1$ están en posiciones
    $i equiv 0 ("mód" 2)$ e $i equiv 1 ("mód" 2)$ respectivamente, pero en
    relación al vector $v$ original están en posiciones
    $i equiv 1 ("mód" 2)$ e $i equiv 3 ("mód" 2)$ respectivamente.
  Se empieza a ver un patrón.

Para simplificar el problema, vamos a utilizar secuencias de índices. El vector
$w$ de $m$ elementos tiene todos los elementos en los índices
$w = (0, 1, ... m - 1)$. En la segunda iteración se toman los índices en
posiciones pares $w_0 = (0, 2, 4, ..., m - 2)$; y los impares conjunto
$w_1 = (1, 3, 5, ..., m - 1)$, siempre y cuando su cardinalidad sea par:
$|w| equiv 0 ("mód" 2)$.

Se denota el vector $w_(a_1,a_2,...,a_k)$ con $a_i in {0, 1}, i = 1, 2, ..., k$
al vector que ha tomado los elementos de paridad $a_k$ del vector
$w_(a_1,a_2,...,a_(k-1))$. El vector $w_(a_1,a_2,...,a_k)$ da la secuencia
ordenada
$(i : i equiv sum_(i=1)^(k) 2^(i-1) a_i ("mód" 2^k), i in 0, 1, ... m - 1)$.

#proof[

- *Hipótesis de inducción*: En la $k"-ésima"$ iteración si
  $m equiv 0 ("mód" 2^k)$, el vector $w_(a_1,a_2,...,a_k) =
  (i: i equiv sum_(i=1)^k 2^(i-1) a_i ("mód" 2^k), i in bb(Z))$. Sea
  $f = sum_(i=1)^k 2^(i-1) a_i$, entonces
  $w_(a_1,a_2,...,a_k) = (f, 2^k + f, 2 dot.c 2^k + f, 3 dot.c 2^k + f, ...)$.
- *Tesis inductiva*: La $k+1"-ésima"$ iteración, puede ser uno de dos vectores
  dependiendo si se divide en pares o impares:
  - *Pares*: El vector $w_(a_1,a_2,...,a_k,0)$, con $a_(k+1) = 0$ tiene los
    valores en posiciones pares del vector $w_(a_1,a_2,...,a_k) =
      (f, 2^k + f, 2 dot.c 2^k + f, 3 dot.c 2^k + f, ...)$.
    Es decir, los elementos $(f, 2 dot.c 2^k + f, 4 dot.c 2^k + f, ...)$
    $= (f, 1 dot.c 2^(k+1) + f, 2 dot.c 2^(k+1) + f, ...)$,
    Que cumplen el predicado:

    $ & (i : i equiv f ("mód" 2^(k+1)), i = 0, 1, ..., m - 1)  \
    = & (i : i equiv sum_(i=1)^k 2^(i-1) a_i ("mód" 2^(k+1)), i = 0, 1, ..., m - 1) \
    = & (i : i equiv 2^k a_(k+1) + sum_(i=1)^k 2^(i-1) a_i ("mód" 2^(k+1)), i = 0, 1, ..., m - 1) \
    = & (i : i equiv sum_(i=1)^(k+1) 2^(i-1) a_i ("mód" 2^(k+1)), i = 0, 1, ..., m - 1)
    $

  - *Impares*: De manera similar el vector $w_(a_1,a_2,...,a_k,1)$, con
    $a_(k+1) = 1$ tiene los valores en posiciones impares del vector
    $w_(a_1,a_2,...,a_k) = (f, 2^k + f, 2 dot.c 2^k + f, 3 dot.c 2^k + f, 4 dot.c 2^k, ...)$.
    Es decir, los elementos
    $(1 dot.c 2^k + f, 3 dot.c 2^k + f, 5 dot.c 2^k + f, 7 dot.c 2^k, ...)$
    $= (2^k + f,
        1 dot.c 2^(k+1) + 2^k + f,
        2 dot.c 2^(k+1) + 2^k + f,
        3 dot.c 2^(k+1) + 2^k + f, ...)$.  Que cumplen el predicado:

    $ & (i : i equiv 2^k + f ("mód" 2^(k+1)), i = 0, 1, ..., m - 1)  \
    = & (i : i equiv 2^k + sum_(i=1)^k 2^(i-1) a_i ("mód" 2^(k+1)), i = 0, 1, ..., m - 1) \
    = & (i : i equiv 2^k a_(k+1) + sum_(i=1)^k 2^(i-1) a_i ("mód" 2^(k+1)), i = 0, 1, ..., m - 1) \
    = & (i : i equiv sum_(i=1)^(k+1) 2^(i-1) a_i ("mód" 2^(k+1)), i = 0, 1, ..., m - 1)
    $
- *Caso base*: En la primera iteración cuando $k = 0$,
  $w = (0, 1, 2, 3, ..., m - 1)$.
]

Esta relación se puede ver gráficamente como muestra la @fig:fft-indices-mod.
Donde las dos flechas que salen de cada cuadrado significa que dividide el
vector en dos partes con los elementos en las posiciones $equiv i ("mód" 2^k)$
en la $k"-ésima"$ iteración.

#let invert-bits(x,bits) = {
  let r = 0
  for i in range(bits) {
    r = 2 * r + calc.rem(x, 2)
    x = calc.quo(x, 2)
  }
  return r
}

#figure(
  caption: [Relación entre el resto y la iteración en que está la transformada
            rápida de Fourier],
  cetz.canvas({
    import cetz.draw: *
    let height = 1cm
    let v-space = 1cm
    let width = 1.5cm
    let rows = 4
    let count = int(calc.pow(2, rows - 1))
    for row in range(rows) {
      let y = (height + v-space) * row
      for col in range(count) {
        rect((col * width, y), ((col + 1) * width, y + height))
        content(((col + 0.5) * width, y + height / 2),
                [$equiv #invert-bits(col, rows - row - 1)$])
        line(((col + 0.5) * width, y),
             ((col + 0.25) * width, y - height),
             mark: (end: "stealth"))
        line(((col + 0.5) * width, y),
             ((col + 0.75) * width, y - height),
             mark: (end: "stealth"))
      }
      content((count * width + 2cm, y + height / 2), [$k = #(rows - row)$])
      count = int(count / 2)
      width = width * 2
    }
  })
) <fig:fft-indices-mod>

El problema de esta aproximación es que para hacerlo de manera iterativa, se
empieza abajo (en el caso base) y se va subiendo hacia arriba combinando los
resultados. Supondría tener que cambiar los elementos de orden en el vector,
pues con los elementos ordenados en los índices: $0,1,2,3,4,5,...,m-3,m-2,m-1$.

Se propone una alternativa que se puede ver en la @fig:fft-indices-good, donde
las flechas indican que se combina la solución
(utilizando $X_n = E_n + e^((-2 pi j) / (2^k) n) O_n$) en el paso recursivo de
la función original. Se puede ver que en la $k"-ésima"$ iteración en el
$n"-ésimo"$ bloque se combina la solución del: $n"-ésimo"$ bloque de la $k-1$-ésima
iteración con el $n + 2^(k-1)"-ésimo"$ bloque de la $k-1"-ésima"$ iteración.

Esto se debe a que se definieron los índices previamente como
$(i: i equiv sum_(i=1)^k 2^(i-1) a_i ("mód" 2^k), i in 0, 1, ..., m - 1)$ para
el vector de índices $w_(a_1,a_2,...,a-k)$ en la $k"-ésima"$ iteración. Si el
siguiente era impar entonces tendría los elementos que sean $equiv 2^k + f
"mód" (2^(k+1))$ y si no eran los elementos que fueran $equiv f ("mód"
2^(k+1))$, donde $f = sum_(i=1)^k 2^(i-1) a_i$.

#figure(
  caption: [Propuesta de índices para la transformada de Fourier],
  cetz.canvas({
    import cetz.draw: *
    let height = 1cm
    let v-space = 1cm
    let width = 1.5cm
    let rows = 4
    let count = int(calc.pow(2, rows - 1))
    for row in range(rows) {
      let y = -(height + v-space) * row
      for col in range(count) {
        rect((col * width, y), ((col + 1) * width, y + height))
        content(((col + 0.5) * width, y + height / 2), [$equiv #col$])
      }
      for col in range(count * 2) {
        line(((col / 2 + 0.25) * width, y + v-space + height),
             ((calc.rem(col, count) + 0.5) * width, y + height),
             mark: (end: "stealth"))
      }
      content((count * width + 2cm, y + height / 2), [$k = q - #(rows - row - 1)$])
      count = int(count / 2)
      width = width * 2
    }
  })
) <fig:fft-indices-good>

Se puede ver en la @fig:fft-indices-good que en la $k"-ésima"$ iteración, el
$n"-ésimo"$ bloque tiene el doble de elementos que en la iteración anterior,
en este caso $p 2^k$ elementos, donde $p$ era la parte impar de la longitud
del vector ($m = p 2^q$), y se computa combinando las soluciones de los
bloques $n$ y $n + 2^(k-1)$.

Llamemos «bloque final» a este $n"-ésimo"$ bloque que combina las soluciones del
$n"-ésimo"$ bloque de la iteración anterior («bloque izquierdo») y el $n +
2^(k-1)$-ésimo bloque de la iteración anterior («bloque derecho»):

- El «bloque izquierdo» empieza en el índice $n p 2^(k-1)$ y tiene
  $p 2^(k-1)$ elementos.
- El «bloque derecho» empieza en el índice $(n + 2^(k-1)) p 2^(k-1)$ y tiene
  $p 2^(k-1)$.
- El «bloque final» empieza en el índice $n p 2^k$ y tiene $p 2^k$ elementos.

Esa es la clave para hacer que algoritmo deje de ser recursivo y sea iterativo.
Véase el diagrama de flujo que muestra la @fig:algorithm-fft-iterative, para
ver el resumen de cómo se podría implementar. Nótese que cuando aparece el
subíndice $w_i$ indexa desde cero $w_i = w(i+1)$.

Nótese que esto también es necesario para la transformada discreta de Fourier
del caso base cuando $p > 1$, en ese caso los elementos están separados $2^q$
elementos.

#[
  #set page(flipped: true)
  #set align(horizon)
  #let diagram = diagram(
    node-stroke: 1pt, {
      /* DTF */
    node((2,1), [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((1,1), align(center)[*2*: \
      $p, q:& p 2^q = m and\
            & p equiv 0 ("mód" 2)\
      w, w' in& bb(C)^m \
      a &<- 0 in bb(N)
      $], shape: shapes.rect)
    edge("-|>")
    node((1,2), name: <loop1>, [*3*: ¿$a < 2^q$?], shape: shapes.parallelogram)
    edge("l", "-|>", [No])
    edge("-|>", [Sí])
    node((1,3), [*4*: $b <- 0$])
    edge("-|>")
    node((1,4), name: <loop2>, [*5*: ¿$b < p$?], shape: shapes.parallelogram)
    edge("-|>", [Sí])
    edge("r,u", "-|>", [No])
    node((1,5), [*6*: $c <- 0$])
    edge("-|>")
    node((0,5), name: <loop3>, [*7*: ¿$c < p$?], shape: shapes.parallelogram)
    edge("u", "-|>", [No])
    edge("-|>", [Sí])
    node((-1,5), [*8*: \
      $ t <-& t + e^((-2 pi j)/p c) + v_(a + 2^q) \
        c <-& c + 1 \
      $], shape: shapes.rect)
    edge("d,r,u", "-|>")
    node((0,4), [*9*: \
      $ w_(a p+b) <- t \
        b <- b + 1 $], shape: shapes.rect)
    edge("r", "-|>")
    node((2,3), [*10*: $a <- a + 1$], shape: shapes.rect)
    edge("u,l", "-|>")
      /* FTF */
    node((0,2), [*11*: ¿$2^q > 1$?], shape: shapes.parallelogram)
    edge("u", "-|>", [No])
    edge("d", "-|>", [Sí])
    node((0,1), [*19*: Fin], shape: shapes.pill)
    node((0,3), [*12*: \ $ q <-& q - 1\  a <-& 0 $], shape: shapes.rect)
    edge("-|>")
    node((-1,3), [*13*: ¿$a < 2^q$?], shape: shapes.parallelogram)
    edge("u", "-|>", [No])
    edge("l", "-|>", [Sí])
    node((-2,3), [*14*: $b <- 0$], shape: shapes.rect)
    edge("-|>")
    node((-2,4), [*15*: ¿$b < p$?], shape: shapes.parallelogram)
    edge("r", "-|>", [No])
    edge("l", "-|>", [Sí])
    node((-3,4), [*16*: \
      $ E                    <-& w_(a p + b) \
        O                    <-& w_((a + 2^q) p + b) \
        w'_(2 a p + b)       <-& E + O e^((-2 pi j)/p b) \
        w'_((2 a + 1) p + b) <-& E - O e^((-2 pi j)/p b) \
        b                    <-& b + 1 \
      $], shape: shapes.rect)
    edge("d,r,u", "-|>")
    node((-1,4), [*17*: $a <- a + 1$], shape: shapes.rect)
    edge("u", "-|>")
    node((-1,2), [*18*: \ $
      p <-& 2 p \
      q <-& q - 1 \
      w <->& w' $], shape: shapes.rect)
    edge("r", "-|>")
  })
  #figure(scale(y: 70%, x: 84%, reflow: true, diagram),
    caption: [Diagrama de flujo de la transformada rápida de Fourier iterativa]
  ) <fig:algorithm-fft-iterative>
]

==== Consideraciones para punto fijo <sec:fft-fixed>
Para punto fijo, el algoritmo desborda en el *paso 8* y en el *paso 16*, se
puede demostrar que en el resto de pasos no hay errores, pero se sale del
alcance de la memoria de este trabajo. Sin embargo existe un estudio del doctor
P. Welch @welch2003fixed que permite simplificar el desarrollo del algoritmo de
la transformada de Fourier utilizando punto fijo.

Este algoritmo permite eliminar el desbordamiento en el *paso 16* del algoritmo
que se muestra en la @fig:algorithm-fft-iterative y sus técnicas se pueden
aplicar para eliminar también el desbordamiento en el *paso 8*. Este algoritmo
necesita que el vector esté uniformizada, es decir, con todos los elementos en
el conjunto abierto $(-1, 1)$.

#theorem[
  $abs(x y) = abs(x) abs(y)$, para cualesquiera $x, y in bb(C)$.
] <thm:abs-prod-prod-abs>

#proof[

  Dados $x = "Re"{x} + "Im"{x}j, x in bb(C)$ e
  $y = "Re"{y} + "Im"{y}j, y in bb(C)$, donde $j=sqrt(-1)$ es la unidad
  imaginaria.

  $ x y = & ("Re"{x} + "Im"{x} j) ("Re"{y} + "Im"{y} j) \
        = & ("Re"{x} "Re"{y} - "Im"{x} "Im"{y})
          + ("Re"{x} "Im"{y} + "Im"{x} "Re"{x} j) \
    "Re"{x y} =& ("Re"{x} "Re"{y} - "Im"{x} "Im"{y}) \
    "Im"{x y} =& ("Re"{x} "Im"{y} + "Im"{x} "Re"{y})
  $

  $ abs(x) = sqrt("Re"{x}^2 + "Im"{x}^2) $
  $ abs(y) = sqrt("Re"{y}^2 + "Im"{y}^2) $
  $ abs(x) abs(y) = sqrt(("Re"{x}^2 + "Im"{x}^2) ("Re"{y}^2 + "Im"{y}^2)) $
  $ abs(x y) =& sqrt("Re"{x y}^2 + "Im"{x y}^2) \
             =& sqrt(("Re"{x} "Re"{y} - "Im"{x} "Im"{y})^2 +
                     ("Re"{x} "Im"{y} + "Im"{x} "Re"{y})^2) \
             =& sqrt(("Re"{x}^2 "Re"{y}^2 + "Im"{x}^2 "Im"{y}^2 +
                     - 2 "Re"{x} "Re"{y} "Im"{x} "Im"{y} \
                     + "Re"{x}^2 "Im"{y}^2 + "Im"{x}^2 "Re"{y}^2
                     + 2 "Re"{x} "Re"{y} "Im"{x} "Im"{y})
                   ) \
             =& sqrt("Re"{x}^2 "Re"{y}^2 + "Im"{x}^2 "Im"{y}^2 +
                     "Re"{x}^2 "Im"{y}^2 + "Im"{x}^2 "Re"{y}^2) \
             =& sqrt("Re"{x}^2 ("Re"{y}^2 + "Im"{y}^2) +
                      "Im"{x}^2 ("Re"{y}^2 + "Im"{y}^2)) \
             =& sqrt("Re"{x}^2 abs(y)^2 + "Im"{x}^2 abs(y)^2) \
             =& sqrt(abs(y)^2 + ("Re"{x}^2 + "Im"{x}^2)) \
             =& sqrt(abs(y)^2 + abs(x)^2) \
             =& abs(x) abs(y) $
]

#corollary[
  Del @thm:abs-prod-prod-abs se obtiene que, dados $x, y in bb(C)$, si
  $abs(x) < 1 and abs(y) < 1$ entonces $abs(x y) < 1$.
] <thm:complex-uniform-product>

#definition(title: [Fórmula de Euler])[
  $ e^(j x) = cos(x) + j sin(x) $
  $ e^(-j x) = cos(x) - j sin(x) $
donde $j = sqrt(-1)$.
] <def:euler>

#theorem(title: [Relación pitagórica])[
  $ sin^2(theta) + cos^2(theta) = 1 $
] <thm:pythagoras>

#lemma(title: [Raíz de la unidad])[
  De la @def:euler y el @thm:pythagoras se obtiene que:

  $ abs(e^(j x)) = 1, forall x in bb(R) $
]

#theorem[
  Sea $x in bb(C)$ tal que $abs(x) < a$ entonces
  $abs("Re"{x}) < a and abs("Im"{x}) < a$, $a in bb(R), a >= 1$.
] <thm:abs-parts>

#proof[

  Dado $x in bb(C)$, con $abs(x) < a$:

    $ abs(x) =& sqrt("Re"{x}^2 + "Im"{x}^2) < a \
            =>& "Re"{x}^2 + "Im"{x}^2 < a^2
    $

  Por contradicción si $abs("Re"{x}) >= a$, entonces $"Re"{x}^2 >= a^2$ y
  como $"Im"{x}^2 >= 0$, se llega a una contradicción porque entonces
  $"Re"{x}^2 + "Im"{x}^2 >= a^2$. Lo mismo aplica si $"Im"{x} >= a$,
  entonces es necesario que $abs("Re"{x}) < a$ y que $abs("Im"{x}) < a$.
]

#theorem(title: [Desigualdad triangular])[
  Dados $x, y in bb(C)$, se cumple que $abs(x + y) <= abs(x) + abs(y)$.
] <thm:triangular>

En el *paso 16* se computa:

  $ E plus.minus O e^((-2 pi j)/p b) $

Supongamos que se cumple antes de ejecutar el *paso 16* que $abs(E) < 1$ y que
$abs(O) < 1$. Entonces por el @thm:abs-prod-prod-abs se puede ver que

  $ abs(O e^((-2 pi j)/p b)) = abs(O) abs(e^((-2 pi j)/p b)) = abs(O) < 1 $

Como $abs(E) < 1$, se puede ver, por la desigualdad triangular
(@thm:triangular), es cierto que:

  $ abs(E plus.minus O e^((-2 pi j)/p b))
      <=& abs(E) + abs(O e^((-2 pi j)/p b)) \
      <=& abs(E) + abs(O) \
       <& 2 $

Es decir al final del *paso 16* el módulo del número complejo se duplica.
Además si se aplica el @thm:abs-parts, se ve que la parte real y parte
imaginaria se duplican. Esta propiedad es muy interesante de cara a ver cómo
crecen los valores en punto fijo y es lo que aprovecha P. Welch para dar tres
posibles aproximaciones, de las cuales se ha decidido utilizar la más sencilla,
porque si no, las demostraciones para el probador de teoremas resultarían muy
complicadas.

Después del *paso 16*, se divide el número complejo entre dos, de esta manera
se mantiene la invariante de que el módulo de $E$ y $O$ y por tanto de todos
los elementos de $w$ están en el conjunto abierto $(-1, 1)$. Así se pueden
utilizar el conjunto uniforme $bb(U)_b$, y al final se reescala el resultado.


/* ==== W E L C H ========================================================== */
#pagebreak(weak:true)
=== _Welch_ <sec:welch>
El método de Welch, también conocido como el método de periodograma, se utiliza
para estimar la densidad espectral y consiste en dividir la señal temporar en
bloques sucesivos, formar el periodograma de cada bloque y hacer la media
@SpectralAudioSignalProcessing.

Se denota el $m$-ésimo marco al que se le ha aplicado una ventana $w$ y que ha
sido rellenado con ceros de la señal $x$ como:

  $ x_m (n) eq.delta w(n) x(n + m R), n = 0, 1, ..., M - 1, m=0,1,...,K-1 $

donde $R$ es el solapamiento de la ventana, y sea $K$ el número de marcos
disponibles @SpectralAudioSignalProcessing. Y donde $M$ es el tamaño del marco.
Luego el periodograma del $m$-ésimo bloque viene dado por

  $ P_(x_m,M) (omega_k) = 1/M abs("FFT"_(N,k) (x_m))^2
              eq.delta 1/M abs(sum_(n=0)^(N-1) x_m (n) e^(-j 2 pi n k / N))^2 $

el estimado de Welch para la densidad espectral viene dado por

  $ hat(S)_x^W (omega_k) eq.delta 1/K sum_(m=0)^(K-1) P_(x_m,M) (omega_k) $

en otras palabras, es la media de los periodogramas a lo largo del tiempo
@SpectralAudioSignalProcessing.

En este caso la ventana que la implementación de referencia ha utilizado es la
ventana de Hann. Para ventana de $n$ elementos, se define la función de ventana
de Hann $w_n (x)$ como:

  $ w_n : bb(N) inter [0, n) -> bb(R) $
  $ w_n (x) = 1/2 - 1/2 cos((2 pi x) / n) $

En primer lugar, mencionar que la imagen de la función de ventana de Hann
$w_n$ es el conjunto $[0, 1]$, porque $cos: bb(R) -> [-1, 1]$ y luego
$-1/2 cos(x) in [-1/2, 1/2], forall x in bb(R)$. Así que no hay problemas al
multiplicar por la ventana utilizando punto fijo.

El algoritmo completo se encuentra en la @fig:algorithm-welch, dada una señal
$x in bb(R)^l$ de $l in bb(N)^+$ elementos, una función ventana
$w: bb(N) -> bb(R)$, un tamaño de marco $M in bb(N)^+, M < l$, un tamaño de
solapamiento $R in bb(N)^+, R < M$ y la frecuencia de muestreo $f > 0$, $f in
bb(R)$. Nótese que el vector $x$ se indexa desde 0 en vez de 1 para este caso.
Además se extiende la definición de $x(i)$ para cuando $i >= l$, en cuyo caso
$x(i) = 0, forall i >= l$.

En el paso final además se aplica un factor de normalización que se va a llamar
$mu_(w,n)$ que depende de la función de ventana y se calcula como:

  $ mu_(w,n) = sum_(i=0)^n w(n) $

#figure(
  scale(100%, diagram(
    node-stroke: 1pt, {
    node((1,0), [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((1,1), align(center)[*2*:\
      $ K     =& floor((l-M)/R) + 1 \
       "Pxx" <-& (0, 0, ..., 0) in bb(R)^(floor(l/2)+1) \
       m     <-& 0 in bb(N) $
      ], shape: shapes.rect)
    edge("-|>")
    node((1,2), [*3*: ¿$m < K$?], shape: shapes.parallelogram)
    edge("r", "-|>", [No])
    edge("d", "-|>", [Sí])
    node((1,3), align(center)[*4*:\
      $ x_m (n) eq.delta& w(n) x(n+m R), n=0,1,...,M-1 \
        t             <-& "FFT"(x_m) \
        "Pxx"(k)      <-& "Pxx"(k) + abs(t(k))^2, k=0,1,...,floor(l/2) \
        m             <-& m + 1
      $
      ], shape: shapes.rect)
    edge("l,u,r", "-|>")
    node((2,2), align(center)[*5*: $"Pxx"(k) <- "Pxx"(k) 1 / (K f mu_(w,n))$
      ], shape: shapes.rect)
    edge("-|>")
    node((2,3), [*6*: Fin], shape: shapes.pill)
  })),
  caption: [Diagrama de flujo del método de Welch]
) <fig:algorithm-welch>

==== Análisis de punto fijo
Los dos pasos más problemáticos son el de la suma del *paso 4* y la división del
*paso 5*. Para solucionarlos se va a utilizar una técnica inspirada en la
solución propuesta para la transformada rápida de Fourier de la @sec:fft-fixed.

La señal que retorna la transformada de Fourier está uniformizada y está
escalada, así que se puede suponer que el resultado de la transformada de
Fourier es un vector de valores uniformes $bb(U)_b subset (-1, 1)$. Para no
perder tanta precisión el tipo del vector _Pxx_ será uno uniforme con el
doble de bits, es decir $bb(U)_(2 b)$.

A partir de ahora se va a tener una variable de tipo entero $s in bb(I)_b$ que
almacenará el valor de reescalado y que irá incrementando en cada iteración.
Al final del algoritmo se reescalará multiplicando por $2^s$ y luego, como se
explicará más adelante, desuniformizando dos veces. Para ello, la transformada
de Fourier se define como:

  $ "FFT"': bb(U)_b^n -> (bb(U)^n dot.c bb(N)) $
  $ (r, s) = "FFT"'(x) $

Retorna dos valores $r$, que es el vector resultante de la transformada de
Fourier, y $s$ es el factor de escalado de la transformada de Fourier que dice
que debe multiplicarse $2^s$ a cada uno de los elementos de $r$.
Véase la @fig:algorithm-welch-fixed, con el algoritmo definitivo para punto
fijo.

#figure(
  scale(85%, diagram(
    node-stroke: 1pt, {
    node((1,0), [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((1,1), align(center)[*2*:\
      $ K     =& floor((l-M)/R) + 1 \
       "Pxx" <-& (0, 0, ..., 0) in bb(U)_(2 b)^(floor(l/2)+1) \
       m     <-& 0 in bb(I_b) \
       s     <-& 0 in bb(I)_b
       $
      ], shape: shapes.rect)
    edge("-|>")
    node((1,2), [*3*: ¿$m < K$?], shape: shapes.parallelogram)
    edge("r", "-|>", [No])
    edge("d", "-|>", [Sí])
    node((1,3), align(center)[
      *4*: $x_m (n) eq.delta& w(n) x(n+m R), n=0,1,...,M-1$], shape: shapes.rect)
    edge("-|>")
    node((1,4), align(center)[*5*: $(t, s') <-& "FFT"'(x_m)$])
    edge("-|>")
    node((2,4), align(center)[
      *6*: \
      $ N(k) <-& abs(t(k)/2)^2, k=0,1,...,floor(l/2) \
        s'   <-& 2 s' + 2 $
    ], shape: shapes.rect)
    edge("-|>")
    node((2,5), align(center)[
      *7*: $ (e_l,e_r,s) <- cases(
        (1, 0, s+1)     &", si " s = s' \
        (s'-s+1,0,s'+1) &", si " s < s' \
        (1, s-s', s+1)  &", si " s > s')
      $
    ], shape: shapes.rect)
    edge("-|>")
    node((1,5), align(center)[*8*: $
        "Pxx"(k)      <-& "Pxx"(k) / 2^(e_l) + N(k) / 2^(e_r), k=0,1,...,floor(l/2) \
        m             <-& m + 1
      $
      ], shape: shapes.rect)
    edge("l,u,u,u,r", "-|>")
    node((2,2), align(center)[*9*: $"Pxx"(k) <- "Pxx"(k) 2^s / (K f mu_(w,n))$
      ], shape: shapes.rect)
    edge("-|>")
    node((2,3), [*10*: Fin], shape: shapes.pill)
  })),
  caption: [Diagrama de flujo del método de Welch]
) <fig:algorithm-welch-fixed>

Es una aproximación equivalente, pero sin desbordamientos.

- *Paso 2*: $s <- 0 in bb(I)_b$, es el factor de escalado. La señal está
  uniformizada, pero todavía no está escalada. Nótese que al inicio del
  algoritmo se mantiene el predicado de que
  $"Pxx"(i) in bb(U)_(2 b) subset [0, 1), forall i = 0, 1, ..., floor(l/2)$.
- *Paso 4*: No puede haber desbordamientos porque la señal está normalizada
  y al menos la ventana de Hann retorna un valor en el conjunto $[0, 1]$, luego
  el producto también es uniforme.
- *Paso 5*: Se computa la transformada de Fourier, el resultado se almacena
  en la variable $t$ y el factor de escalado, en la variable $s'$. La señal
  $x_m$ está uniformizada, así que se puede llamar a $"FFT"'$ sin problema,
  el resultado de dicha transformada está uniformizado y escalado entre un
  factor de $2^(s')$
- *Paso 6*: En vez de calcular las normas al cuadrado de los números complejos
  del vector resultado de la transformada de Fourier $t$, se calcula un cuarto
  de la norma al cuadrado. Esto se debe a que $"Re"{t(k)}, "Im"{t(k)} in (-1, 1),
  forall k = 0, 1, ..., floor(l/2)$, pero no sabemos si la norma es menor que
  uno o no, así que $abs(t(K)) in [0, 2), forall k = 0, 1, ..., floor(l/2)$.
  Si se divide entre dos sus elementos la norma está en $[0, 0.5)$.

  Estos valores de los cuartos de las normas al cuadrado se almancenan en el
  vector de las normas. El factor de escalado $s'$ ha cambiado, como se hace
  el cuadrado el factor de escalado se cuadra, también se añade $2$, porque
  se ha dividido entre $2^2$ y luego hay que reescalarlo. Nótese que se podría
  obtener un mejor factor de escalado si en vez de utilizar el peor caso, se
  tomara el valor absouto máximo de las normas y se escalara de acuerdo con él.
- *Paso 7*: Los valores del vector _Pxx_ en este punto están divididos entre
  el factor de escalado $2^s$, a la hora de sumar el valor antiguo de _Pxx_
  con su norma $N$ es necesario que tengan el mismo factor de escalado. En este
  paso se calculan el exponente del divisor de _Pxx_ ($e_l$) y el de $N$
  ($e_r$), y se obtiene el factor de escalado del resultado:

  - Si $s = s'$, recordemos que $N(k) in [0, 0.5)$, pero que $"Pxx"(k) in [0,
    1)$. Así que $"Pxx"(k)$ se debe dividir entre $2$, por lo que $e_l = 1$ y
    $e_r$. El resultado estará en el rango $[0, 1)$ y como se ha dividido
    $"Pxx"(k)$ entre dos, el factor de escalado aumenta en uno.

  - Si $s < s'$, significa que $N(k)$ está dividido entre una potencia más
    grande de dos que $"Pxx"(k)$, por lo que hay que dividir $"Pxx"(k)$ entre
    la diferencia. El factor de escalado es el mayor de los dos $s' + 1$,
    porque $"Pxx"(k)$ se divide entre $2^(s'-s) dot.c 2$, luego su factor de
    escalado será $s' - s + s + 1 = s' + 1$.

  - Si $s > s'$, es el caso contrario $"Pxx"(k)$ se debe dividir entre dos para
    que esté en $[0, 0.5)$, y, como está dividido entre una potencia más grande
    que $N(k)$, hay que dividir este último para que tenga el mismo factor de
    escalado.

- *Paso 8*: El valor $"Pxx"(k)/(2^(e_i)) in [0, 0.5)$ y $N(k)/(2^(e_r)) in [0,
  0.5)$, porque $e_i > 0$. Así que el nuevo valor de $"Pxx"(k)$ estará en
  $[0, 1)$ y por consiguiente se mantiene la invariante del bucle.

- *Paso 9*: Se aplica el factor de reescalado y se divide por los valores entre
  los que se dividía antes para obtener el resultado definitivo. Para este paso
  hay que asegurarse de que tipo del resultado pueda representarlo.

Hay que tener en cuenta que en el paso *paso 9*, el resultado está uniformizado
dos veces, porque en el *paso 6* se hizo el cuadrado y por consiguiente el
factor de uniformización se cuadró.



/* ==== D E N S I D A D _ E S P E C T R A L _ D E _ P O T E N C I A ======== */
=== Densidad espectral de potencia (PSD)
Este algoritmo utiliza la función Welch (@sec:welch) para computarlo. Se trata
de integrar la estimación espectral de potencia utilizando el método de Simpson
(@sec:simpson) en distintos rangos de señales. La densidad integral de potencia
se utiliza como una de las características estadísticas para determinar si una
época es un ataque epiléptico o no.

En el estudio se utilizaron los rangos de 2 a 12 Hz para la que llaman
$"PSD"_1$, el rango de 12 a 18 Hz para la $"PSD"_2$ y el rango 18 a 35 Hz para
la $"PSD"_3$. El rango del vector resultante del método de Welch que se debe
integrar viene dado por la expresión:

  $ (L, H) = "rangos"(l, h) = (floor(l M / s), floor(h M / s)) $

Donde $M$ era el tamaño del marco o la ventan del método de Welch (@sec:welch)
y $s$ es la frecuencia de lectura de la señal (en este caso 256 muestras por
segundo). Luego:

  $ "PSD"(x,l,h) = "unif"^(-1)("unif"^(-1)(
      "simpson"("welch"(x,f:=s,M:=M,R:=M slash 2)))) $

Donde:

  $ "PSD"_1(x) = "PSD"(x,2,12) $
  $ "PSD"_2(x) = "PSD"(x,12,18) $
  $ "PSD"_3(x) = "PSD"(x,18,35) $


/* ==== B A T C H _ N O R M A L I S A T I O N ============================== */
#pagebreak(weak:true)
=== _Batch normalisation_ <sec:batch-normalisation>
Es una técnica de normilización que se usa para mejorar el tiempo y la
estabilidad de entrenamiento de redes de neuronas artificiales, pues recentra
los valores alrededor del cero y los reescala, fue introducido por Sergey Ioffe
and Christian Szegedy en 2015 @IoffeS15.

Dada la media $mu' = mu(v,m) in bb(R)$ y la varianza $sigma^2 = "Var"(v,m)$ de
un vector $v in bb(R)^m$. Se denota la normalización de _batch_ como
$v' = "bnorm"(v,m)$ y se define para como:

  $ v'(i) = "bnorm"(v,m) = (v(i) - mu') / sqrt(sigma^2 + epsilon) $

Donde $0 < epsilon < 1$ es un valor pequeño que se utiliza para dar estabilidad
numérica. Como $sigma^2 >= 0$ y que $epsilon > 0$, $sqrt(sigma^2 + epsilon) >
0$ y por tanto nunca se divide entre cero.

==== Análisis de punto fijo
Solo se encuentran problemas cuando se trabaja con punto fijo, pues la
operación del numerador puede desbordar, la suma dentro de la raíz cuadrada
puede desbordar y la división puede hacer que desborde el resultado. También
hay que trabajar con el cuarto de la varianza, valor que está normalizado dos
veces (ver @sec:energy) y al que se le va a dar como nombre

  $ q = "cuarto_var"(v,m) = "unif"_(b,f) ("unif"_(b,f) (sigma^2 / 4)) $

La expresión queda como:

  $ v'(i) = "bnorm"(v,m) =& ("unif"_(b, f) (v) (i) - mu ("unif"_(b,f) (v, m)))
                         / sqrt(4 q + epsilon) \
    =& ("unif"_(b, f) (v) (i) - mu ("unif"_(b,f) (v, m))) / 2 1 / sqrt(q + epsilon)
  $

Como el numerador está uniformizado y el denominador también lo está (al hacer
la raíz cuadrada, del factor de uniformización $(2^(f+b-1))^2$ da
$sqrt((2^(f+b-1))^2)=2^(f+b-1)$, que es solo un factor de uniformización), el
resultado es escalar y no está uniformizado (se cancelan los factores de
uniformización).

Sea $nu in [0, 1)$,

  $ nu = "unif"_(b, f) (v) (i) div 2 - mu ("unif"_(b,f) (v, m)) div 2 $

El denominador en vez de ser $sqrt(q + epsilon)$, suma que puede desbordar,
es preferible que sea $d in (0, 1)$:

  $ d =  cases(delta_(b,f)     & ", si " sqrt(q) = 0,
               sqrt(q)         & ", si no") $

El $epsilon$ estaba para que el denominador no fuera cero, en el caso en el que
la raíz cuadrada sea cero, el denominador se vuelve el valor más pequeño
representable en el conjunto de punto fijo $bb(X)_(b,f)$, que es
$delta_(b,f) = 2^f$.

En segundo lugar $q in [0, 1)$ porque es uniforme como se ve al final de la
@algorithm-variance-problems, eso quiere decir que $sqrt(q) in [0, 1)$. Además
$d > 0$.

Puesto que por definición la normalización de _batch_ se teóricamenete acerca
los valores alrededor de cero. Para simplificar la demostración se ha decidido
poner un rango finito para el resultado de las expresiones. Pues al dividir
entre un denominador muy pequeño implicaría que el resultado aumenta.

Por razones prácticas se ha decidido que el rango sea $[-B, B]$ con $B = 16$, o
que es lo mismo: $exists b', f': -16, 16 in bb(X)_(b',f')$, por ejemplo:
$bb(X)_(32,-13)$ es válido para almacenar el resultado.

El valor del cociente $nu / d$ siempre va crecer en valor absoluto, porque
$d < 1$, para evitar el desbordamiento es preciso encontrar un predicado que
preferiblemente no desborde:

- Si $nu >= 0$ entonces:

  $ nu / d &>= 0     && #h(1cm) [d > 0 and nu >= 0]      & \
    nu / d &<= B     && #h(1cm) [#text([Postcondición])] & \
    nu &<= B d       && #h(1cm) [d > 0]                  & \
    nu / B &<= d     && #h(1cm) [B > 0]                  & $

  Es decir, si $nu > 0 and nu / B <= d$ entonces $nu / d <= B$.

- Si $nu < 0$ entonces:

  $ nu / d &< 0       && #h(1cm) [d > 0 and nu < 0]       & \
    nu / d &>= -B     && #h(1cm) [#text([Postcondición])] & \
    nu &>= -B d       && #h(1cm) [d > 0]                  & \
    nu / (-B) &<= d   && #h(1cm) [-B < 0]                 & $

  Es decir, si $nu < 0 and nu / (-B) <= d$ entonces $nu / d >= -B$.

El valor del resultado depende del signo y de los valores del numerador y del
denominador:

  $ v'(i) = "bnorm"(v,m) =
      cases(nu / d  &", si " nu >= 0 and nu / B <= d,
            B       &", si " nu >= 0 and nu / B > d ,
            nu / d  &", si " nu < 0  and nu / (-B) <= d,
            -B      &", si " nu < 0 and nu / B > d ) $


/* ==== D E F O R M A C I Ó N _ D I N Á M I C A _ D E L _ T I E M P O ====== */
#pagebreak(weak:true)
=== Deformación dinámica del tiempo (DTW)
Es un algoritmo para medir la similaridad entre dos secuencias temporales que
difieren en la velocidad. El algoritmo está basado en la implementación de
referencia @PPMC-DAC.

Dadas dos señales de $m$ elementos $u in bb(X)_(b'',f'')^m$ y
$v in bb(X)_(b''',f''')^m$. Sean $u', v' in bb(X)_(b,f)^m$ las señales _batch_
normalizadas de acuerdo con la @sec:batch-normalisation como $u'="bnorm"(u,m)$
y $v'="bnorm"(v,m)$. Sea $w in bb(N)^+, w < m$ la ventana de deformación
(_warping window_). El algoritmo se resume en la @fig:algorithm-dtw. Donde
$"máx"(...)$ es la función que retorna el valor máximo de una secuencia de
valores

  $ "máx"(x)         =& x \
    "máx"(x, y, ...) =& cases(x             & ", si " x > "máx"(y, ...),
                              "máx"(y, ...) & ", si no")
  $

$"mín"(...)$ es la función que retorna el mínimo de una secuencia de valores

  $ "mín"(x)         =& x \
    "mín"(x, y, ...) =& cases(x             & ", si " x < "mín"(y, ...),
                              "mín"(y, ...) & ", si no")
  $

$"dist"(a, b)$ es la función de distancia que se define como:

  $ "dist"(a, b) = (a - b)^2 $

y $M$ es un valor arbitrario lo suficientemente grande.

==== Problemas
Al igual que el resto de algoritmos, existen puntos de fallo, especialmente
cuando se indexa. Sin embargo, en este caso algunas soluciones ya se han
aplicado al algoritmo @fig:algorithm-dtw para simplificar la explicación.

- *Paso 2*: La operación $2 w + 3$ puede desbordar
- *Paso 4*: Las sumas pueden desbordar.
- *Paso 6*: Indexar los vectores $u("row")$ y $v("col")$ puede ser fuera de
  rango.
- *Paso 8*: Indexar puede fallar.
- *Paso 9*: Indexar en $"band"_a (i+1)$ puede fallar.
- *Paso 10*: Incrementar cualquiera de los acumulador puede desbordar.
- *Paso 11*: Incrementar el acumulador puede desbordar.
- *Paso 12*: Indexar en $"band"_b (i+1)$ puede fallar.

Además, para punto fijo hay problemas en:

- *Paso 6*: La función $"dist"(a,b)$ también puede desbordar.
- *Paso 8*: Cualquiera de las sumas puede y lo más seguro es que desborde, pues
  ambos vectores se inician con todos elementos a $M$, que es muy grande.


#figure(
  diagram(
    node-stroke: 1pt, {
    let v-sep = 1

    node((1,0), name: <A1>, [*1*: Inicio], shape: shapes.pill)
    edge("-|>")
    node((1,v-sep), align(center)[*2*:\
      $ "size"   <-& 2 w + 3 \
        "band"_a <-& (M, ..., M) in bb(R)^m \
        "band"_b <-& (M, ..., M) in bb(R)^m \
        i        <-& 1                                      \
        "row"    <-& 1 in bb(I)_B                           $
      ], shape: shapes.rect)
    edge("-|>")
    node((1,v-sep*2), name: <rowloop>, align(center)[*3*: ¿$"row" <= m$?],
      shape: shapes.parallelogram)
    edge("-|>", [No])
    node((2,v-sep*2), align(center)[*12*: $"result" <- "band"_b ("index" - 1)$])
    edge("-|>")
    node((2, v-sep*1), align(center)[*13*: Fin], shape: shapes.pill)

    edge(<rowloop.south>, <rowfirst.north>, "-|>", [Sí])
    node((1, v-sep*3), align(center)[*4*:\
      $i     <-& "máx"(1, w - "row" + 2) \
       F     <-& "máx"(1, "row" - w) in bb(I)_B \
       L     <-& "mín"(m, "row" + w) in bb(I)_B \
       "col" <-& L in bb(I)_B            $
    ], name: <rowfirst>, shape: shapes.rect)
    edge("-|>")
    node((1,v-sep*4), name: <colloop>, align(center)[*5*: ¿$"col" <= L$?],
      shape: shapes.parallelogram)
    edge("l,u", "-|>", [#h(1.2em) No])
    node((0,v-sep*3), align(center)[*11*:\
      $"band"_a <->& "band"_b \ "row" <-& "row" + 1$], shape:shapes.rect)
    edge("u,r", "-|>")

    edge(<colloop.south>, <colfirst.north>, "-|>", [Sí])
    node((1,v-sep*5), align(center)[*6*: $d <- "dist"(u("row"), v("col"))$],
         name: <colfirst> , shape:shapes.rect)
    edge("-|>")
    edge(<cond.south>, <band.north>, "-|>", [Sí])
    node((1,v-sep*6), align(center)[*7*: ¿$"col" = 1 and "row" = 1$?],
         shape:shapes.parallelogram, name: <cond>)

    edge("-|>", [No])
    node((0,v-sep*7), align(center)[*8*:\
      $y <-& "band"_a (i) + d \
       x <-& "band"_b (i + 2) + d \
       z <-& "band"_b (i + 1) + d \
       d <-& "mín"(x,y,z) $
    ], shape:shapes.rect)

    edge("-|>")
    node((1,v-sep*7), align(center)[*9*: $"band"_a (i + 1) <- d$],
         name: <band>, shape:shapes.rect)
    edge("-|>")
    node((2,v-sep*7), align(center)[*10*:\ $
        "col" <-& "col" + 1 \
        "i"   <-& "i + 1"
      $],shape:shapes.rect)
    edge("u,u,u,l", "-|>")
  }),
  caption: [Algoritmo de la deformación dinámica del tiempo (DTW)]
) <fig:algorithm-dtw>

==== Soluciones
En primer lugar para que el *paso 2* no desborder en la suma $2 w + 3$, basta
con añadir la precondición:

  $  2 w + 3 <= 2^(B-1) - 1 \
  => 2 w <= 2^(B - 1) - 4   \
  => w <= 2^(B - 2) - 2     $

A continuación en el *paso 4*, pueden desbordar las operaciones:
$(w - "row" + 2)$, $("row" - w)$, $("row" + w)$, eso añade más precondiciones,
se sabe que $"row" in [1, m]$:

  $  & -2^(B-1) <= w - "row" + 2 <= 2^(B-1) - 1 & \
  <=>& -2^(B-1) <= (w + 2) - "row" <= 2^(B-1) - 1
        & #h(1cm) [1 <= w <= 2^(B - 2) => 3 <= (w + 2) <= 2^(B-2)] \
  <=>& -2^(B-1) <= 3 - "row" and 2^(B-2) - "row" <= 2^(B-1) - 1 & \
  <=>& -2^(B-1) <= 3 - 2^(B-1) - 1 and 2^(B-2) - 1 <= 2^(B-1) - 1
        & #h(1cm) [1 <= "row" <= "m" <= 2^(B-1)-1] $

La expresión $(w - "row" + 2)$ no añade ninguna precondición nueva, porque
siempre está en rango y siempre es cierto que $-2^(B-1) - 1 + 3 <= -2^(B-1)$ y
que $2^(B-2) - 1 <= 2^(B-1) - 1$. De igual manera:

  $  & -2^(B-1) <= "row" - w <= 2^(B-1) - 1  & \
  <=>& -2^(B-1) <= 1 - w and 2^(B - 1) - 1 - w <= 2^(B-1) - 1
        & #h(1cm) [1 <= "row" <= m <= 2^(B-1) - 1] \
  <=>& -2^(B-1) <= 1 - 2^(B-2) - 2 and 2^(B - 1) - 1 - 1 <= 2^(B-1) - 1
        & #h(1cm) [1 <= w <= 2^(B - 2) - 2]$

Siempre es cierto y entonces $"row" - w$ no añade ninguna precondición. Y
finalmente:

  $  & -2^(B-1) <= "row" + w <= 2^(B-1) - 1 & \
  <=>& "row" + w <= 2^(B - 1) - 1
        & #h(1cm) ["row" >= 1 and w >= 1] \
  <=>&  m + w <= 2^(B-1) - 1 & #h(1cm) ["row" <= m]$

Que sí añade una precondición adicional, pues dicha operación puede desbordar,
se deduce que:

  $ m + w <= 2^(B-1) - 1 $

El *paso 6* depende de cuál es el máximo y cuál es el mínimo del *paso 4*:

$ i =& cases(1             &", si " w - "row" + 2 <= 1,
             w - "row" + 2 &", si " w - "row" + 2 > 1) \
  F =& cases(1             &", si " "row" - w <= 1,
             "row" - w     &", si " "row" - w > 1) \
  L =& cases(m             &", si " "row" + w >= m,
             "row" + w     &", si " "row" + w < m) \
$

Si se reordenan los las inecuaciones:

$ i =& cases(1             &", si " "row" >= w + 1,
             w - "row" + 2 &", si " "row" < w + 1) \
  F =& cases(1             &", si " "row" <= w + 1,
             "row" - w     &", si " "row" > w + 1) \
  L =& cases(m             &", si " "row" >= m - w,
             "row" + w     &", si " "row" < m - w) \
$

Y si se combinan $i$ y $F$ en la misma expresión condicional:

$
  (i, F) =& cases(
    (w - "row" + 2,& 1)  & ", si " "row" < w + 1,
    (1,& 1)              & ", si " "row" = w + 1,
    (1,& "row" - w)      & ", si " "row" > w + 1)
$

Si también se combina con $L$, añadiendo una *precondición adicional
$m - w > w + 1$*, se puede llegar a que

$
  (i, F, L) =& cases(
    (w - "row" + 2,& 1,& "row" + w) & ", si " "row" < w + 1,
    (1,& 1,& "row" + w)             & ", si " "row" = w + 1,
    (1,& "row" - w,& "row" + w)     & ", si " w + 1 < "row" < m - w,
    (1,& "row" - w,& m)             & ", si " "row" >= m - w)
$

Conocer cuánto valen $i$, $F$ y $L$ en cada iteración nos permite identificar
cuál es el dominio de _col_ en el bucle de iteración más anidado, pues
depende de los valores de $L$ y de $F$:

$
  L - F =& cases(
    "row" + w - 1 & ", si " "row" < w + 1,
    "row" + w - 1 & ", si " "row" = w + 1,
    2 w           & ", si " w + 1 < "row" < m - w,
    m - "row" + w & ", si " "row" >= m - w)
$

Además se puede demostrar que $L - F in [w, 2 w]$, por casos:

#proof[

- *$"row" < w + 1$:* Como $L - F = "row" + w - 1$ y como $1 <= "row" < w$
  $=> 1 + w <= "row" + w < 2 w$ $=> w <= "row" + w - 1 < 2 w - 1$.
- *$"row" = w + 1$:* Como $L - F = "row" + w - 1 = w + w + 1 - 1 = 2 w in [w, 2 w]$
- *$"row" < m - w$:* Como $L - F = 2 w in [w, 2 w]$.
- *$"row" >= m - w$:* Como $L - F = m - "row" + w$ y como $m - w <="row" <= m$
  $=> 0 <= m - "row" <= w $ $=> w <= m - "row" + w <= 2 w$
]

Otro valor que es necesario acotar es $i$ y la expresión $i - F + L$, que es el
valor que tendría $i$ al final del bucle en el *paso 10*. Se puede demostrar
por casos que $i in [1 .. w]$ y que $i + L - F in [w + 1, 2 w + 1]$.

#proof[

- *$"row" < w + 1$*:
  1. $i = w - "row" + 2$, como $1 <= "row" < w + 1$ $=> -1 < w - "row" <= w - 1 $
    $=> 1 < w - "row" + 2 <= w + 1 $. $i in [2, w+1] subset [1, w+1]$
  2. $i + L - F = "row" + w - 1 + w - "row" + 2 = 2 w + 1 in [w + 1, 2 w + 1]$
- *$"row" = w + 1$*:
  1. $i = w - "row" + 2 = w - (w + 1) + 2 = 1 in [1, w + 1]$.
  2. $i + L - F = 1 + "row" + w - 1 = 2 w + 1 in [w + 1, 2 w + 1]$.
- *$"row" < m - w$*:
  1. $i = 1 in [1, w + 1]$
  2. $i + L - F = 2 w + 1 in [w + 1, 2 w + 1]$
- *$"row" >= m - w$*:
  1. $i = 1 in [1, w + 1]$
  2. $i + L - F = 1 + m - "row" + w$, como se demostró antes cuando
     $"row" >= m - w$ entonces $w <= L - F <= 2 w$ luego
     $1 + w <= 1 + L - F <= 2 w + 1$ y $i + L - F in [w + 1, 2 w + 1]$.
]

De aquí se obtienen tres propiedades esenciales:

- El bucle itera $L - F + 1 in [w + 1, 2 w + 1]$ veces
- $i in [1, 2 w + 1]$ en cualquier punto del bucle
- $"col" in [1, m]$, porque $F = "máx"(1, "row" - w)$ que es el índice inferior
  y se minimiza cuando $F = 1$; y $L = "mín"(m, "row" + w)$ que es el índice
  superior que maximiza cuando $L = m$.

El problema del *paso 6* se soluciona automáticamente porque $"row" in [1, m]$
y $"col" in [1, m]$, ya que $u$ y $v$ son vectores de $m$ elementos.

El problema del *paso 9* se soluciona porque $i in [1, 2 w + 1]$ y las bandas
son vectores de $2 w + 3$ elementos e $i + 1 in [2, 2 w + 2]$. De igual manera
se soluciona el *paso 8* y el *paso 12*.

El desbordamiento *paso 10* y el *paso 11* se pueden solucionar si se pone la
restricción de que $m < 2^(B-1) - 1$.

Es preciso tener en cuenta que los valores en las bandas $"band"_a$ y
$"band"_b$ son estrictamente positivos:

#proof[

  Se quiere demostrar que
  $"band"_a (k) >= 0 and "band"_b (k) >= 0, forall k in 1, 2, ... m$.

- *Paso 2*: Como $"band"_a (k) = M and "band"_b (k) = M, forall k in 1, 2, ... m$
  y como $M > 0$, se ve que es cierto.
- *Paso 3*: En la primera iteración es cierto, es necesario demostrar que para
  una iteración arbitraria seguirá siendo cierto suponiendo que es cierto en la
  iteración previa (inducción):

  - *Paso 4*: Irrelevante.
  - *Paso 5*: En la primera iteración es cierto suponiendo que es cierto en el
    *paso 3*, es necesario demostrar que para una iteración arbitraria se
    mantiene la condición suponiendo que la iteración anterior es cierta
    (inducción):

    - *Paso 6*: $d >= 0$, es no negativo.
    - *Paso 7*: $d >= 0$
    - *Paso 8*: Como $d >= 0$ y $"band"_a (k) = M and "band"_b (k) = M,
      forall k in 1, 2, ... m$ entonces $y >= 0$, $x >= 0$ y $z >= 0$ y por lo
      tanto $"mín"(x, y, z) >= 0$.
    - *Paso 9*: $d >= 0$ independientemente de si ha pasado por el *paso 8* y
      por lo tanto $"band"_a (i + 1) >= 0$.
    - *Paso 10*: Irrelevante.

    Si suponiendo que
    $"band"_a (k) >= 0 and "band"_b (k) >= 0, forall k in 1, 2, ... m$, al
    final del bucle se sigue manteniendo la condición, como en la primera
    iteración también se cumple, por inducción se
    demuestra que el bucle más anidado mantiene la invariante de que:
    $"band"_a (k) >= 0 and "band"_b (k) >= 0, forall k in 1, 2, ... m$.

  - *Paso 11*: Intercambia el valor de $"band"_a$ y $"band"_b$, se sigue
    manteniendo el predicado de que sean todos sus elementos positivos.

  Como en la primera iteración es cierto que
  $"band"_a (k) >= 0 and "band"_b (k) >= 0, forall k in 1, 2, ... m$ y para
  una iteración arbitraria suponiendo que es cierto 
  $"band"_a (k) >= 0 and "band"_b (k) >= 0, forall k in 1, 2, ... m$ se sigue
  manteniendo la invariante al final del bucle, por inducción se demuestra
  que
  $"band"_a (k) >= 0 and "band"_b (k) >= 0, forall k in 1, 2, ... m$ es una
  invariante de los pasos 2 al 11 (todos incluidos).

]

Finalmente, queda solucionar los problemas de punto fijo. Para ello, en el
*paso 8* se ha decidido utilizar suma saturada, que se define solo para números
positivos pues en el como se ha visto por inducción una invariante del bucle es
que $"band"_a (k) >= 0 and "band"_b (k) >= 0, forall k in 1, 2, ... m$; y
también se puede ver que
$"dist"(a, b) >= 0, forall a in bb(R), forall b in bb(R)$:

  $ "sat_sum"_(b,f): bb(X)_(b,f) inter [0, infinity) -> bb(X)_(b,f) inter [0, infinity)
                    -> bb(X)_(b,f) inter [0, infinity) $
  $ "sat_sum"_(b,f) (a, b) = cases(2^(b-1)-1 & ", si" a > 2^(b-1) - 1 - b,
                                   a + b     & ", si no") $

La función $"sat_sum"_(b,f) (a, b)$ lo que hace si la suma
$a + b in bb(X)_(b,f)$ entonces retorna $a + b$, si no retorna el valor más
grande del conjunto que es $2^(b-1) 2^f$. De esta manera es posible evitar
desbordamientos. Se sustituyen las sumas del *paso 8* por sumas saturadas.

En el *paso 6*, para la función distancia se podría utilizar el producto
saturado, o bien, como se ha hecho en este proyecto trabajar sobre un conjunto
$bb(X)_(b,f)$ de manera que el cuadrado del valor máximo y mínimo del conjunto
del tipo del resultado de la función _batch normalisation_ sea contenido en él.
