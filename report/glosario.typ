#import "layout/lib.typ": azuluc3m

#let entries = (
  (
    name: [AArch64],
    desc: [Nombre que recibe la extensión de 64 bits del juego de instrucciones ARM]
  ),
  (
    name: [algoritmo],
    desc: [Serie de instrucciones que se aplican a unos datos de entrada para
           solucionar un problema.]
  ),
  (
    name: [asincronía],
    desc: [En informática, se refiere a eventos que ocurren independientemente
           al flujo principal del programa como señales o interrupciones.]
  ),
  (
    name: [biblioteca],
    desc: [En informática, colección de subrutinas.]
  ),
  (
    name: [bit],
    desc: [Unidad mínima de información.]
  ),
  (
    name: [byte],
    desc: [Grupo de bits, normalmente son octetos de 8 bits.]
  ),
  (
    name: [caché],
    desc: [En informática, estructura que almacena datos para acelerar
           consultas futuras a esos mismos datos.]
  ),
  (
    name: [codificación],
    desc: [En informática, representación de la información en un soporte.]
  ),
  (
    name: [_copyleft_],
    desc: [Técnica legal que permite dar libertades de uso y distribución de
           una obra bajo derechos de autor.]
  ),
  (
    name: [compilador],
    desc: [En informática, programa que traduce código fuente en un lenguaje
           de programación a código máquina, que entiende un computador.]
  ),
  (
    name: [corolario],
    desc: [En matemáticas, proposición que no necesitar ser demostrada que se
           deduce de un teorema demostrado previamente.]
  ),
  (
    name: [desbordar],
    desc: [En informática, se dice de estructura de datos o un valor numérico
           que rebasa el límite fijado.]
  ),
  (
    name: [discreto],
    desc: [En matemáticas, usualmente se refiere a procesos o a cantidades que
           se pueden representar con números enteros: ..., -3, -2, -1, 0, 1, 2,
           ...,]
  ),
  (
    name: [_front-end_],
    desc: [En informática, capa de presentación.]
  ),
  (
    name: [función],
    desc: [En matemáticas es una relación entre dos conjuntos. En informática
           es una subrutina.]
  ),
  (
    name: [indexar],
    desc: [En informática, acción de acceder a un elemento en una colección
           dado su índice.]
  ),
  (
    name: [instanciar],
    desc: [En informática, acción de crear una instancia de un objeto.]
  ),
  (
    name: [iteración],
    desc: [En informática, repetición de un segmento de código.]
  ),
  (
    name: [lema],
    desc: [En matemáticas, proposición que es preciso demostrar antes de probar
           un teorema.]
  ),
  (
    name: [lenguaje de programación],
    desc: [Lenguaje formal que permite describir el comportamiento de un
           programa de computador.]
  ),
  (
    name: [_log_],
    desc: [Del inglés, registro.]
  ),
  (
    name: [megabyte],
    desc: [1 MiB = 1024 bytes]
  ),
  (
    name: [O.D.S.],
    desc: [Objetivos de desarrollo sostenible]
  ),
  (
    name: [procedimiento],
    desc: [En informática, subrutina.]
  ),
  (
    name: [punto fijo],
    desc: [En informática, valor numérico escalado por un factor implícito
           conocido.]
  ),
  (
    name: [punto flotante],
    desc: [En informática, valor numérico que codifica un factor en el propio
           dato.]
  ),
  (
    name: [recursivo],
    desc: [En informática, que se define consigo mismo en la propia definición.]
  ),
  (
    name: [rendimiento],
    desc: [Proporción entre producción y medios utilizados.]
  ),
  (
    name: [retornar],
    desc: [En informática, acción de devolver un valor de una subrutina.]
  ),
  (
    name: [RISC-V],
    desc: [Juego de instrucciones.]
  ),
  (
    name: [sistema empotrado],
    desc: [O sistema empotrado, es un sistema de computación que tiene una
           función dedicada dentro de un sistema más grande.]
  ),
  (
    name: [subdesbordar],
    desc: [Acción de desbordar por el límite inferior.]
  ),
  (
    name: [subrutina],
    desc: [En informática, es una porción de un programa que tiene una interfaz
           y comportamiento bien definidos y que se puede invocar múltiples
           veces.]
  ),
  (
    name: [teorema],
    desc: [Proposición demostrable lógicamente partiendo de axiomas ya
           demostrados.]
  ),
  (
    name: [tiempo real],
    desc: [En informática, se habla de un sistema de información que debe
           responder dentro de unos límites de tiempo.]
  ),
  (
    name: [_toolchain_],
    desc: [Del inglés _tool chain_, es una cadena de herramientas. En
           informática, se suele utilizar para hablar de compiladores.]
  ),
)

#let glosario = []

#{
  for entry in entries {
    glosario = [
      #glosario

      #set par(
        first-line-indent: 0em,
        hanging-indent: 2em,
        justify: true,
      )
      #text(azuluc3m, weight: "bold")[#entry.name ---] #entry.desc
    ]
  }
}
