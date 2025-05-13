#import "@preview/cetz:0.3.4"
#import cetz.draw: *
#import "utility.typ": *

= Estado del Arte
== PaFESD
== Representación de números reales
=== Números reales


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



== Procesadores empotrados
== Rangos
== Programación paralela
