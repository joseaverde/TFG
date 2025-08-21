#import "layout/lib.typ": conf, azuluc3m

// Glosario
#import "@preview/glossarium:0.5.8": make-glossary, register-glossary, print-glossary, gls, glspl
#show: make-glossary
#import "./glosario.typ": entry-list
#register-glossary(entry-list)

#show: conf.with(
  title: "Procesamiento de señales de encefalograma para la detección de ataques epilépticos.",
  author: "José Antonio Verde Jiménez",
  degree: "Grado en Ingeniería Informática",
  advisors: ("José Daniel García Sánchez",),
  location: "Leganés, España,",
  thesis-type: "TFG",
  date: datetime(year: 2025, month: 9, day: 8),
  bibliography-file: "../referencias.bib",
  bibliography-style: "ieee",
  language: "es",
  style: "fancy",
  double-sided: false,
  logo: "new",
  short-title: "Detección de ataques epilépticos",
  date-format: "[day padding:none] de septiembre de [year]",  // [month repr:long]
  license: true,
  flyleaf: true,
  epigraph: (
    quote: [Beware of bugs in the above code;\
            I have only proved it correct,\
            not tried it.],
    author: "Donald E. Knuth"),
  abstract: (
    body: [
      // TODO
      *TODO*
    ],
    keywords: ("Optimización", "Epilepsia", "Tiempo real")),
  english-abstract: (
    body: [
      // TODO
      *TODO*
    ],
    keywords: ("Optimisation", "Seizure detection", "Real time")),
  acknowledgements: [
    *TODO*
    // TODO
    // José Daniel García Sánchez
    // Rafael Asenjo Plaza, María Ángeles González Navarro y Felipe Muñoz López
    // Álvaro Guerrero Espinosa, Lucía, Diego, Santiago y Daniel
    // Luisda, Jorge Lázaro, Edu
    // Amigos y familia
  ],
  outlines: (figures: true, tables: true, listings: true),
  appendixes: none,
  glossary: print-glossary(entry-list, show-all: true),
  abbreviations: none) // TODO

#set text(
  lang:   "es",
  region: "es")

#import "@preview/codly:1.3.0": *
#import "@preview/codly-languages:0.1.1": *
#show: codly-init.with()

#codly(
  languages: (
    ada: (name: "Ada", color: rgb("#CE412B")),
    adb: (name: "SPARK", color: rgb("#CE412B")),
  )
)

#set list(marker: ([•], [--]), indent: 1em)
 
#show figure: set figure.caption(position: top)
#show figure.caption: it => [
  #set text(azuluc3m, weight: "semibold")
  #set par(first-line-indent: 0em, justify: false)
  #context smallcaps(it.supplement)
  #context smallcaps(it.counter.display(it.numbering))
  #set text(black, weight: "regular")
  #set align(center)
  #smallcaps(it.body)
]

#show table: block.with(stroke: (y: 0.7pt))
#set table(
  row-gutter: -0.1em,   // Row separation
  stroke: (_, y) => if y == 0 { (bottom: 0.2pt) }
)

#include "1-introducción.typ"
#pagebreak();
#pagebreak();
#include "2-estado-del-arte.typ"
#pagebreak();
#pagebreak();
#include "3-análisis.typ"
#pagebreak();
#pagebreak();
#include "4-diseño-e-implementación.typ"
#pagebreak();
#pagebreak();
#include "5-validación-verificación-y-evaluación.typ"
#pagebreak();
#pagebreak();
#include "6-marco-regulador.typ"
#pagebreak();
#pagebreak();
#include "7-entorno-socio-económico-y-ods.typ"
#pagebreak();
#pagebreak();
#include "8-planificación.typ"
#pagebreak();
#pagebreak();
#include "9-conclusiones-y-trabajo-futuro.typ"
#pagebreak();
