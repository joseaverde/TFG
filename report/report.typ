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
  date-format: "[day padding:none] de [month repr:long] de [year]",
  license: true,
  flyleaf: true,
  epigraph: (
    quote: [Beware of bugs in the above code;\
            I have only proved it correct,\
            not tried it.],
    // source: "https://www-cs-faculty.stanford.edu/~knuth/faq.html",
    author: "Donald E. Knuth"),
  abstract: (
    body: [
      La epilepsia es una de las enfermedades cerebrales más comunes, la
      detección de ataques epilépticos a partir de señales de encefalograma
      puede tener un impacto positivo en la vida de los pacientes que la
      padecen. Pero para ello es necesario detectarlos a tiempo.

      Esta tesis toma de base un estudio sobre de detección de ataques
      epilépticos utilizando patrones aumentados y las características
      estadísticas de la señal de encefalogama para implementarlo en un sistema
      de bajo consumo en tiempo real, además de diminuir el tiempo de
      entrenamiento del modelo.

      Se hace un estudio sobre el impacto de distintas técnicas de
      programación, del paralelismo, de varios lenguajes de programación
      (Python 3, C++ y Ada) y de utilizar números codificados en punto fijo
      y en punto flotante en el tiempo de ejecución del programa en las
      distintas plataformas.

      También se hace un estudio exhaustivo y riguroso de los algoritmos
      que forman el detector para demostrar formalmente mediante SPARK la
      ausencia de errores en tiempo de ejecución y demostrar las propiedades
      del propio programa.
    ],
    keywords: ("Señales de encefalograma", "Epilepsia", "Rendimiento", "Tiempo real")),
  english-abstract: (
    body: [
      Epilepsy is one of the most common brain-related diseases.
      Electroencephalography-based seizure detection may have a possitive
      impact in patients who suffer it.
      But for that to happen, it is essential to detect them on time.

      This thesis builds upon a study about pattern augmented by features
      epileptic seizure detection,
      as a means to implement it in an energy-efficient real-time system.
      And to reduce training time.

      Different programming techniques, parallelism, various programming
      languages (Python 3, C++ and Ada) and real number encoding (fixed types
      and floating types) are analysed.
      And their performance is measured in different platforms.

      A thorough and strict study about the algorithms that make-up the
      detector is performed as well.
      By using SPARK it is possible to formally prove the absence of run-time
      errors and certain properties about the program itself.
    ],
    keywords: ("EEG Signals", "Seizure detection", "Performance", "Real time")),
  acknowledgements: [
    Me gustaría dedicarle esta tesis a esas personas que me fueron guiando
    desde el inicio y gracias a las cuales, hoy puedo presentar esta tesis. A
    mi tutor, José Daniel García Sánchez; y a los investigadores de la
    Universidad de Málaga con los que cooperamos, que escribieron el artículo
    en el cual se basa este trabajo: a María Ángeles González Navarro, a Felipe
    Muñoz López y especialmente a Rafael Asenjo Plaza, que descanse en paz. A
    vosotros os lo dedico y os agradezco de todo corazón todo el tiempo que me
    han dedicado, los recursos que me han dado y el interés por verlo
    completado. Muchas gracias.

    Asimismo, agradezco a mis compañeros del laboratorio de ARCOS de la
    Universidad Carlos III de Madrid, por haber estado a mi lado y por haberme
    ayudado cuando lo necesitaba. A Santiago, por haberme ayudado con partes
    más técnicas del desarrollo, por haberse quedado conmigo horas y horas
    depurando el programa. Gracias a él funciona mi `CMakeFiles.txt`. A Álvaro
    Guerrero y a Lucía por haberme ayudado con varias demostraciones y a
    depurar y discutir distintas soluciones para varios problemas. Y al resto
    de compañeros del laboratorio de ARCOS: a Diego, a Álvaro, a Daniel y a
    Elisa. Muchas gracias.

    Igualmente, quiero dar las gracias a Luisda, a Jorge Lázaro y a Eduardo
    Alarcón por sus contribuciones para preparar y mejorar la plantilla de este
    mismo documento. Muchas Gracias.

    Finalmente, a esas personas que me han estado apoyando desde el principio,
    en mis peores y en mis mejores momentos, que siempre han estado ahí cuando
    más lo necesitaba. A vosotros os lo dirijo: a mi familia y a mis amigos.
    Muchas gracias.
  ],
  outlines: (figures: true, tables: true, listings: true),
  // appendixes: [],
  genai-declaration: [
    #set align(center)
    *DECLARACIÓN DE USO DE IA GENERATIVA EN EL TRABAJO DE FIN DE GRADO* \

    #set align(left)
    El autor de esta tesis *no* ha usado ningún tipo de inteligencia artificial
    generativa de ningún tipo: ni durante el desarrollo del proyecto ni durante
    la redacción de este documento. No considera que sea útil.
  ],
  glossary: print-glossary(entry-list, show-all: true),
  abbreviations: none)

#set text(
  lang:   "es",
  region: "es")

#import "@preview/codly:1.3.0": *
#import "@preview/codly-languages:0.1.1": *
#show: codly-init.with()

#codly(
  languages: (
    ada: (name: "Ada", color: rgb("#005a00")),
    adb: (name: "SPARK", color: rgb("#c69ed3")),
    cpp: (name: "C++", color: rgb("#659bd3")),
    python: (name: "Python 3", color: rgb("#ffd947")),
    lean: (name: "Lean", color: rgb("#6ba4ff")),
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


