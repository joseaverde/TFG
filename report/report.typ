#import "layout/lib.typ": conf, azuluc3m

#show: conf.with(
  degree: "Grado en Ingeniería Informática",
  title: "Procesamiento de señales de encefalograma para la detección de ataques epilépticos.",
  author: "José Antonio Verde Jiménez",
  advisors: ("José Daniel García Sánchez",),
  place: "Leganés",
  bibliography_file: "../referencias.bib",
  date: (2024, 2025),
  toc: true,
  logo: "new",
  shortitle: "Detección de ataques epilépticos",
  chapter_on_new_page: true,
  language: "es",
)

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
