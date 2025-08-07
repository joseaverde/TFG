#import "layout/lib.typ": conf

#show: conf.with(
  degree: "Ingeniería Informática",
  title: "Procesamiento de señales de encefalograma para la detección de ataques epilépticos.",
  author: "José Antonio Verde Jiménez",
  advisors: ("José Daniel García Sánchez",),
  place: "Leganés",
  // bibliography_file: "referencias.bib",
  date: datetime.today,
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

#include "1-introducción.typ"
#include "2-estado-del-arte.typ"
#include "3-análisis.typ"
#include "4-diseño.typ"
#include "5-implementación-y-despliegue.typ"
#include "6-validación-verificación-y-evaluación.typ"
#include "7-planificación-y-análisis-de-costes.typ"
#include "8-marco-regulatorio.typ"
#include "9-conclusiones-y-trabajo-futuro.typ"

#bibliography("referencias.bib")
