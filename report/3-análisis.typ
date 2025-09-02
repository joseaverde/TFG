#import "./requisitos.typ" : config, srs, reqs, reference

= Análisis <sec:3>
En este capítulo se da una descripción general del problema
(@sec:3-plantemiento), los casos de uso (@sec:3-casos-de-uso),
los requisitos del mismo (@sec:3-requisitos) y el análisis de los mismos
(@sec:3-análisis-de-requisitos). Finalmente, se resume la arquitectura del
sistema (@sec:3-arquitectura).

== Planteamiento del problema <sec:3-plantemiento>
El objetivo del proyecto es implementar el algoritmo para la detección de
ataques epilépticos que utiliza los patrones y las características de una señal
encefalograma para clasificar segmentos (épocas) de una señal desarrollado en
la Universidad de Málaga @PaFESD, de forma que ejecute en tiempo real en
sistemas empotrados de bajo consumo energético, además de optimizar el código
preexistente para reducir el tiempo de entrenamiento.

El proceso se separa en dos fases: una primera fase de entrenamiento, que genera
un _batch_ (modelo) con patrones relevantes e intervalos de los valores de las
características de la señal que define (distancia máxima, energía y densidad
espectral de potencia) que optimiza la puntuación $F_1$ ($F_1$ _score_) del
modelo; y una segunda fase, en la que un paciente lleva un dispositivo empotrado
conectado a un sensor de señales de encefalograma, que hace uso del modelo
generado previamente para clasificar la época actual de la señal en «zona de
ataque epiléptico» (positivo) o «zona libre de ataque epiléptico» (negativo).

Para el desarrollo se ha definido que un _stride_ (número de muestras por
segundo) son 256 muestras, que una época son cinco _strides_ o 1280 muestras y
que el _batch_ tiene a lo sumo tres patrones con que compararlo. Para
considerar que el sistema ejecute en tiempo real (todo aquel sistema capaz de
garantizar una respuesta antes de un tiempo límite) debe ser capaz de procesar
en el peor de los casos una época por segundo.

Existe ya una implementación @PPMC-DAC que solamente entrena el modelo. Además
de implementar el _software_ para el sistema empotrado, también hay que mejorar
el tiempo de entrenamiento de dicha implementación, que comparte la función de
detección con el dispositivo empotrado, pero este solo la usa para optimizar
el modelo.

== Casos de uso <sec:3-casos-de-uso>
De acuerdo con Craig Larman, _Unified Process_ define el modelo de casos de uso
dentro de la disciplina de requisitos e insiste en que los casos de uso son
documentos textuales, no diagramas, y que el modelado de casos de uso es
principalmente un acto de escribir texto, no de dibujar diagramas @LarmanUML. 

El mismo autor define «actor» como todo aquello con comportamiento, eso incluye
el propio sistema cuando este hace uso de otros servicios o sistemas. Los
actores no solo son roles que interpretan personas, también organizaciones,
_software_ y máquinas. Hay tres tipos de actores externos: actor principal,
actor de apoyo (_supporting actor_) y actor entre bastidores (_offstage
actor_). @LarmanUML

En este proyecto solo tiene sentido de hablar de dos actores: que son el
paciente, que quiere detectar sus propios ataques epilépticos; y el doctor,
que puede ser un médico o un investigador que se encarga de entrenar el modelo
para posteriormente detectar ataques epilépticos.

#srs.show-template(reqs, srs.make-tag("CU"), "cu-template")

#srs.show-items(
  reqs,
  srs.make-tag("CU"),
  // custom formatter bc use cases are ✨special✨
  formatter: srs.defaults.table-item-formatter-maker(
    style: (columns: (8em, 1fr), align: left),
    breakable: true,
  ),
)

#figure(
  image("uml/casos-de-uso.svg", width: 70%),
  caption: [Modelo de casos de uso]
)

== Requisitos <sec:3-requisitos>
En esta sección se provee la lista de requistios que se ha obtenido a partir de
un análisis exhaustivo de los casos de uso, definidos previamente en la
@sec:3-casos-de-uso. Los requisitos a continuación se clasifican en dos grandes
categorías:

- *Requisitos funcionales*: se dice de aquellos requisitos que prescriben el
  comportamiento del sistema. Responden a la pregunta: _«¿qué debe hacer?»_.
- *Requisitos no funcionales*: se dice de aquellos requisitos que imponen
  restricciones sobre cómo debe implementarse el sistema. Responden a la
  pregunta: _«¿cómo debe hacerlo?»_.

Cada requisito se identifica de manera unequívoca con un identificador con el
formato:

- *Requisitos funcionales*: `RF-XX`, donde `XX` es un valor numérico de dos
  cifras, que comienza en `01` y que crece monótonamente de uno en uno.
- *Requisitos no funcionales*: `RNF-XX`, donde `XX` es un valor numérico de dos
  cifras, que comienza en `01` y que crece monótonamente de uno en uno.

La @srs:rf-template muestra el formato de los requisitos
funcionales y la @srs:rnf-template de los no funcionales.

=== Requisitos funcionales
#srs.show-template(reqs, srs.make-tag("R", "F"), "rf-template")
#srs.show-items(reqs, srs.make-tag("R", "F"))

=== Requisitos no funcionales
#srs.show-template(reqs, srs.make-tag("R", "N"), "rnf-template")
#srs.show-items(reqs, srs.make-tag("R", "N"))

== Análisis de requisitos <sec:3-análisis-de-requisitos>
Finalmente, las siguientes dos matrices de trazabilidad nos permiten ver la
relación de cada requisito con cada caso de uso para determinar la cobertura y
el grado de dependencia entre ambos. La primera (@srs:R-F-traceability) relaciona los
requisitos funcionales con los casos de uso, las segunda (@srs:R-N-traceability)
relaciona los requisitos no funcionales con los casos de uso. Se puede ver
que todos los requisitos cubren todos los casos de uso, y no hay requisitos sin
relación con ningún caso de uso.

#srs.show-traceability(reqs, srs.make-tag("R", "F"))
#srs.show-traceability(reqs, srs.make-tag("R", "N"))

== Arquitectura <sec:3-arquitectura>
La arquitectura se del sistema se puede separar en cuatro componentes
principales:

- *Lector* (@srs:c-lector): que lee una señal de encefalograma de un sensor.
- *Detector* (@srs:c-detector): que utiliza un _batch_ (o modelo) generado por
  el #reference("c-entrenador", [entrenador]) para clasificar la señal
  generada por el #reference("c-lector", [lector]).
- *Entrenador* (@srs:c-entrenador): que genera un modelo a partir de los datos de
  un paciente.
- *Validador* (@srs:c-validador): que es utilizado por el
  #reference("c-entrenador", [entrenador]) para optimizar el modelo.

El diagrama UML se resume en la @arquitectura, donde se pueden ver los
distintos componentes y su relación entre ellos.

#srs.show-template(reqs, srs.make-tag("C"), "comp-template")

#srs.show-items(
  reqs,
  srs.make-tag("C"),
  formatter: srs.defaults.table-item-formatter-maker(
    style: (columns: (8em, 1fr), align: left),
    breakable: false,
  ),
)

#[
  #set page(flipped: true)
  #set align(horizon)
  #figure(
    image("uml/arquitectura.svg", width: 100%),
    caption: [Arquitectura]
  ) <arquitectura>
]
