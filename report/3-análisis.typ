#import "srs/lib.typ" as srs

#let scale-m-type = srs.make-enum-field(
  h: "Alto",
  m: "Medio",
  l: "Bajo")

#let scale-f-type = srs.make-enum-field(
  h: "Alta",
  m: "Media",
  l: "Baja")

#let stability-type = srs.make-enum-field(
  c: "Constante",
  i: "Inconstante",
  vu: "Inestable")

#let source-type = srs.make-enum-field(
  c: "Cliente",
  a: "Analista")

#let config = srs.make-config(
  /* *** R e q u i s i t o s *** --------------------------------------- *** */
  srs.make-class(
    "R",
    "Requisito",
    fields: (
      /* Requisito :: Descripción */
      srs.make-field(
        "Descripción",
        srs.content-field,
        [Descripción detallada del requisito]),

      /* Requisito :: Necesidad */
      srs.make-field(
        "Necesidad",
        scale-f-type,
        [Prioridad del requisito para el usuario]),

      /* Requisito :: Prioridad */
      srs.make-field(
        "Prioridad",
        scale-f-type,
        [Prioridad del requisito para el desarrollador]),

      /* Requisito :: Estabilidad */
      srs.make-field(
        "Estabilidad",
        stability-type,
        [Indica la variabilidad del requisito a lo largo del proceso de
        desarrollo.]),

      /* Requisito :: Verificabilidad */
      srs.make-field(
        "Verificabilidad",
        scale-f-type,
        [Capacidad de probar la valided del requisito.]),

      /* Requisito :: Fuente */
      srs.make-field(
        "Fuente",
        source-type,
        [Quién ha propuesto el requisito.]),
    ),
    classes: (
      srs.make-class("F", "Requisito funcionales",
        origins: srs.make-origins(
          [Caso de uso del que es origen este requisito.],
          srs.make-tag("CU"))),
      srs.make-class("N", "Requisito no funcional",
        origins: srs.make-origins(
          [Caso de uso del que es origen este requisito.],
          srs.make-tag("CU")))),
  ),
  /* *** C a s o s   d e   u s o *** ----------------------------------- *** */
  srs.make-class(
    "CU",
    "Caso de uso",
    fields: (
      /* Caso de uso :: Nombre */
      srs.make-field(
        "Nombre",
        srs.content-field,
        [Nombre del caso de uso, es una acción así que debe empezar con un
         verbo.],),

      /* Caso de uso :: Alcance */
      srs.make-field(
        "Alcance",
        srs.make-enum-field(
          empotrado:     [Dispositivo empotrado],
          entrenamiento: [Entrenamiento],
          todo:          [Dispositivo empotrado y entrenamiento]),
        [A qué subsistema o subsistemas específicos afecta el mismo. En
         este proyecto se diferencia la fase de entrenamiento del dispositivo
         empotrado con el sistema en tiempo real.]),

      /* Caso de uso :: Nivel */
      srs.make-field(
        "Nivel",
        srs.make-enum-field(
          meta-de-usuario : [Meta de usuario],
          subfunción      : [Subfunción]),
        [«Meta de usuario» o «Subfunción». La subfunción se diferencia de la
         meta de usuario en que es un paso intermedio.]),

      /* Caso de uso :: Actores principales */
      srs.make-field(
        "Actores principales",
        srs.content-field,
        [Los agentes que hacen uso de los servicios del sistema para alcanzar
         su objetivo.]),

      /* Caso de uso :: Parte interesada */
      srs.make-field(
        "Parte interesada",
        srs.content-field,
        [Son los actores a quienes les concierne y qué quieren.]),

      /* Caso de uso :: Precondiciones */
      srs.make-field(
        "Precondiciones",
        srs.content-field,
        [Qué debe ser cierto al inicio.]),

      /* Caso de uso :: Postcondiciones */
      srs.make-field(
        "Postcondiciones",
        srs.content-field,
        [Qué se garantiza que es cierto al completar la operación.]),

      /* Caso de uso :: Escenario de éxito principal */
      srs.make-field(
        "Escenario de éxito principal",
        srs.content-field,
        [La descrición de los pasos de las situación más típica.]),

      /* Caso de uso :: Extensiones */
      srs.make-field(
        "Extensiones",
        srs.content-field,
        [Escenarios alternativos y ramificaciones del escenario principal.]),

      /* Caso de uso :: Requisitos especiales */
      srs.make-field(
        "Requisitos especiales",
        srs.content-field,
        [Requisitos no funcionales relacionados.]),

      /* Caso de uso :: Frecuencia con que ocurre */
      srs.make-field(
        "Frecuencia con que ocurre",
        srs.content-field,
        [Cada cuánto tiempo se espera que se utilice el caso de uso.]),
    ),
  ),
//srs.make-class(
//  "C",
//  "Component",
//  fields: (
//    srs.make-field(
//      "Role",
//      srs.content-field,
//      [Component's function in the system],
//    ),
//    srs.make-field(
//      "Dependencies",
//      srs.content-field,
//      [Components that depe],
//    ),
//    srs.make-field(
//      "Description",
//      srs.content-field,
//      [Explanation of the component's functionality],
//    ),
//    srs.make-field(
//      "Inputs",
//      srs.content-field,
//      [Component's input data],
//    ),
//    srs.make-field(
//      "Outputs",
//      srs.content-field,
//      [Component's output data],
//    ),
//  ),
//),
//srs.make-class(
//  "T",
//  "Test",
//  fields: (
//    srs.make-field(
//      "Description",
//      srs.content-field,
//      [Test description],
//    ),
//    srs.make-field(
//      "Preconditions",
//      srs.content-field,
//      [Conditions that must be fulfilled in order to perform the test],
//    ),
//    srs.make-field(
//      "Postcondition",
//      srs.content-field,
//      [Conditions that must be fulfilled after performing the test in order to
//        pass],
//    ),
//    srs.make-field(
//      "Evaluation",
//      srs.make-enum-field(ok: "OK", err: "Error"),
//      [Result of the test],
//    ),
//  ),
//  classes: (
//    srs.make-class(
//      "VAT",
//      "Prueba de validación",
//      origins: srs.make-origins(
//        [Requirement that originated this test.],
//        srs.make-tag("R", "S", "FN"),
//        srs.make-tag("R", "S", "NF"),
//      ),
//    ),
//  ),
//),
//srs.make-class(
//  "VET",
//  "Prueba de verificación",
//  origins: srs.make-origins(
//    [Requirement that originated this test.],
//    srs.make-tag("R", "U", "CA"),
//    srs.make-tag("R", "U", "RE"),
//  ),
//),
)

#let make-item(id, class, fields) = { srs.make-item(id, class, ..fields) }

#let reqs = srs.create(
  config: config,

  /* *** C a s o s   d e   u s o *** ----------------------------------- *** */

  // Caso de uso : Leer señal de encefalograma
  make-item("uc-sample", srs.make-tag("CU"), (
    Nombre: [Leer señal de encefalograma],
    Alcance: "empotrado",
    Nivel: "subfunción",
    "Actores principales": [],
    "Parte interesada": [
      - Paciente: Quiere que la señal de encefalograma para detectar un posible
        ataque epiléptico sea continua y no se detenga bajo ningún concepto.
      - Doctor: Quiere poder utilizar tanto señales reales como señales
        pregrabadas como señales sintéticas para hacer estudios y entrenar el
        modelo.],
    Precondiciones: [
      El sensor tiene alimentación y se ha activado por _software_.],
    Postcondiciones: [
      El resultado es un valor real que está dentro de un rango válido.],
    "Escenario de éxito principal": [
      1. El sensor mide un valor analógico en bruto en un instante.
      2. El sensor convierte dicho valor a otro con las unidades esperadas.
      3. El sensor escribe el valor en la señal.],
    Extensiones: [
      1. Desconexión del sensor
         1. El sensor notifica al paciente
         2. El sensor escribe el valor máximo del rango válido, para no detener
            el sistema ni dejar esperando al resto de actores.
      2. Si el valor después de convertirlo fuera mayor que el límite superior
         del rango válido.
         1. El sensor escribe un _log_.
         2. El sensor escribe el valor máximo del rango válido y continúa con
            la operación.
      3. Si el valor después de convertirlo fuera menor que el límite inferior
         del rango válido.
         1. El sensor escribe un _log_.
         2. El sensor escribe el valor mínimo del rango válido y continúa con
            la operación.],
    "Requisitos especiales": [
      - Debe leer 256 muestras por segundo.
      - Debe leer a un ritmo constante.
      - Los valores leídos deben estar en un rango válido.],
    "Frecuencia con que ocurre": [Continuo])),

  // Caso de uso : Detectar ataque
  make-item("uc-detect", srs.make-tag("CU"), (
    Nombre: [Detectar ataque],
    Alcance: "todo",
    Nivel: "meta-de-usuario",
    "Actores principales": [Paciente],
    "Parte interesada": [
      - Detector: Quiere clasificar las distintas épocas de la señal para
        entrenar el modelo o hacer estudios..
      - Paciente: Quiere saber si está teniendo un ataque epiléptico.],
    Precondiciones: [
      El modelo debe estar cargado, y el sensor debe estar activo.],
    Postcondiciones: [Ninguna],
    "Escenario de éxito principal": [
      1. El detector espera a tener una época de señal del sensor.
      2. El detector pasa un filtro de paso bajo y paso alto a la época.
      3. El detector decide que la época no es un artefacto (pestañeo, ...).
      4. El detector computa las características de la señal (`max_distance`,
         ...).
      5. El detector computa la distancia entre la época y cada uno de los
         patrones.
      6. Retorna que es un ataque epiléptico.],
    Extensiones: [
      1. En el paso 3., si se trata de un artefacto.
         1. Como es un artefacto, no hace falta continuar, retorna que no es un
             ataque.
      2. En el paso 4. si para alguna de las características el valor cae fuera
         de los rangos definidos por el modelo (_batch_).
         1. No es un ataque epiléptico, retorna que no lo es.
      3. En el paso 5. si para ninguno de los patrones la distancia es lo
         suficientemente pequeña (determinada por el modelo).
         1. No es un ataque epiléptico, lo retorna.],
    "Requisitos especiales": [
      - Debe procesar una época y compararla hasta con tres patrones por
        segundo.],
    "Frecuencia con que ocurre": [Cotinua (una vez por segundo)])),

  // Caso de uso : Entrenar modelo
  make-item("uc-train", srs.make-tag("CU"), (
    Nombre: [Entrenar el modelo],
    Alcance: "entrenamiento",
    Nivel: "meta-de-usuario",
    "Actores principales": [Doctor],
    "Parte interesada": [
      - Doctor: Quiere obtener el modelo y su eficacia.
      - Paciente: Quiere un modelo eficaz para detectar sus posibles ataques
        epilépticos.],
    Precondiciones: [
      Una señal continua con los intervalos de ataque epiléptico etiquetados.],
    Postcondiciones: [
      Un modelo válido con los rangos de características de la señal y los
      patrones de la propia señal que optimicen la puntuación $F_1$ del
      sistema. Además de la precisión, la sensibilidad y su puntuación $F_1$.],
    "Escenario de éxito principal": [
      1. El doctor graba la señal de un paciente durante un periodo largo de  
         tiempo.
      2. El doctor etiqueta los intervalos de la propia señal en los que está
         ocurriendo un ataque.
      3. El sensor pasa un filtro de paso bajo y paso alto a la señal.
      4. El detector marca en la señal las secciones con artefactos.
      5. El optimizador (externo) toma el detector y optimiza los parámetros
         del modelo para maximizar la puntuación $F_1$.
      6. El optimizador retorna el modelo preparado en un archivo.],
    Extensiones: [
      1. Puntuación $F_1$ baja.
         1. El doctor estudiará por qué ocurre.
         2. El doctor reconfigura el optimizador y vuelve a intentar generar el
            modelo.],
    "Requisitos especiales": [
      - La puntuación $F_1$ debe ser superior al $99%$.
    ],
    "Frecuencia con que ocurre": [
      Escasa: el modelo se entrene una sola vez por paciente y tarda mucho en
      hacerlo])),


  /* *** R e q u i s i t o s *** --------------------------------------- *** */

  /* *** Requisitos funcionales ---------------------------------------- *** */

  /* Plantilla v{y

  make-item("", srs.make-tag("R", "F"), (
    Descripción: [],
    Necesidad: "h/m/l",
    Prioridad: "h/m/l",
    Estabilidad: "c/i/vu",
    Verificabilidad: "h/m/l",
    Fuente: "a/c")),

  */

  make-item("rf-binding", srs.make-tag("R", "F"), (
    Descripción: [
      Debe existir una interfaz con Python 3 (_binding_) en forma de módulo,
      que se integre con la base de código antigua.],
    Necesidad: "m",
    Prioridad: "l",
    Estabilidad: "c",
    Verificabilidad: "h",
    Fuente: "c")),

  make-item("rf-classify", srs.make-tag("R", "F"), (
    Descripción: [
      El sistema debe clasificar una época dada en ataque o no ataque.
    ],
    Necesidad: "h",
    Prioridad: "h",
    Estabilidad: "c",
    Verificabilidad: "l",
    Fuente: "c")),

  make-item("rf-notify", srs.make-tag("R", "F"), (
    Descripción: [
      El sistema debe notificar al paciente si está teniendo un ataque
      epiléptico.
    ],
    Necesidad: "m",
    Prioridad: "h",
    Estabilidad: "c",
    Verificabilidad: "m",
    Fuente: "a")),

  make-item("rf-read", srs.make-tag("R", "F"), (
    Descripción: [
      El sistema debe leer señales de encefalograma de un lector de señales de
      encefalograma.
    ],
    Necesidad: "l",
    Prioridad: "l",
    Estabilidad: "i",
    Verificabilidad: "l",
    Fuente: "a")),

  make-item("rf-replicate", srs.make-tag("R", "F"), (
    Descripción: [
      El sistema debe tener el mismo compotamiento que la implementación de
      referencia.
    ],
    Necesidad: "h",
    Prioridad: "h",
    Estabilidad: "c",
    Verificabilidad: "h",
    Fuente: "c")),

  /* *** Requisitos NO funcionales (Restricciones) --------------------- *** */

  /* Plantilla v{y

  make-item("", srs.make-tag("R", "N"), (
    Descripción: [],
    Necesidad: "h/m/l",
    Prioridad: "h/m/l",
    Estabilidad: "c/i/vu",
    Verificabilidad: "h/m/l",
    Fuente: "a/c")),

  */

  make-item("rnf-muestras-por-segundo", srs.make-tag("R", "N"), (
    Descripción: [
      El sensor de encefalograma en el dispositivo empotrado debe leer a razón
      de 256 muestras por segundo.],
    Necesidad: "h",
    Prioridad: "h",
    Estabilidad: "i",
    Verificabilidad: "m",
    Fuente: "c")),

  make-item("rnf-ritmo-de-muestro", srs.make-tag("R", "N"), (
    Descripción: [
      El sensor debe leer muestras a un ritmo constante, es decir],
    Necesidad: "h",
    Prioridad: "h",
    Estabilidad: "i",
    Verificabilidad: "m",
    Fuente: "a")),

  make-item("rnf-épocas-por-segundo", srs.make-tag("R", "N"), (
    Descripción: [
      El detector de encefalograma deberá procesar como mínimo una época de
      1280 muestras por segundo en el peor de los casos.],
    Necesidad: "h",
    Prioridad: "h",
    Estabilidad: "i",
    Verificabilidad: "h",
    Fuente: "c")),

  make-item("rnf-no-fallo", srs.make-tag("R", "N"), (
    Descripción: [
      El sistema no puede terminar de forma abrupta bajo ningún concepto.],
    Necesidad: "l",
    Prioridad: "l",
    Estabilidad: "c",
    Verificabilidad: "m",
    Fuente: "a")),

  make-item("rnf-f1", srs.make-tag("R", "N"), (
    Descripción: [
      La puntuación $F_1$ o _$F_1$ score_ para un modelo entrenado debe ser
      superior al $99%$ y se calcula como:
      $F_1 = (2 dot.c "PRE" dot.c "SEN")/("PRE" + "SEN")$, donde
      $"SEN" = "TP"/("TP"+"FN")$ y $"PRE" = "TP"/("TP"+"FP")$. Donde _TP_ son
      los verdaderos positivos; _FN_, los falsos negativos; y _FP_, los falsos
      positivos.],
    Necesidad: "l",
    Prioridad: "l",
    Estabilidad: "c",
    Verificabilidad: "m",
    Fuente: "a")),

  make-item("rnf-max-distance-error", srs.make-tag("R", "N"), (
    Descripción: [
      Debe haber un error medio absoluto al $10^(-6)$ en la función
      `max_distance` con respecto a la implementación original en Python 3.
    ],
    Necesidad: "m",
    Prioridad: "l",
    Estabilidad: "c",
    Verificabilidad: "h",
    Fuente: "c")),

  make-item("rnf-better", srs.make-tag("R", "N"), (
    Descripción: [
      El sistema debe detectar más rápido un ataque epiléptico que la
      implementación de referencia, comparado en el mismo dispositivo.
    ],
    Necesidad: "h",
    Prioridad: "h",
    Estabilidad: "c",
    Verificabilidad: "h",
    Fuente: "c")),

  make-item("rnf-embed", srs.make-tag("R", "N"), (
    Descripción: [
      El sistema debe funcionar en un dispositivo empotrado de bajo consumo.
      Preferiblemente con un procesador RISC-V como el ESP32C3 o el ESP32C6.
    ],
    Necesidad: "h",
    Prioridad: "m",
    Estabilidad: "vu",
    Verificabilidad: "h",
    Fuente: "c")),

)

= Análisis
En este capítulo se da una descripción general del problema
(@sec:2-plantemiento), los casos de uso (@sec:2-casos-de-uso),
los requisitos del mismo (@sec:2-requisitos) y el análisis de los mismos
(@sec:2-análisis-de-requisitos).

== Planteamiento del problema <sec:2-plantemiento>
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

== Casos de uso <sec:2-casos-de-uso>
De acuerdo con Craig Larman, _Unified Process_ define el modelo de casos de uso
dentro de la disciplina de requisitos e insiste en que los casos de uso son
documentos textuales, no diagramas, y que el modelado de casos de uso es
principalmente un acto de escribir texto, no de dibujar diagramas @LarmanUML. 

El mismo autor define actor como todo aquello con comportamiento, eso incluye
el propio sistema cuando este hace uso de otros servicios o sistemas. Los
actores no solo son roles que interpretan personas, también organizaciones,
_software_ y máquinas. Hay tres tipos de actores externos: actor principal,
actor de apoyo (_supporting actor_) y actor entre bastidores (_offstage
actor_). @LarmanUML

En este proyecto solo tiene sentido de hablar de dos actores que son el
paciente, que quiere detectar sus propios ataques epilépticos; y el doctor,
que puede ser un médico o un investigador que se encarga de entrenar el modelo
para posteriormente detectar ataques epilépticos.

#srs.show-class(reqs, srs.make-tag("CU"),
  class-formatter: srs.default-class-formatter-maker(),
  item-formatter: srs.default-item-formatter-maker(
    name: srs.name-by-field-maker("Nombre")))

#figure(
  image("uml/casos-de-uso.svg", width: 70%),
  caption: [Modelo de casos de uso]
)

== Requisitos <sec:2-requisitos>
En esta sección se provee la lista de requistios que se ha obtenido a partir de
un análisis exhaustivo de los casos de uso, definidos previamente en la
@sec:2-casos-de-uso. Los requisitos a continuación se clasifican en dos grandes
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

La @tab:rf muestra el formato de los requisitos funcionales y la @tab:rnf de
los no funcionales.

=== Requisitos funcionales
#srs.show-class(reqs, srs.make-tag("R", "F"),
  class-formatter: srs.default-class-formatter-maker(
    id       : "tab:rf",
    breakable: false),
  item-formatter: srs.default-item-formatter-maker(
    name:      srs.incremental-name-maker("RF-", first: 1, width: 2),
    breakable: false))

=== Requisitos no funcionales

#srs.show-class(reqs, srs.make-tag("R", "N"),
  class-formatter: srs.default-class-formatter-maker(
    id       : "tab:rnf",
    breakable: false),
  item-formatter: srs.default-item-formatter-maker(
    name:      srs.incremental-name-maker("RNF-", first: 1, width: 2),
    breakable: false))

== Análisis de requisitos <sec:2-análisis-de-requisitos>
Finalmente, las siguientes dos matrices de trazabilidad nos permiten ver la
relación de cada requisito con cada caso de uso para determinar la cobertura y
el grado de dependencia entre ambos.

// La @tab:matrix-rf relaciona requisitos funcionales con casos de uso y la
// @tab:matrix-rnf relaciona requisitos no funcionales con casos de uso. Como se
// puede ver todos los casos de uso están cubiertos.
// TODO: Decir más cosas, no sé
// TODO: Añadir orígenes.
