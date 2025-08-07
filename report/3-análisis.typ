// #import "src/lib.typ" as srs

= Análisis
En este capítulo se da una descripción general del problema
(@sec:análisis-descripción), los casos de uso (@sec:análisis-casos-de-uso) y
los requisitos del mismo (@sec:análisis-requisitos).

== Descripción general <sec:análisis-descripción>
El objetivo es implementar el algoritmo para la detección de ataques
epilépticos a partir de patrones y las características de una señal
encefalograma desarrollado en la Universidad de Málaga @PaFESD, de forma que
ejecute en tiempo real en sistemas empotrados de bajo consumo energético,
además de optimizar el código preexistente para reducir el tiempo de
entrenamiento.

La parte de entrenamiento utiliza el algoritmo de validación para optimizar los
parámetros y el sistema empotrado usa únicamente el dicho algoritmo para
clasificar una época de la señal en: «zona de ataque epiléptico» o «zona libre
de ataque epiléptico» dado un modelo preentrenado llamado _batch_.

== Casos de uso <sec:análisis-casos-de-uso>
De acuerdo con Craig Larman, _Unified Process_ define el modelo de casos de uso
dentro de la disciplina de requisitos e insiste que los casos de uso son
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

#{
  show figure: set block(breakable: true)
  show table.cell.where(x: 0): set par(justify: false)
  figure(
    caption: [Plantilla del],
    table(
      // columns: (13.5em, auto),
      columns: (8.0em, auto),
      align: (left, left),
      table.header([*Campo*], [*Comentario*]),
    
      [*Nombre*],
      [Nombre del caso de uso, es una acción así que debe empezar con un verbo.],
    
      [*Alcance*],
      [«Dispositivo empotrado» o «Entrenamiento».
       A qué subsistema o subsistemas específicos afecta el mismo. En
       este proyecto se diferencia la fase de entrenamiento del dispositivo
       empotrado con el sistema en tiempo real.],
    
      [*Nivel*],
      [«Meta de usuario» o «Subfunción». La subfunción se diferencia de la meta
       de usuario en que es un paso intermedio.],
    
      [*Actores principales*],
      [Los actores que hacen uso de los servicios del sistema para alcanzar su
       objetivo.],
    
      [*Parte interesada*],
      [Son los actores a quienes les concierne y qué quieren.],
    
      [*Precondiciones*],
      [Qué debe ser cierto al inicio.],
    
      [*Postcondiciones*],
      [Qué se garantiza que es cierto al completar la operación.],
    
      [*Escenario de éxito principal*],
      [La descrición de los pasos de las situación más típica.],
    
      [*Extensiones*],
      [Escenarios alternativos y ramificaciones del escenario principal.],
    
      [*Requisitos especiales*],
      [Requisitos no funcionales relacionados.],

      [*Frecuencia en que ocurre*],
      [Cada cuánto tiempo se espera que se utilice el caso de uso.],
    )
  )
  set block(breakable: false)
}

#{
  show figure: set block(breakable: true)
  show table.cell.where(x: 0): set par(justify: false)
  figure(
    caption: [Leer señal de encefalograma],
    table(
      // columns: (13.5em, auto),
      columns: (8.0em, auto),
      align: (left, left),
      table.header([*Campo*], [*Comentario*]),
    
      [*Nombre*],
      [Leer señal de encefalograma],
    
      [*Alcance*],
      [Dispositivo empotrado],
    
      [*Nivel*],
      [Subfunción],
    
      [*Actores principales*],
      [Sensor],
    
      [*Parte interesada*],
      [
- Sensor: Quiere muestrear continuamente.
- Detector: Quiere un surtido continuo de muestras de encefalograma para
  realizar su función.
- Doctor: Quiere toda la señal de encefalograma en bruto para entrenar el
  modelo.
  ],
    
      [*Precondiciones*],
      [El sensor tiene alimentación y se ha activado por _software_.],
    
      [*Postcondiciones*],
      [El resultado es un valor real que está dentro de un rango válido.],
    
      [*Escenario de éxito principal*],
      [
1. El sensor mide un valor analógico en bruto en un instante.
2. El sensor convierte dicho valor a otro con las unidades esperadas.
3. El sensor escribe el valor en la señal.
      ],
    
      [*Extensiones*],
      [
1. Desconexión del sensor
   1. El sensor notifica al paciente
   2. El sensor escribe el valor máximo del rango válido, para no detener el
      sistema ni dejar esperando al resto de actores.
2. Si valor después de convertirlo fuera mayor que el límite superior del
   rango válido.
   1. El sensor escribe un _log_.
   2. El sensor escribe el valor máximo del rango válido y continúa con la
      operación.
3. Si el valor después de convertirlo fuera menor que el límite inferior del
   rango válido.
   1. El sensor escribe un _log_.
   2. El sensor escribe el valor mínimo del rango válido y continúa con la
      operación.
  ],
    
      [*Requisitos especiales*],
      [
- Debe leer 256 muestras por segundo.
- Debe leer a un ritmo constante.
- Los valores leídos deben estar en un rango válido.
  ],

      [*Frecuencia en que ocurre*],
      [Continuo],
    )
  )
  set block(breakable: false)
}

#{
  show figure: set block(breakable: true)
  show table.cell.where(x: 0): set par(justify: false)
  figure(
    caption: [Detectar ataque],
    table(
      // columns: (13.5em, auto),
      columns: (8.0em, auto),
      align: (left, left),
      table.header([*Campo*], [*Comentario*]),
    
      [*Nombre*],
      [Detectar ataque],
    
      [*Alcance*],
      [Entrenamiento],
    
      [*Nivel*],
      [Meta de usuario],
    
      [*Actores principales*],
      [Detector],
    
      [*Parte interesada*],
      [
- Detector: Quiere clasificar la época actual.
- Paciente: Quiere saber si está teniendo un ataque epiléptico.
],
    
      [*Precondiciones*],
      [El modelo debe estar cargado, y el sensor debe estar activo.],
    
      [*Postcondiciones*],
      [Ninguna],
    
      [*Escenario de éxito principal*],
      [
1. El detector espera a tener un época de señal del sensor.
2. El detector pasa un filtro de paso bajo y paso alto a la época.
3. El detector decide que la época no es un artefacto (pestañeo, ...).
4. El detector computa las características de la señal (`max_distance`, ...).
5. El detector computa la distancia entre la época y cada uno de los patrones.
6. Retorna que es un ataque epiléptico.
],
    
      [*Extensiones*],
      [
1. En el paso 3., si se trata de un artefacto.
   1. Como es un artefacto, no hace falta continuar, retorna que no es un
      ataque.
2. En el paso 4. si para alguna de las características el valor cae fuera de
   los rangos definidos por el modelo (_batch_).
   1. No es un ataque epiléptico, retorna que no lo es.
3. En el paso 5. si para ninguno de los patrones la distancia es lo
   suficientemente pequeña (determinada por el modelo).
   1. No es un ataque epiléptico, lo retorna.
],
    
      [*Requisitos especiales*],
      [
1. Debe procesar una época y compararla hasta con tres patrones por segundo.
],

      [*Frecuencia en que ocurre*],
      [Cotinua (una vez por segundo)],
    )
  )
  set block(breakable: false)
}

#{
  show figure: set block(breakable: true)
  show table.cell.where(x: 0): set par(justify: false)
  figure(
    caption: [Entrenar el modelo],
    table(
      // columns: (13.5em, auto),
      columns: (8.0em, auto),
      align: (left, left),
      table.header([*Campo*], [*Comentario*]),
    
      [*Nombre*],
      [Entrenar el modelo],
    
      [*Alcance*],
      [Meta de usuario],
    
      [*Nivel*],
      [Entrenamiento],
    
      [*Actores principales*],
      [Doctor y detector],
    
      [*Parte interesada*],
      [
Doctor: Quiere obtener el modelo y su eficacia.
Detector: Quiere utilizar el modelo para hacer su trabajo.
Paciente: Quiere el modelo para configurar y personalizar su sistema de detección.
],
    
      [*Precondiciones*],
      [Una señal continua con los intervalos de ataque epiléptico etiquetados.],
    
      [*Postcondiciones*],
      [Un modelo válido con los rangos de características de la señal y los
      patrones de la propia señal que optimicen la puntuación $F_1$ del sistema.
      Además de la precisión, la sensibilidad y su puntuación $F_1$.],
    
      [*Escenario de éxito principal*],
      [
1. El doctor graba la señal de un paciente durante un periodo largo de tiempo.
2. El doctor etiqueta los intervalos de la propia señal en los que está
   ocurriendo un ataque.
3. El sensor pasa un filtro de paso bajo y paso alto a la señal.
4. El detector marca en la señal las secciones con artefactos.
5. El optimizador (externo) toma el detector y optimiza los parámetros del
   modelo para maximizar la puntuación $F_1$.
6. El optimizador retorna el modelo preparado en un archivo.
],
    
      [*Extensiones*],
      [
1. Puntuación $F_1$ baja.
  1. El doctor estudiará por qué ocurre.
  2. El doctor reconfigura el optimizador y vuelve a intentar generar el
     modelo.
],
    
      [*Requisitos especiales*],
      [Ninguno],

      [*Frecuencia en que ocurre*],
      [Escasa: el modelo se entre una sola vez por paciente y tarda mucho en
      hacerlo],
    )
  )
  set block(breakable: false)
}

#{
  show figure: set block(breakable: true)
  show table.cell.where(x: 0): set par(justify: false)
  figure(
    caption: [Notifica al paciente],
    table(
      // columns: (13.5em, auto),
      columns: (8.0em, auto),
      align: (left, left),
      table.header([*Campo*], [*Comentario*]),
    
      [*Nombre*],
      [Notifica al paciente],
    
      [*Alcance*],
      [Entrenamiento y dispositivo empotrado],
    
      [*Nivel*],
      [Meta de usuario],
    
      [*Actores principales*],
      [Detector y paciente],
    
      [*Parte interesada*],
      [
Detector: Quiere notificar al paciente que está teniendo un ataque epiléptico.
Paciente: Quiere saber si está teniendo un ataque epiléptico.
Doctor: Quiere saber si su paciente está teniendo un ataque epiléptico.
],
    
      [*Precondiciones*],
      [El dectector configurado y el sensor alimentado.],
    
      [*Postcondiciones*],
      [Ninguna],
    
      [*Escenario de éxito principal*],
      [
1. El paciente está teniendo un ataque epiléptico.
2. El detector lo detecta.
3. El detector lo notifica.
],
    
      [*Extensiones*],
      [
1. Si el paciente está teniendo un ataque epiléptico, pero el detector no lo
   detecta como tal.
   1. El paciente marca en la señal, dónde ha tenido el ataque.
   2. El modelo se reentrena con la información actualizada.
   3. Se informa del caso al doctor y a los desarrolladores.
],
    
      [*Requisitos especiales*],
      [Ninguno],

      [*Frecuencia en que ocurre*],
      [Escasa, cada vez que tiene un ataque epiléptico el paciente],
    )
  )
  set block(breakable: false)
}

#figure(
  image("uml/casos-de-uso.svg", width: 80%),
  caption: [Modelo de casos de uso]
)

== Requisitos <sec:análisis-requisitos>

// #let make-class(
//   id,
//   name,
//   fields: (),
//   classes: (),
//   origins: (:),
// ) = {
