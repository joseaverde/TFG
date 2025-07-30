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
dentro de la disciplina de requisitos: «los casos de uso son documentos
textuales, no diagramas, y el modelado de casos de uso es principalmente un
acto de escribir texto, no de dibujar diagramas» @LarmanUML. 

#figure(
  caption: [Cosas],
  table(
    columns: (13.5em, auto),
    table.header([*Campo*], [*Comentario*]),
  
    [*Nombre*],
    [Empezar con un verbo],
  
    [*Alcance*],
    [],
  
    [*Nivel*],
    [_user-goal_ o _subfunction_],
  
    [*Actores principales*],
    [],
  
    [*Parte interesada*],
    [],
  
    [*Precondiciones*],
    [],
  
    [*Postcondiciones*],
    [],
  
    [*Escenario de éxito principal*],
    [],
  
    [*Extensiones*],
    [],
  
    [*Requisitos especiales*],
    [Requisitos no funcionales relacionados]
  )
)

== Requisitos <sec:análisis-requisitos>

