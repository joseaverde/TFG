#import "utility.typ": *

= Introducción <sec:1>
Este capítulo describe brevemente la motivación que existe detrás del proyecto
(@sec:1-motivación), los objetivos principales del mismo (@sec:1-objetivos) y
se resume brevemente el contenido de cada uno de los capítulos
(@sec:1-estructura). El código y la documentación de este proyecto están
alojados en https://github.com/joseaverde/TFG .

== Motivación <sec:1-motivación>
Este proyecto surge de la colaboración entre el grupo de investigación de
modelos de programación paralela y compiladores de la Universidad de Málaga
(PPMC) @PPMC, que escribió un artículo sobre detección de ataques epilépticos
utilizando patrones aumentados y características estadísticas @PaFESD, y la
Universidad Carlos III de Madrid.

El estudio obtuvo resultados prometedores y el foco pasó a ser la viabilidad
de ejecución del algoritmo de detección en un dispositivo empotrado de bajo
consumo en tiempo real. La detección de estos ataques es computacionalmente
costosa, pero si fuera factible que funcionase en tales dispositivos abriría
las puertas al desarrollo de accesorios que monitoreen la actividad cerebral de
un paciente en tiempo real.

Finalmente se quería estudiar cuál es el impacto del paralelismo en el tiempo
de ejecución del programa de entrenamiento del modelo, pues consume mucha
energía y tiempo ejecutarlo y cualquier mejora es disponible; y también qué
técnicas se podrían aplicar a los algoritmos para mejorar su rendimiento en
máquinas de bajas características computacionales.


== Objetivos <sec:1-objetivos>
El objetivo principal del proyecto es hacer un estudio de distintas técnicas y
algoritmos para la optimización de programas. Se ha utilizado un artículo
redactado por investigadores de la universidad de Málaga @PaFESD como base para
explorar en qué formas se puede mejorar el rendimiento del mismo.

Se ha trabajado en compañía de dicho equipo para intentar mejorar los tiempos
de entrenamiento y para confirmar la viabilidad de ejecutar el algoritmo de
detección en un sistema empotrado de bajo consumo. Se decidió también que el
sistema estuviera escrito en C++, por su increíble rendimiento y su facilidad
para ejecutarlo en sistemas empotrados.

A mitad del proyecto sugió un tercer objetivo tras determinar la viabilidad de
utilizar números en punto fijo en vez de punto flotante para la representación
y procesamiento de los datos. Había que verificar formalmente que no hubiera
casos de desbordamiento en operaciones de punto fijo. Para ello se usó el
lenguaje de programación Ada junto a SPARK, que usa probadores de teoremas por
debajo, para verificar formalmente el trabajo.

- *O1*: Implementar un módulo de Python3 en #cxx que disminuya el tiempo
  de entrenamiento del modelo para la detección de ataques epilépticos de la
  implementación de referencia @PPMC-DAC.
- *O2*: Implementar un sistema de tiempo real en un dispositivo empotrado de
  bajo consumo que utilice el modelo generado para clasificar épocas de señal
  en «ataques epiléticos» y en «libres de ataques epilépticos».
- *O3*: Verificar formalmente utilizando un probador de teoremas interactivo
  que el programa se comporta como debe y no puede terminar de manera
  abrupta.

== Estructura del documento <sec:1-estructura>
El documento contiene los siguientes capítulos:

- Capítulo 1 -- #link(label("sec:1"), [_Introducción_]), explica la motivación
  que llevó a realizar este proyecto, los objetivos del mismo y la propia
  estructura.

- Capítulo 2 -- #link(label("sec:2"), [_Estado del arte_]), un análisis del
  estado de la cuestión, qué temas son relevantes y qué estudios existen al
  respecto. Analiza estudios sobre detección de ataques epilépticos,
  arquitecturas y juegos de instrucciones, representación de valores numéricos,
  demostraciones interactivas de teoremas y técnicas de programación.

- Capítulo 3 -- #link(label("sec:3"), [_Análisis_]), estudia y analiza los
  casos de uso del sistema, a partir de los cuales enumera y examina los
  requisitos funcionales y no funcionales del mismo. Finalmente se da el
  diagrama con la arquitectura del sistema.

- Capítulo 4 -- #link(label("sec:4"), [_Diseño e implementación_]), contiene
  el estudio de la solución final: define matemáticamente el problema, a base
  de la cual se justifican las decisiones de diseño del problema.

- Capítulo 5 -- #link(label("sec:5"), [_Validación, verificación y
  evaluación_]), efectúa una validación y una verificación del programa final,
  y hace una evaluación del rendimiento del mismo.

- Capítulo 6 -- #link(label("sec:6"), [_Marco regulador_]), estudia la
  legislación aplicable sobre el mismo y las licencias de las herramientas
  utilizadas y las dependencias con las que se enlaza la solución, además se
  identifican los estándares técnicos relevantes para el desarrollo del
  proyecto.

- Capítulo 7 -- #link(label("sec:7"), [_Entorno socio-económico y objetivos de
  desarrollo sostenible_]), elabora un presupuesto para el proyecto, hace un
  análisis del entorno socio-económico y compara a qué objetivos de desarrollo
  sostenible se adhiere.

- Capítulo 8 -- #link(label("sec:8-planificación"), [_Planifiación_]), separa
  el proyecto en distintas tareas y elabora un cronograma indicando en qué
  momento empiezan y en qué momento terminan.

- Capítulo 9 -- #link(label("sec:9"), [_Conclusiones y trabajo futuro_]),
  concluye el proyecto, resume qué se hizo y estudia cómo se podría continuar y
  qué se podría mejorar.
