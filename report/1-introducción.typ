= Introducción <sec:1>
Este capítulo describe brevemente la motivación detrás del proyecto
(@sec:1-motivación), los objetivos principales del mismo (@sec:1-objetivos) y
se dan pequeñas pinceladas sobre el contenido de cada uno de los capítulos
(@sec:1-estructura).

== Motivación <sec:1-motivación>


== Objetivos <sec:1-objetivos>
El objetivo principal del proyecto es hacer un estudio de distintas técnicas y
algoritmos para la optimización de programas. Se ha utilizado un artículo
redactado por investigadores de la universidad de Málaga @PaFESD como base para
explorar en qué formas se puede mejorar.

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

- *O1*: Implementar un módulo de Python3 en #box([C++]) que disminuya el tiempo
  de entrenamiento del modelo para la detección de ataques epilépticos de la
  implementación de referencia @PPMC-DAC.
- *O2*: Implementar un sistema de tiempo real en un dispositivo empotrado de
  bajo consumo que utilice el modelo generado para clasificar épocas de señal
  en «ataques epiléticos» y en «libres de ataques epilépticos».
- *O3*: Verificar formalmente utilizando un probador de teoremas interactivo
  que el programa se comporta como debe y no hay puede terminar de manera
  abrupta.

== Estructura del documento <sec:1-estructura>
El documento contiene los siguientes capítulos:

- Capítulo 1 -- #link(label("sec:1"), [_Introducción_]), explica la motivación
  que llevo a realizar este proyecto, los objetivos del mismo y la propia
  estructura.

- Capítulo 2 -- #link(label("sec:2"), [_Estado del arte_]), un análisis del
  estado de la cuestión, qué temas son relevantes y qué estudios existen al
  respecto. Sobrevuela estudios sobre detección de ataques epilépticos,
  arquitecturas y juegos de instrucciones, representación de valores numéricos,
  demostraciones interactivas de teoremas y técnicas de programación.

- Capítulo 3 -- #link(label("sec:3"), [_Análisis_]), estudia y analiza los
  casos de uso del sistema, a partir de los cuales enumera y examina los
  requisitos funcionales y no funcionales del mismo.

- Capítulo 4 -- #link(label("sec:4"), [_Diseño e implementación_]), define
  matemáticamente el problema, a base de la cuál se justifican las decisiones 
  de diseño del problema. Finalmente se muestra un diagrama con la arquitectura
  del sistema implementado en global.

- Capítulo 5 -- #link(label("sec:5"), [_Validación, verificación y
  evaluación_]), efectua una validación y una verificación del programa final.
  Y hace una evaluación del rendimiento del mismo.

- Capítulo 6 -- #link(label("sec:6"), [_Marco regulador_]), estudia la
  legislación aplicable sobre el mismo y las licencias de las herramientas
  utilizadas y las dependencias con las que se enlaza la solución, además, se
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
  conluye el proyecto, resume qué se hizo y estudia cómo se podría continuar y
  qué se podría mejorar.
