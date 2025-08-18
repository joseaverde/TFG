= Planificación <sec:8-planificación>
En este capítulo se describe la planificación del proyecto, el cual se divide
en siete tareas distintas, y cada una de estas en otras subtareas. Se puede ver
en la @gantt un diagrama de Gantt del proyecto con la fecha de inicio y
finalización de cada una de las tareas.

La planificación no cuenta el tiempo necesario para elaborar la memoria, única
y exclusivamente de los pasos para realizar el trabajo desde principio a fin.
En total fueron 34 semanas.

// TODO: Referenciar en la parte de diseño a qué se refiere
- *Tarea 1*: _Configuración y familiarización con el código original._ \
  Esta tarea consiste en preparar el computador para el desarrollo del
  programa. Como este utiliza como base un trabajo preexistente, es necesario
  comprender: cómo funciona el código original, cómo obtiene los datos de los
  pacientes, cómo entrena modelos, dónde se puede mejorar. Finalmente, como el
  programa está escrito en #box([C++]), es necesario descargar el compilador y
  las herramientas de desarrollo.
  - *Tarea 1.1*: Instalación de Manjaro
  - *Tarea 1.2*: Instalación de herramientas de desarrollo.
  - *Tarea 1.3*: Familiarización con del repositorio original.
  - *Tarea 1.4*: Configuración del proyecto de C++ con `conan`.
- *Tarea 2*: _Implementación en C++._\
  Una vez que está el entorno preparado, se procede a hacer ingeniería inversa
  para identificar cómo funcionan las subrutinas del código original, a partir
  de las cuales se escribe el código optimizado en C++. Cuando este está
  terminado, se procede con un _binding_ (interfaz entre dos lenguajes de
  programación) de C++ con Python 3, para que se pueda integrar en el programa
  original.
  - *Tarea 2.1*: Implementación de características de la señal.
  - *Tarea 2.2*: Implementación del módulo de Python 3.
  - *Tarea 2.3*: Pruebas y comparación de resultados entre ambas
    implementaciones.
- *Tarea 3*: _Análisis de rendimiento y paralelización del algoritmo._
  Con el programa funcionando, se puede investigar cuáles son los cuellos de
  botella y qué partes del mismo se pueden mejorar. También se paraleliza para
  mejorar el tiempo de entrenamiento.
  - *Tarea 3.1*: Búsqueda de cuellos de botella
  - *Tarea 3.2*: Optimización y paralelización
  - *Tarea 3.3*: Pruebas de rendimiento
- *Tarea 4*: _Compilación cruzada de C++._
  El programa de detección debe correr en algún dispositivo empotrado y se debe
  determinar si corre o no en tiempo real.
  - *Tarea 4.1*: Portar código y dependencias al sistema de Espressif para
    compilar para la ESP32C3, ESP32C6 y ESP32C6.
  - *Tarea 4.2*: Compilación cruzada a Aarch64: Raspberry Pi3 y Raspberry Pi 4.
  - *Tarea 4.3*: Pruebas de rendimiento el empotrado
- *Tarea 5*: _Implementación en Ada._
  Se reimplementan algunas funciones del algoritmo en Ada para probar si es
  viable utilizar punto fijo, como Ada tiene soporte para punto fijo de manera
  nativa. Se implementa inicialmente en punto flotante y luego se sustituye el
  tipo base de flotante a fijo para hacer pruebas.
  - *Tarea 5.1*: Implementación de funciones básicas.
  - *Tarea 5.2*: Paralelización del algoritmo de detección.
  - *Tarea 5.3*: Pruebas de rendimiento en punto flotante y comparación.
  - *Tarea 5.4*: Pruebas de rendimiento en punto fijo y comparación.
- *Tarea 6*: _Compilación cruzada de Ada._
  Al igual que C++ es necesario hacer compilación cruzada en Ada para probar si 
  funciona en tiempo real o no.
  - *Tarea 6.1*: Integración de `gprbuild` con el sistema de Espressif.
    y compilación cruzada del código de Ada como biblioteca.
  - *Tarea 6.2*: Compilación cruzada para Aarch64: Raspberry Pi 3 y Raspberry
    Pi 4.
  - *Tarea 6.3*: Pruebas de rendimiento.
- *Tarea 7*: _Implementación en SPARK._
  Al determinar que es viable usar punto fijo, se procede a estudiar los casos
  de desbordamiento y subdesbordamiento del mismo para asegurar la ejecución
  correcta del mismo.
  - *Tarea 7.1*: Demostración de `maxdistance`.
  - *Tarea 7.2*: Demostración de `energy` e implementación y demostración de
    sus funciones auxiliares.
  - *Tarea 7.3*: Demostración de la función `dtw` e implementación y
    demostración de sus funciones auxiliares.
  - *Tarea 7.4*: Implementación y demostración de las funciones: seno, coseno
    y raíz cuadrada.
  - *Tarea 7.5*: Implementación y demostración de `simpson`.
  - *Tarea 7.6*: Implementación del resto de funciones.
#[
  #set page(flipped: true)
  #set align(horizon)
  #figure(
    image("uml/gantt.svg", width: 100%),
    caption: [Diagrama de Gantt del proyecto]
  ) <gantt>
]
