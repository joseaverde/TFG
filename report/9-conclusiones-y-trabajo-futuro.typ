= Conclusiones y trabajo futuro <sec:9>
En este capítulo se enuncian en primer lugar las conclusiones del proyecto
(@sec:9-proyecto), en segundo lugar las (@sec:9-personal) y finalmente qué
resta para el futuro (@sec:9-futuro).

== Conclusiones del proyecto <sec:9-proyecto>
Se han completado todos los objetivos, aunque el tercer objetivo parcialmente.
Los objetivos se describieron en la @sec:1-objetivos.

- *O1*: Se pudo implementar dicho módulo de Python 3 en #box([C++]) y como se
  observa en la @sec:5-validation los resultados son geniales. En un portátil
  se ha mejorado el tiempo $56.7$ veces y en un servidor, que es donde va a
  acabar corriendo el entrenamiento, va $85.9$ veces más rápido que la
  implementación anterior.

  Reducir el tiempo de entrenamiento permite que se pueda iterar más rápido y
  experimentar sin esperar tanto tiempo. Además va de acuerdo con los objetivos
  de desarrollo sostenible, menos tiempo de cómputo, menos energía consumida.

  Según el equipo de la Universidad de Málaga con el que se ha trabajado, los
  resultados son generiales y mejores de lo esperado. Así que, este primer
  objetivo se da por finalizado.

- *O2*: Se pudo hacer compilación cruzada de #box([C++]) a dispositivos
  empotrados de bajo consumo como la ESP32C3, ESP32C6 y ESP32S3 de Espressif,
  además de otras máquinas como la Raspberry Pi 3 y la Raspberry Pi 4. El
  programa ejecutaba en tiempo real como se puede ver en los resultados de la
  evaluación en la @sec:5-cxx-realtime, con un total de $1.64$ épocas por
  segundo en el peor de los casos contrastando con tres patrones distintos en
  la ESP32C3.

  Por lo que el objetivo se dio por completado. Sin embargo, el autor de este
  documento decidió experimentar cuánto se podría mejorar. Entre las razones,
  si computa más épocas por segundo, el sistema tiene más tiempo para descansar
  y así puede llegar a consumir menos energía. Se implementón en Ada con punto
  fijo y como se ve en la @sec:5-spark-realtime, los resultados son muchos
  mejores con un total de $10.36$ épocas por segundo en el peor de los casos
  contrastando con tres patrones distintos en la ESP32C3. De ahí también se
  origina el objetivo *O3*.

- *O3*: Puesto que punto fijo es muy sensible a errores de desbordamiento se
  hizo todo el estudio de la @sec:4-estudio-de-la-solución-final en el que,
  junto a un probador de teoremas, se pudo demostrar en la mayor parte del
  código la ausencia de errores en tiempo de ejecución. Sin embargo, dado que
  demostrar cada uno de los teoremas demora mucho tiempo, no se pudieron
  demostrar todos y cada uno de ellos. Aun así, puesto que todas las pruebas,
  pese no haber demostrado completamente el programa entero, se da por
  completado el objetivo y se deja como trabajo futuro el demostrar el resto
  del algoritmo.

Así pues, el trabajo se da por terminado.

Se concluye que #box([C++]) es _de facto_ más rápido que Python 3 o Ada, y que
es preferible para desarrollar sistemas en los que el rendimiento es
indispensable. Además, hoy en día es preferible utilizar punto flontante en vez
de punto fijo, excepto cuando se esté trabajando para dispositivos empotrados
que no tengan FPU o para aplicaciones bancarias.

Ada junto a SPARK permiten demostrar gran variedad de propiedades del código y
permite verificarlo formalmente con probadores de teoremas. Sin embargo, dado
que trabajar con SPARK es tedioso y costoso no está indicado para cualquier
proyecto, únicamente para aquellos en los que el fallo sea la diferencia entre
la vida y la muerte. Es preferible utilizarlo para punto específicos del
programa más sensibles a fallos.  Ada sigue siendo un gran lenguaje de
programación, pese a no ser tan rápido como C++. Permite describir propiedades
del programa mediante contratos.


== Conclusiones personales <sec:9-personal>
Este ha sido un proyecto que me ha permitido utilizar gran variedad de
tecnologías y juntarlas entre sí para descubrir qué es lo que funciona y qué
no. He podido utilizar lenguajes de programación con los que estaba
familiarizado como #box([C++]), Python 3 y Ada.

He aprendido mucho sobre compilación cruzada y he descubierto sobre todos los
rinconcitos que tiene cada arquitectura y sobre cómo exprimirlos al máximo. He
aplicado gran variedad de técnicas desde paralelismo, pasando por vectorización
hasta metaprogramación y uso de la excelente bibliotecas de rangos de
#box([C++]). 

Previamente he utilizado SPARK, pero esta es la primera vez que lo utilizo para
un proyecto serio. He aplicado todo lo aprendido en lógica y matemáticas para
demostrar propiedades de mi código fuente. Estoy muy orgulloso con los
resultados y me dan ganas de continuar con ello.


== Trabajo futuro <sec:9-futuro>
Como se dijo previamente, todavía queda por demostrar utilizando SPARK los
algoritmos de la transformada rápida de Fourier (`FFT`) y de `Welch`. Aunque
sean más complicados de demostrar, existen artículos al respecto al menos para
la transformada de Fourier @welch2003fixed (que es el algoritmo que se ha
implementado).

Después de discutirlo con los investigadores de la Universidad de Málaga, como
trabajo futuro queda: por su parte investigar cómo predecir en vez de
clasificar ataques epilépticos, por nuestra parte quedaría permitir que el
modelo se reentrene en tiempo real. Para ello era necesario rascar hasta el
milisegundo y por eso este trabajo se ha centrado tanto en la parte de
optimización del algoritmo.
