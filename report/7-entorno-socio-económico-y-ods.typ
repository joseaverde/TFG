= Entrono socio-económico y objetivos de desarrollo sostenible
== Presupuesto
=== Recursos humanos
El proyecto ha sido desarrollado por un único desarrollador a lo largo de 34
semanas de acuerdo con la planificación (@sec:8-planificación), en las que
trabajó de media unas 15 horas semanales. El salario de un investigador
_junior_ en una universidad de España suele comenzar en los $20000$€ anuales
@salarioMedio. Un año tiene 52 semanas, de las cuales 30 días naturales (o sea,
unas 4 semanas) son de vacaciones en España. Se aproxima el salario por hora
como:

  $ 20000"€"/"año"
    dot.c (1 "año")/((52 - 4) "semanas laborales")
    dot.c (1 "semana laboral")/(5 "días laboral")
    dot.c (1 "día laboral")/(8 "horas laborales") $
  $ approx 10.42 "€"/"hora laboral" $

Luego de $34$ semanas, trabajando $15$ horas cada día, da un total de $660$
horas. Lo que da un total de $6877.20$ € (CINCO MIL TRESCIENTOS CATORCE EUROS
CON VEINTE CÉNTIMOS) como se ve en la @tab:7-costes-humanos.

#figure(
  caption: [Costes humanos],
  table(
    columns: (auto, auto, auto),
    table.header([Horas totales], [€/Hora], [Coste total (€)]),
    [$510$], [$10.42$], [$5314.20$])) <tab:7-costes-humanos>

=== Recursos materiales
En este caso solo se cuentan los recursos físicos como el _hardware_, todas las
licencias que se usaron fueron de código abierto y gratuitas. Se utilizó: un
portátil para hacer el entrenamiento y programar el código; y distintos tipos
de dispositivos empotrados para hacer pruebas de rendimiento.

Espressif está comprometido con que sus productos tengan una longevidad mínima
de 12 años desde que se empezaron a fabricar @EspressifLogevity. Los
dispositivos empotrados en cuestión son los modelos ESP32C3, ESP32S3 y ESP32C6.

#{
  show table.cell.where(y: 0): set par(justify: false)
  figure(
    caption: [Costes materiales],
    table(
      columns: (10em, auto, auto, auto, auto, auto),
      align: (left+horizon, horizon, horizon, horizon, horizon, horizon),
      table.header([Producto], [Coste], [Vida útil], [Tiempo de uso],
                   [Coste mensual], [Coste amortizado]),

      [ESP32C3 (x3)],   [$  15.99$€], [$108$ meses], [$12$ meses], [$ 0.15$€], [$  1.78$€],
      [ESP32S3],        [$  13.59$€], [$108$ meses], [$12$ meses], [$ 0.13$€], [$  1.51$€],
      [ESP32C6],        [$  15.99$€], [$132$ meses], [$12$ meses], [$ 0.12$€], [$  1.45$€],
      [Raspberry Pi 4], [$  99.90$€], [$ 96$ meses], [$31$ meses], [$ 1.04$€], [$ 32.26$€],
      [Portátil],       [$1100.00$€], [$ 60$ meses], [$23$ meses], [$18.33$€], [$421.67$€],
      table.hline(stroke: 0.3pt + black),
      [*Total*], table.cell(colspan: 5, align: left)[*$458.67$€*],
    ))}

=== Costes indirectos

=== Coste total
El coste total se considera la suma de todos los costes, como se ve en la
@tab:7-coste-total, el coste total asciende a los ¿? € ().

#figure(
  caption: [Coste total],
  table(
    columns: (auto, auto),
    align: (left+horizon, right+horizon),
    table.header([Concepto], [Coste]),

    [Recursos humanos],    [$6877.20$ €],
    [Recursos materiales], [$458.67$ €],
    [Costes indirectos],   [¿? €],
    table.hline(stroke: 0.3pt + black),
    [*Total*],             [¿? €]
  )) <tab:7-coste-total>

== Impacto socio-económico
El objetivo del proyecto no era crear un algoritmo de detección de ataques
epilépticos de cero, sino utilizar uno ya existente, estudiarlo y optimizarlo.
De hecho, el algoritmo utilizado es de clasificación, es decir, decide si está
o no en un ataque epiléptico.

Sin embargo, desmostrar que es posible ejecutar un programa tan
computacionalmente costoso como este en tiempo real en un dispositivo empotrado
de bajas características permite indicar que esta clase de detectores son
viables, y, además que no tienen por qué consumir muchos recursos. Este estudio
ha sido desarrollado en colaboración la Universidad de Málaga, donde se ideó el
algoritmo original.

El otro objetivo del trabajo fue hacer un análisis comparativo entre distintas
técnicas de programación, lenguajes de programación... Y observar cómo mejora
o empeora el rendimiento en distintas plataformas.

Utilizar un probador de teoremas junto al código permite, entre otras cosas,
demostrar matemáticamente que el programa no puede fallar: todos los caminos
que dan paso a un error están tratados y por ende el sistema no puede terminar
de manera abrupta. Es un filosofía de programación que, pese a ser
extremadamente costosa, para estos casos tan delicados como es la detección de
ataques epilépticos da paz mental a los pacientes; pues saben que en ningún
momento va a _petar_ la aplicación.


== Objetivos de desarrollo sostenible
Los objetivos de desarrollo sostenible son un conjunto de objetivos globales
(17) para erradicar la pobreza, proteger el planeta y asegurar la prosperidad
para todos como parte de una nueva agenda de desarrollo sostenible. Cada
objetivo tiene metas específicas que deben alcanzarse en el año 2030 @uc3mODS.

El primero de los objetivos con el contribuye es el tercero: «salud y
bienestar». El proyecto se trata de una aplicación médica para detección de
ataques epilépticos. Descubrir qué patrones de señales neuronales preceden una
convulsión permite a los pacientes tomar medicación antes de que ocurra.

Esto lleva al décimo objetivo de desarrollo sostenible: «reducción de las
desigualdades». La epilepsia es una enferemedad que en 2006 afecta a seis de
cada mil europeos y está prohibido en muchos países, por ejemplo, la conducción
de personas con epilepsia, excepto aquellas que puedan controlar los posibles
ataques @ElPaísConducirYEpilepsia. Así pues, poder detectar el ataque antes de
que ocurra podría permitir que estas personas puedan actuar y luego poder
conducir como el resto de ciudadanos.

Por último, en relación con el decimotercer objetivo («acción por el clima») y
el séptimo objetivo («energía asequible y no contaminante»), una de las
prioridades del proyecto es que el dispositivo final corra en tiempo real y con
un consumo mínimo. El estudio de los algoritmos, sus optimizaciones y las
técnicas de programación son en pos de conseguir un sistema además de eficaz,
eficiente.
