= Entrono socio-económico y objetivos de desarrollo sostenible
== Presupuesto
=== Recursos humanos

#figure(
  caption: [Costes humanos],
  table(
    columns: (auto, auto, auto),
    table.header([Horas totales], [€/Hora], [Coste total (€)]),
    [$650$], [$9.97$], [$6480.50$]))

=== Recursos materiales
==== _Hardware_

#{
  show table.cell.where(y: 0): set par(justify: false)
  figure(
    caption: [Costes de _hardware_],
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
      [*Total*], table.cell(colspan: 5, align: left)[*$458.67$€*],
    ))}
// https://www.espressif.com/en/products/longevity-commitment
==== _Software_

== Impacto socio-económico

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
