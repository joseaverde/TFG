= Entrono socio-económico <sec:7>
== Presupuesto
=== Recursos humanos
El proyecto ha sido desarrollado por un único desarrollador a lo largo de 34
semanas de acuerdo con la planificación (@sec:8-planificación), en las que
trabajó de media unas 15 horas semanales. El salario de un investigador
_junior_ en una universidad de España suele comenzar en los $20000$€ anuales
@salarioMedio. Un año tiene 52 semanas, de las cuales 30 días naturales (o sea,
unas 4 semanas) son de vacaciones en España. Se aproxima el salario por hora
como:

#let euros(x) = {
  let y = str(calc.round(x, digits: 2))
  if regex("[.]\d$") in y {
    y = y + "0"
  } else if regex("^\d+$") in y {
    y = y + ".00"
  }
  return y
}

#let semanas = 34
#let meses = 9
#let horas-diarias = 3
#let coste-hora = 10.42
#let total-horas = horas-diarias * semanas * 7
#let personal = total-horas * coste-hora

  $ 20000"€"/"año"
    dot.c (1 "año")/((52 - 4) "semanas laborales")
    dot.c (1 "semana laboral")/(5 "días laboral")
    dot.c (1 "día laboral")/(8 "horas laborales") $
  $ approx 10.42 "€"/"hora laboral" $

Luego de $#semanas$ semanas, trabajando $#horas-diarias$ horas cada día, da un
total de $#total-horas$ horas. Lo que da un total de $#euros(personal)$ € como
se ve en la @tab:7-costes-humanos.

#figure(
  caption: [Costes humanos],
  table(
    columns: (auto, auto, auto),
    table.header([Horas totales], [€/Hora], [Coste total (€)]),
    [$#total-horas$], [$#coste-hora$], [*$#euros(personal)$*]))
    <tab:7-costes-humanos>

=== Recursos materiales
En este caso solo se cuentan los recursos físicos como el _hardware_, todas las
licencias que se usaron fueron de código abierto y gratuitas. Se utilizó: un
portátil para hacer el entrenamiento y programar el código; y distintos tipos
de dispositivos empotrados para hacer pruebas de rendimiento.

#let gastos = (
  (name: [ESP32C3 (x3)],   cost:   15.99, life: 30, used:  9),
  (name: [ESP32S3],        cost:   13.59, life: 30, used:  9),
  (name: [ESP32C6],        cost:   15.99, life: 30, used:  9),
  (name: [Raspberry Pi 4], cost:   99.90, life: 36, used:  8),
  (name: [Portátil],       cost: 1600.00, life: 48, used: 10))

#let total-material = gastos.map((x) => x.cost / x.life * x.used).reduce((a, b) => a + b)

#{
  show table.cell.where(y: 0): set par(justify: false)
  figure(
    caption: [Costes materiales],
    table(
      columns: (10em, auto, auto, auto, auto, auto),
      align: (left+horizon, right, horizon, horizon, right, right),
      table.header([Producto], [Coste], [Vida útil], [Tiempo de uso],
                   [Coste mensual], [Coste amortizado]),

      ..(gastos.map((x) =>
        (x.name, [$#euros(x.cost)$€], [$#x.life$ meses], [$#x.used$ meses],
          [$#euros(x.cost / x.life)$€],
          [$#euros(x.cost / x.life * x.used)$€])).flatten()),
      table.hline(stroke: 0.3pt + black),
      [*Total*], [], [], [], [], [*$#euros(total-material)$€*],
    ))}

=== Costes indirectos
Los costes indirectos son aquellos que no pueden asociarte directamente a un
producto o servicio en particular, pero que son necesarios para la operación
general de la empresa @CostesIndirectos. Son costes indirectos: el alquier,
la luz, el agua, el Internet etcétera. Véase la @tab:7-costes-indirectos.

#let indirectos = (
  (name: [Luz],      cost: 60),
  (name: [Internet], cost: 50),
  (name: [Agua],     cost: 15),
  (name: [Alquiler], cost: 450))

#let total-indirectos = indirectos.map((x) => x.cost * meses).reduce((a,b)=>a+b)

#figure(
  caption: [Costes indirectos, para #meses meses],
  table(
    columns: (10em, auto, auto),
    align: (left, right, right),
    table.header([Concepto], [Coste], [Total]),
    ..(indirectos.map((x) => (x.name, [$#x.cost$€], [$#(x.cost * meses)$€]))
       .flatten()),
    table.hline(stroke: 0.3pt + black),
    [*Total*], [], [*$#euros(total-indirectos)$€*],
  )
) <tab:7-costes-indirectos>

=== Coste total
#let beneficio = 16
#let iva = 21
#let partial = total-indirectos + personal + total-material
#let total = partial * (beneficio + 100) / 100 * (iva + 100) / 100

El coste del proyecto se considera la suma de todos los costes. El importe
final es el coste del proyecto más el beneficio industrial, que será del
#beneficio%, y el I.V.A. aplicable del #iva%. Como se ve en la
@tab:7-coste-total, el coste del proyecto asciende a los $#euros(partial)$€
y el importe total será de $#euros(total)$€.

#figure(
  caption: [Coste total],
  table(
    columns: (auto, auto),
    align: (left+horizon, right+horizon),
    table.header([*Concepto*], [*Coste*]),

    [Recursos humanos],    [$#euros(personal)$ €],
    [Recursos materiales], [$#euros(total-material)$ €],
    [Costes indirectos],   [$#euros(total-indirectos)$ €],
    table.hline(stroke: 0.3pt + black),
    [*Total del proyecto*],[*$#euros(partial)$ €*],
    table.hline(stroke: 0.3pt + black),
    [Beneficio industrial (#beneficio%)], [$#euros(partial * beneficio/100)$€],
    [IVA (#iva%)], [$#euros(partial * iva/100)$€],
    table.hline(stroke: 0.3pt + black),
    [*Importe final*],     [*$#euros(total)$ €*]
  )) <tab:7-coste-total>

== Impacto socio-económico
El objetivo del proyecto no era crear un algoritmo de detección de ataques
epilépticos original, sino utilizar uno ya existente, estudiarlo y optimizarlo.
De hecho, el algoritmo utilizado es de clasificación, es decir, decide si está
o no en un ataque epiléptico.

Sin embargo, desmostrar que es posible ejecutar un programa tan
computacionalmente costoso como este en tiempo real en un dispositivo empotrado
de bajas características permite indicar que esta clase de detectores son
viables y además, que no tienen por qué consumir muchos recursos. Este estudio
ha sido desarrollado en colaboración la Universidad de Málaga, donde se ideó el
algoritmo original.

El otro objetivo del trabajo fue hacer un análisis comparativo entre distintas
técnicas de programación, lenguajes de programación, arquitecturas... Y
observar cómo mejora o empeora el rendimiento en distintas plataformas.

Utilizar un probador de teoremas junto al código permite, entre otras cosas,
demostrar matemáticamente que el programa no puede fallar: todos los caminos
que dan paso a un error están tratados y por ende el sistema no puede terminar
de manera abrupta. Es un filosofía de programación que, pese a ser
extremadamente costosa, para estos casos tan delicados como es la detección de
ataques epilépticos da paz mental a los pacientes; pues saben que en ningún
momento va a terminar de forma abrupta la aplicación.


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
desigualdades». La epilepsia es una enferemedad que en 2006 afectaba a seis de
cada mil europeos. En muchos países, por ejemplo, está prohibido que conduzcan
personas con epilepsia, exceptuando a aquellos que puedan controlar los posibles
ataques @ElPaísConducirYEpilepsia. Así pues, poder detectar el ataque antes de
que ocurra permite que estas personas puedan reaccionar y, por consiguiente,
conducir como el resto de ciudadanos.

Por último, en relación con el decimotercer objetivo («acción por el clima») y
el séptimo objetivo («energía asequible y no contaminante»), una de las
prioridades del proyecto es que el dispositivo final corra en tiempo real y con
un consumo mínimo. El estudio de los algoritmos, sus optimizaciones y las
técnicas de programación tienen como finalidad la de conseguir un sistema
que sea eficiente además de eficaz.
