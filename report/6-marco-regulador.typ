= Marco regulador
En este capítulo se hace un breve recorrido sobre la legislación aplicable al
proyecto en la @sec:6.1; se enumera y se describe resumidamente las licencias
de las dependencias de los componentes del proyecto en la @sec:6.2; y
finalmente se comenta qué estándares técnicos más importantes se han utilizado
a lo largo del desarrollo del propio proyecto en la @sec:6.3.

== Legislación aplicable <sec:6.1>
Pese a que se trata de datos de personas reales, estos datos están anonimizados
y el _dataset_ del que proviene es libre de uso @CHBMIT @physionet
@ML-Seizure-Detection @ODC-By-1-0. Las dependencias de enlazado del proyecto son todas de código
abierto y son compatibles con la
licencia que le ha puesto el autor a este proyecto: la *EUPL-1.2*.

== Licencias de _software_ <sec:6.2>
Dado que el programa depende de bastantes bibliotecas externas, es preciso
definir primero las licencias y luego cómo depende cada _software_ de otros.
Se ofrece una breve descripción de cada una de ellas. Sin embargo, es
preferible que el lector lea el texto oficial.

- *EUPL-1.2*: _European Union Public License_ es la primera licencia libre de
  código abierto de la Unión Europea que tiene tiene validez en todos los
  idiomas oficiales de la misma @EUPL-1-2. Esta se puede ver en
  #link("https://interoperable-europe.ec.europa.eu/collection/eupl/eupl-text-eupl-12")
  en las veintitrés lenguas oficiales. Es compatible con las licencias:
  GPLv2, GPLv3, AGPLv3, LGPLv2.1, LGPLv3, CC BY-SA 3.0, MIT, BSD, entre otras
  @EUPLmatrix.

  A continuación un breve resumen de las características de la misma.
  Otorga derechos a: utilizar, reproducir, modificar, realizar obras derivadas,
  distribuir, prestar y alquilar, entre otras. El código fuente debe
  facilitarse de forma gratuita. Sin embargo el licenciatario está obligado a:
  atribuir, que obras derivadas o distribución de la misma tenga una licencia
  de _copyleft_ entre otras.

- *MIT*: Es la licencia libre de código abierto del Instituto Tecnológico de
  Massachussetts. El _software_ licenciado con la misma viene in garantía, pero
  no tiene restricciones en cuanto a: copiar, modificar, fusionar, publicar,
  distribuir, sublicenciar ni vender copias del mismo; siempre y cuando incluya
  copias de la propia licencia @MIT.

- *BSD-3-Clause*: La licencia BSD (_Berkeley Software Distribution_) modificada
  de 3 cláusuras es una licencia libre de código abierto, que como su nombre
  indica, contiene tres cláusuras:
  1. La redistribución del código fuente debe mantener el aviso de _copyright_
     y el descargo de responsabilidad.
  2. La redistribución en formato binario debe mantener el aviso de
     _copyright_ y el descargo de responabilidad en la documentación; y además
     otros materiales suministrados con la distribución.
  3. No se puede utilizar el nombre de los titulares de los derechos de autor
     para promocionar productos derivados sin su consentimiento @BSD-3-Clause.

- *Apache License v2.0*: Es una licencia permisiva que requiere que se mantenga
  el aviso de _copyright_ y la licencia. Contribuidores ceden automáticamente
  los derechos de la patente en sus contibuciones. Trabajos licenciados y
  modificaciones se pueden distribuir bajo otras condiciones @Apache2.

- *BSL-1.0*: _Boost Software License -- Version 1.0_ es una licencia permisiva
  que únicamente requiere que se mantenga el aviso de _copyright_ y la licencia
  para la distribución del código fuente (no del binario). Se puede distribuir
  bajo otros términos modificaciones y trabajos licenciados @BSL-1-0.

- *GPLv3*: _GNU Public License -- Version 3 _ es una licencia de _copyleft_
  para _software_ libre y de código abierto, que requiere a los usuarios de
  _software_ licenciado bajo esta licencia a hacer disponible el código fuente
  completo bajo la misma licencia: la *GPLv3*. Se debe mantenere el aviso de
  _copyright_ y la licencia. Además los contribuidores ceden los derechos sobre
  sus contribuciones automáticamente @GPLv3.

- *GPLv3 Runtime Library Exception*: Tiene las mismas condiciones que la
  *GPLv3*, pero añade una excepción a _software_ que enlace con la biblioteca,
  es decir, el _software_ que se ha enlazado no con dicha biblioteca no tiene
  por qué cambiar su licencia @GPLv3RLE.


El programa de referencia que se encuentra en GitHub
#link("https://github.com/PPMC-DAC/PaFESD-Epileptic-Seizure-Detection") tiene
como licencia *MIT*.

Se diferencia entre las dependencias del programa escrito en C++ y el escrito
en SPARK y Ada, como si fueran programas distintos. Las dependencias del
proyecto son las siguientes:

- *C++*
  - *Desarrollo*: los siguientes programas se han utilizado para desarrollar la
    aplicación, ninguno de ellos impone ningún tipo de restricción sobre la
    salida que generan. Es decir, el código compilado con GCC o clang en forma
    de objeto no tiene ninguna restricción, el único problema es a la hora de
    enlazar.
    - `conan`: Tiene como licencia *MIT* @License-conan. Se utiliza para
      gestionar las dependencias.
    - `CMake`: Tiene como licencia *BSD-3-Clause* @License-CMake.  Se utiliza
      junto a `conan` para gestionar el proceso de compilación y enlazar con
      las dependencias de manera correcta.
    - `GCC`: Tiene como licencia *GPLv3* @License-GCC. Se utiliza para
      convertir el código fuente en código objeto (compilar) y posteriormente
      enlazar.
    - `clang`: Tiene como licencia *Apache License v2.0 with LLVM Exceptions*,
       que además de las condiciones de la licencia *Apache License v2.0*,
       añade una excepción: si al compilar el programa, porciones del código
       fuente de `clang` acabaran en dicho código objeto, este queda exenpto de
       las cláusuras 4(a), 4(b) y 4(d) de *Apache License v2.0* @License-llvm.
    - `benchmark/1.8.3`: Tiene la licencia *Apache-2.0* y se utiliza para
       hacer pruebas de rendimiento en el código @License-benchmark. Se utiliza
       para desarrollo y no va enlazado en el ejecutable final.
  - *Bibliotecas*: las bibliotecas son colecciones de subrutinas con las que se
    enlaza el código fuente para obtener ciertas funcionalidades. En este caso,
    como las bibliotecas se empaquetan junto al ejecutable final, este tiene
    que adherirse a las restricciones de uso de sendas licencias.
    - `libstdc++`: Tiene la licencia *GPLv3 Runtime Library Exception*,
      implementa la biblioteca estándar de C++ y es necesaria a la hora de
      enlazar el programa. Tiene la excepción como biblioteca en tiempo de
      ejecución (_runtime library exception_), así que no hay problema al
      enlazar con ella @License-stdcxx.
    - `newlib` (Espressif): Esta solamente tiene sentido a la hora de enlazar
      el código fuente en los dispositivos empotrados de Espressif (ESP32C3,
      ESP32C6, ESP32S3) y tiene la licencia *BSD* @EspressifLicense.
    - `ms-gsl/4.0.0`: Tiene la licencia *MIT* @License-ms-gsl. Se utiliza para
      dar soporte a las _#box([C++]) Core Guidelines_.
    - `rapidcsv/8.80`: Tiene la licencia *BSD-3-Clause* @License-rapidcsv. Se
      utiliza para abrir archivos C.S.V.
    - `range-v3/0.12.0`: Tiene la licencia *BSL-1.0* @License-range-v3. Se
      utiliza para hacer operaciones con rangos más complejas que lo que
      permite la biblioteca estándar.
    - `onetbb/2021.12.0`: Está licencia bajo *Apache-2.0* @License-onetbb. Se
      utiliza para implementar ciertos algoritmos de forma paralela.
    - `pybind11/2.10.4`: Tiene la licencia *BSD-3-Clause* @License-pybind11. Se
      utiliza para crear el módulo para que Python3 pueda ejecutar funciones
      escritas en C++.
    - `boost/1.86.0`: Tiene la licencia *BSL-1.0* @License-boost. Se utiliza
      para lecturas y escrituras asíncronas de puertos seriales.
    - `PaFESD-Epileptic-Seizure-Detection`: Es el _software_ de referencia y
      está bajo la licencia *MIT* @License-PaFESD. De él solo se utiliza su
      implementación de la función de la deformación dinámica del tiempo.

- *Ada*
  - *Desarrollo*: Las siguientes herramientas se utilizaron únicamente para
    desarrollar la aplicación, no se enlaza con ninguna de ellas, así que no
    hay problemas de licencias.
    - `GNAT`: Es parte de la colección de GCC @License-GCC y está bajo la misma
      licencia, la *GPLv3* @License-GNAT @License-GNAT-ARM
      @License-GNAT-RISC-V. Se utiliza para convertir código fuente de Ada a
      código objeto.
    - `gprbuild`: Bajo la licencia *GPLv3 Runtime Library Exception*
      @License-gprbuild. Se utiliza de manera parecida a `cmake` para organizar
      la compilación y enlazado de múltiples archivos de código fuente de Ada,
      C y C++.
    - `alire`: Está bajo la *GPLv3* @License-alire. Es un gestor de paquetes
      similar a `conan` para gestionar dependencias y _toolchains_ en Ada.
    - `spark2014`: Está bajo la *GPLv3* @License-SPARK. Contiene el programa
      `gnatprove` que permite correr distintos probadores de teoremas en el
      código de Ada para verificar propiedades del mismo. Además de una
      biblioteca con lemas y teoremas que no genera código, para ayudar al
      desarrollador.
  - *Bibliotecas*:
    - `libgnat`: Es la biblioteca que implementa la biblioteca estándar de Ada
      para GNAT en GCC, tiene la licencia *GPLv3 Runtime Library Exception*
      @License-GNAT @License-GNAT-ARM @License-GNAT-RISC-V; así que no hay
      ningún problema al enlazar con ella.
    - `newlib` (Espressif): La misma que en las bibliotecas de C++, tiene la
      licencia *BSD* @EspressifLicense. `libgnat` necesita ciertos símbolos que
      exporta `newlib` para entrada/salida y para arrancar la placa.

== Estándares técnicos <sec:6.3>
=== C++23 (ISO/IEC 14882:2024)
#box([C++]) es un lenguaje de programación estandarizado por ISO (_International
Organization of Standarization_) y en el que está escrita la mayor parte del
código fuente del proyecto. Una revisión del estándar se hace cada tres años,
y recibe un nombre desde #box([C++11]) (primera revisión del que se considera
#box([C++]) moderno), #box([C++14]), #box([C++17]), #box([C++20]) y
#box([C++23]) (la última versión ratificada). Además ya se está preparando la
siguiente revisión que se denominará en un principio #box([C++26]).

Es un lenguaje de programación de propósito general originalmente basado en el
lenguaje de programación C, pero del que se ha ido alejando a lo largo de los
años. Añade tipos de datos adicionales, clases, _templates_ (plantillas, para
genericidad), excepciones, espacios de nombres (_namespaces_), sobrecarga de
operadores y de funciones, referencias y una biblioteca adicional de utilidades.
@ISOCXX2023

=== Ada 2022 (ISO/IEC 8652:2023)
Ada es otro lenguaje de programación estandarizado por ISO, en el que se
describe la forma y el significado de los programas escritos en dicho lenguaje,
para promover la portabilidad de programas escritos en Ada a variedad de
sistema de cómputo. Especifica además, una biblioteca estandarizada que debe
ser suministrada junto a la distribución @ISOAda2022. En el proyecto se utiliza
para la parte de verificación formal.
