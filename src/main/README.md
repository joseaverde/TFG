Main Program
============
There are multiple configurations for building the program:

* Fixed point or single or double precision floating point.
* Debug, Release or Validation mode
* Ada or C++
* Clang or GCC

And there are multiple targets:

* Native
* Raspberry Pi 3
* Raspberry Pi 4
* ESP32C3
* ESP32C6
* ESP32S3

And there are multiple executables:

* Benchmark
* Application
* Python Module
* Tests

It is an _n×m_ problem. This `conan` project, alongside the different profiles
provided in the [profiles/](profiles) directory. Allows to test multiple
configurations without needing to change many things in the program.

API
---
In order to remain parcial, we are going to use the C programming language for
the main program. All the libraries must provide a common API which is
described in the file [include/detector.h](detector.h).
