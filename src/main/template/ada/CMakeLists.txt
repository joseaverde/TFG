# /-------------------------------------------------------------------------\ #
# | Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved | #
# |-------------------------------------------------------------------------| #
# | File:    CMakeLists.txt                                                 | #
# | Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        | #
# | License: European Union Public License 1.2                              | #
# \-------------------------------------------------------------------------/ #

cmake_minimum_required(VERSION 3.16)
project(detector C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include($ENV{IDF_PATH}/tools/cmake/idf.cmake)
include("../../utils.cmake")

# /=== Some checks =========================================================/ #

set(INVALID_DETECTOR_EXECUTABLES "tests" "module")
if (${DETECTOR_EXECUTABLE} IN_LIST INVALID_DETECTOR_EXECUTABLES)
   message(FATAL_ERROR "Executable ${DETECTOR_EXECUTABLE} is not valid in this configuration (IDF)")
endif ()

# /=== Compilation =========================================================/ #

set(elf_file ${CMAKE_PROJECT_NAME}.elf)

copy_file("${root_dir}/src/application.c" "${CMAKE_SOURCE_DIR}/")
copy_file("${root_dir}/src/benchmark.c" "${CMAKE_SOURCE_DIR}/")
copy_file("${root_dir}/src/detector.h" "${CMAKE_SOURCE_DIR}/")

add_compile_definitions(-DDETECTOR_SAMPLE_SIZE=${DETECTOR_SAMPLE_SIZE})
add_compile_definitions(-DDETECTOR_FEATURE_SIZE=${DETECTOR_FEATURE_SIZE})
add_compile_definitions(-DSAMPLES_PER_STRIDE=${SAMPLES_PER_STRIDE})
add_compile_definitions(-DSTRIDES_PER_EPOCH=${STRIDES_PER_EPOCH})
add_compile_definitions(-DPATTERN_COUNT=${PATTERN_COUNT})
add_compile_definitions(-DIDF_PROJECT)

if ("${DETECTOR_LANGUAGE}" STREQUAL "Ada")
   execute_process(COMMAND bash -c "alr build")

   get_libgnat_path(libgnat_path)
   add_library(libgnat STATIC IMPORTED GLOBAL)
   set_property(TARGET libgnat PROPERTY IMPORTED_LOCATION "${libgnat_path}")

   ada_get_libdetector_path(libdetector_path)
   add_library(libdetector STATIC IMPORTED GLOBAL)
   set_property(TARGET libdetector PROPERTY IMPORTED_LOCATION "${libdetector_path}")
endif()

# /=== IDF Boilerplate =====================================================/ #

set(TARGET "esp32c6")
idf_build_process("${TARGET}"
                COMPONENTS freertos esptool_py
                SDKCONFIG ${CMAKE_CURRENT_LIST_DIR}/sdkconfig
                BUILD_DIR ${CMAKE_BINARY_DIR})

add_executable(${elf_file} "${DETECTOR_EXECUTABLE}.c" detector.h)
target_link_libraries(${elf_file} idf::freertos idf::spi_flash libdetector libgnat)
idf_build_executable(${elf_file})
