#-/-------------------------------------------------------------------------\-#
#-| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |-#
#-|-------------------------------------------------------------------------|-#
#-| File:    CMakeLists.txt                                                 |-#
#-| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |-#
#-| License: European Union Public License 1.2                              |-#
#-\-------------------------------------------------------------------------/-#

cmake_minimum_required(VERSION 3.22)
project(detector C)

set(targets "native" "rpi3" "rpi4" "esp32c3" "esp32c6" "esp32s3")
set(esp_targets "esp32c3" "esp32c6" "esp32s3")
set(languages "ada" "cxx")
set(reals "fixed" "single" "double")
set(profiles "release" "release-with-checks" "validation" "development")

if(NOT "${DETECTOR_TARGET}" IN_LIST targets)
   message(FATAL_ERROR "Invalid target `${DETECTOR_TARGET}'")
endif()

if(NOT "${DETECTOR_LANGUAGE}" IN_LIST languages)
   message(FATAL_ERROR "Invalid language `${DETECTOR_TARGET}'")
endif()

if(NOT "${DETECTOR_REAL}" IN_LIST reals)
   message(FATAL_ERROR "Invalid real type `${DETECTOR_REAL}'")
endif()

if(NOT "${DETECTOR_PROFILE}" IN_LIST profiles)
   message(FATAL_ERROR "Invalid profile `${DETECTOR_PROFILE}'")
endif()

# If language is Ada we need to import

if("${TARGET}" IN_LIST targets)
    # Include for ESP-IDF build system functions
    include($ENV{IDF_PATH}/tools/cmake/idf.cmake)
    # Create idf::{target} and idf::freertos static libraries
    idf_build_process("${TARGET}"
                    # try and trim the build; additional components
                    # will be included as needed based on dependency tree
                    #
                    # although esptool_py does not generate static library,
                    # processing the component is needed for flashing related
                    # targets and file generation
                    COMPONENTS freertos esptool_py
                    SDKCONFIG ${CMAKE_CURRENT_LIST_DIR}/sdkconfig
                    BUILD_DIR ${CMAKE_BINARY_DIR})
else()
    message(WARNING "Unknown target ${TARGET}, creating stubs for esp32 instead")
    # Create stubs for esp32 and freertos, stub::esp32 and stub::freertos
    add_subdirectory(stubs/esp32)
    add_subdirectory(stubs/freertos)
    add_subdirectory(stubs/spi_flash)
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(elf_file ${CMAKE_PROJECT_NAME}.elf)
add_executable(${elf_file} main.c)

# Link the static libraries to the executable
if("${TARGET}" IN_LIST targets)
    target_link_libraries(${elf_file} idf::freertos idf::spi_flash)
    # Attach additional targets to the executable file for flashing,
    # linker script generation, partition_table generation, etc.
    idf_build_executable(${elf_file})
else()
    target_link_libraries(${elf_file} stub::esp32 stub::freertos stub::spi_flash)
endif()
