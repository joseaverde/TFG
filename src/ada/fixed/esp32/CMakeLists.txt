# Source: https://blog.adacore.com/embedded-ada-spark-theres-a-shortcut
cmake_minimum_required(VERSION 3.16)

include($ENV{IDF_PATH}/tools/cmake/project.cmake)
project(seizure_detector)

# Build the Ada code using Alire
execute_process(COMMAND alr build --release -- -XSEIZURE_DETECTOR_TARGET=esp32c3
                WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/../ada_code/")

# Add the Ada code static library
add_library(SeizureDetectorFixed STATIC IMPORTED GLOBAL)
set_property(TARGET SeizureDetectorFixed PROPERTY IMPORTED_LOCATION
   "${CMAKE_SOURCE_DIR}/../lib/release-esp32c3/libSeizureDetectorFixed.a")

# Add the other souce code library
add_library(Binding STATIC IMPORTED GLOBAL)
set_property(TARGET Binding PROPERTY IMPORTED_LOCATION
   "${CMAKE_SOURCE_DIR}/lib/release-esp32c6/libdetector_binding.a")
target_link_libraries(seizure_detector.elf PUBLIC Binding SeizureDetectorFixed)

# Get path of Ada run-time library (libgnat.a)
execute_process(COMMAND bash -c "alr exec -- riscv64-elf-gnatls --RTS=light-rv32imac -v 2>&1 | grep adalib"
                WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/.."
                RESULT_VARIABLE gnatls_result
                OUTPUT_VARIABLE gnatls_output)

string(STRIP "${gnatls_output}" ada_runtime_dir)
message(STATUS "Link Ada run-time ${ada_runtime_dir}/libgnat.a")
add_library(libgnat STATIC IMPORTED GLOBAL)
set_property(TARGET libgnat PROPERTY IMPORTED_LOCATION "${ada_runtime_dir}/libgnat.a")

set(GNAT "${CMAKE_BINARY_DIR}/libgnat.a")
 
add_custom_command(
   OUTPUT  ${GNAT}
   COMMAND ${CMAKE_COMMAND} -E copy "${ada_runtime_dir}/libgnat.a" "${GNAT}"
   COMMAND ar d "${GNAT}" "s-macres.o"
   DEPENDS libgnat
   COMMENT "====================="
   VERBATIM)

add_custom_target(stripped_libgnat DEPENDS ${GNAT})
add_library(the_real_stripped_libgnat STATIC IMPORTED)
set_property(TARGET the_real_stripped_libgnat PROPERTY IMPORTED_LOCATION
   "${GNAT}")

# Add the Ada run-time static library
add_dependencies(seizure_detector.elf stripped_libgnat)
target_link_libraries(seizure_detector.elf PUBLIC the_real_stripped_libgnat)
