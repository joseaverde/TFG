cmake_minimum_required(VERSION 3.16)

include($ENV{IDF_PATH}/tools/cmake/project.cmake)
project(seizure_detector)

add_library(detector STATIC IMPORTED GLOBAL)
set_property(TARGET detector PROPERTY IMPORTED_LOCATION
   "${CMAKE_SOURCE_DIR}/../lib/release-esp32c3-float32-s256-e5/libdetector.a")
target_link_libraries(seizure_detector.elf PRIVATE detector)
# 
# Get path of Ada run-time library (libgnat.a)
# # # execute_process(COMMAND bash -c "alr exec -- riscv64-elf-gnatls --RTS=light-rv32imac -v 2>&1 | grep adalib"
# # #                 WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/.."
# # #                 RESULT_VARIABLE gnatls_result
# # #                 OUTPUT_VARIABLE gnatls_output)

# # # string(STRIP "${gnatls_output}" ada_runtime_dir)
set(ada_runtime_dir ${CMAKE_SOURCE_DIR})
# 
# # Add the Ada run-time static library
message(STATUS "Link Ada run-time ${ada_runtime_dir}/libgnat.a")
add_library(libgnat STATIC IMPORTED GLOBAL)
set_property(TARGET libgnat PROPERTY IMPORTED_LOCATION "${ada_runtime_dir}/libgnat.a")
target_link_libraries(seizure_detector.elf PRIVATE libgnat)
