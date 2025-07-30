# /-------------------------------------------------------------------------\ #
# | Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved | #
# |-------------------------------------------------------------------------| #
# | File:    CMakeLists.txt                                                 | #
# | Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        | #
# | License: European Union Public License 1.2                              | #
# \-------------------------------------------------------------------------/ #

function (copy_file path dest_dir)
   cmake_path(GET path FILENAME filename)
   cmake_path(APPEND dest_dir ${filename} OUTPUT_VARIABLE output)
   add_custom_command(
      OUTPUT  ${output}
      COMMAND ${CMAKE_COMMAND} -E copy ${path} ${output}
      DEPENDS ${path}
      VERBATIM)
endfunction()

function (list_to_string output_variable lst separator nexus first last)
   list(LENGTH lst length)
   set(result "${first}")
   if ("${length}" EQUAL "0")
   elseif ("${length}" EQUAL "1")
      list(GET lst 0 item)
      string(APPEND result item)
   else ()
      math(EXPR end1 "${length} - 1")
      math(EXPR end2 "${length} - 2")
      if (NOT "${length}" EQUAL "2")
         math(EXPR end "${length} - 3")
         foreach (i RANGE 0 ${end})
            list(GET lst ${i} item)
            string(APPEND result ${item} "${separator}")
         endforeach()
      endif()
      list(GET lst ${end2} item)
      string(APPEND result "${item}")
      string(APPEND result "${nexus}")
      list(GET lst ${end1} item)
      string(APPEND result "${item}")
   endif()
   string(APPEND result "${last}")
   set(${output_variable} "${result}" PARENT_SCOPE)
endfunction()

function (enum_param variable default message)
   set(result "${default}" CACHE STRING "${message} Valid values are ${params}")
   list_to_string(params "${ARGN}" ", " " and " "" "")
   if ("${result}" STREQUAL "")
      set(result "$ENV{${variable}}")
   endif()

   if (NOT "${result}" IN_LIST ARGN)
      set(msg "Invalid value for variable ${variable} valid values are: ")
      message(FATAL_ERROR "${msg}${params}")
   endif()
   set(${variable} "${result}" PARENT_SCOPE)
endfunction()

function (int_param variable default message)
   set(result "${default}" CACHE STRING "${message}")
   if ("${result}" STREQUAL "")
      set(result "$ENV{${variable}}")
   endif()

   if ("${result}" STREQUAL "")
      message(FATAL_ERROR "Value for ${variable} is missing!")
   endif()
   set(${variable} "${result}" PARENT_SCOPE)
endfunction()

function (get_libgnat_path output_variable)
   # Source: https://blog.adacore.com/embedded-ada-spark-theres-a-shortcut
   execute_process(
      COMMAND           bash -c "cd \"${CMAKE_SOURCE_DIR}\" && alr exec -- riscv64-elf-gnatls --RTS=light-rv32imac -v 2>&1 | grep adalib"
      WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/.."
      RESULT_VARIABLE   gnatls_result
      OUTPUT_VARIABLE   gnatls_output)
   string(STRIP "${gnatls_output}" ada_runtime_dir)
   set(${output_variable} "${ada_runtime_dir}/libgnat.a" PARENT_SCOPE)
endfunction()

function (ada_get_libdetector_path output_variable)
   cmake_path(APPEND lang_dir "ada" ${DETECTOR_REAL} OUTPUT_VARIABLE ada_path)
   cmake_path(APPEND ada_path "lib" OUTPUT_VARIABLE lib_dir)
   cmake_path(APPEND lib_dir "release-${DETECTOR_TARGET}" OUTPUT_VARIABLE lib_dir)
   cmake_path(APPEND lib_dir "libdetector.a" OUTPUT_VARIABLE lib_path)
   set(${output_variable} ${lib_path} PARENT_SCOPE)
endfunction()

cmake_path(GET CMAKE_SOURCE_DIR PARENT_PATH root_dir)
cmake_path(GET root_dir PARENT_PATH root_dir)
cmake_path(GET root_dir PARENT_PATH lang_dir)

enum_param(DETECTOR_LANGUAGE ""
   "The programming language the detector library is written in."
   "Ada" "C++")
enum_param(DETECTOR_EXECUTABLE "application" 
   "The executable to build"
   "application" "benchmark" "tests" "module")
enum_param(DETECTOR_REAL ""
   "The real type to use"
   "fixed" "single" "double")
enum_param(DETECTOR_TARGET ""
   "The target for the detector"
   "native" "rpi3" "rpi4" "esp32c3" "esp32c6")
enum_param(DETECTOR_PROFILE ""
   "The profile"
   "ReleaseWithoutChecks" "Release" "Validation" "Development")

enum_param(DETECTOR_SAMPLE_SIZE ""
   "The size of a sample in bits"
   "32" "64")
enum_param(DETECTOR_FEATURE_SIZE ""
   "The size of a feature in bits"
   "32" "64")
int_param(SAMPLES_PER_STRIDE ""
   "The number of samples there are in a stride")
int_param(STRIDES_PER_EPOCH ""
   "The number of strides there are in a epoch")
int_param(PATTERN_COUNT ""
   "The maximum number of patterns the batch can have")
