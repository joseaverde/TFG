# +-------------------------------------------------------------------------+ #
# | CMakeLists.txt                                                          | #
# |                                                                         | #
# +-------------------------------------------------------------------------+ #

cmake_minimum_required(VERSION 3.18)
project(seizure-algorithm LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} \
   -Ofast -Wno-ignored-optimization-argument")
add_compile_options(-Wall -Wextra -Werror -pedantic -pedantic-errors -g)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
   add_compile_options(-Wno-nan-infinity-disabled)
   # Sanitizer tests:
   #  * -fsanitize=address
   #  * -fsanitize=memory
   #  * -fsanitize=undefined
   #  * -fsanitize=dataflow
   #  * -fsanitize=thread
   # set(CMAKE_CXX_CLANG_TIDY clang-tidy;
   #      -format-style=file;
   #      -header-filter=.;)
   # set(sanitizer -fsanitize=address)
   # add_compile_options(${sanitizer} -fno-omit-frame-pointer -Og -g)
   # add_link_options(${sanitizer})
endif ()

set(SEIZURE_LIBRARIES)

# =========================================================================== #
# ======================== C O N F I G U R A T I O N ======================== #
# =========================================================================== #

option(SEIZURE_CSV_SUPPORT "CSV support" ON)
option(SEIZURE_JSON_SUPPORT "JSON support" ON)

if ("${SEIZURE_MAX_THREADS}" STREQUAL "")
   message(WARNING "Maximum number of threads not specified, defaulting to 64")
   set(SEIZURE_MAX_THREADS 64)
endif()
add_compile_definitions(SEIZURE_MAX_THREADS=${SEIZURE_MAX_THREADS})

if ("${SEIZURE_PARALLEL_MODE}" STREQUAL "")  # sequential openmp thread tbb
   message(WARNING "Parallel mode not specified, defaulting to `thread'")
   set(SEIZURE_PARALLEL_MODE "openmp") # Values: sequential openmp thread tbb
endif()

if (${SEIZURE_CSV_SUPPORT})
   add_compile_definitions(SEIZURE_CSV_SUPPORT=1)
   find_package(rapidcsv REQUIRED CONFIG)
   list(APPEND SEIZURE_LIBRARIES rapidcsv::rapidcsv)
endif()

if (${SEIZURE_JSON_SUPPORT})
   add_compile_definitions(SEIZURE_JSON_SUPPORT=1)
   find_package(nlohmann_json REQUIRED)
   list(APPEND SEIZURE_LIBRARIES nlohmann_json::nlohmann_json)
endif()

set(SEIZURE_PARALLEL_MODE "sequential")
if ("${SEIZURE_PARALLEL_MODE}" STREQUAL "openmp")
   find_package(OpenMP REQUIRED)
   list(APPEND SEIZURE_LIBRARIES OpenMP::OpenMP_CXX)
elseif ("${SEIZURE_PARALLEL_MODE}" STREQUAL "tbb")
   find_package(TBB REQUIRED)
   list(APPEND SEIZURE_LIBRARIES onetbb::onetbb)
elseif ("${SEIZURE_PARALLEL_MODE}" STREQUAL "thread")
elseif ("${SEIZURE_PARALLEL_MODE}" STREQUAL "sequential")
else()
   message(FATAL_ERROR "Unknown parallel mode `${SEIZURE_PARALLEL_MODE}'")
   set(SEIZURE_PARALLEL_MODE "thread")
endif()

# =========================================================================== #
# ============================= P A C K A G E S ============================= #
# =========================================================================== #

find_package(Microsoft.GSL REQUIRED)
find_package(pybind11 REQUIRED)
find_package(FFTW3 REQUIRED)
find_package(benchmark REQUIRED)
find_package(range-v3 REQUIRED)
list(APPEND SEIZURE_LIBRARIES range-v3::range-v3 Microsoft.GSL::GSL)

# =========================================================================== #
# ======================= S U B D I R E C T O R I E S ======================= #
# =========================================================================== #

set(CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR} ${CMAKE_MODULE_PATH})
set(CMAKE_PREFIX_PATH ${CMAKE_BINARY_DIR} ${CMAKE_PREFIX_PATH})

add_subdirectory(src)
add_subdirectory(tests)
# add_subdirectory(modules)
