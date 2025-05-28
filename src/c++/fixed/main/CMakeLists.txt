include(FetchContent)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(NOT CMAKE_BUILD_EARLY_EXPANSION)
   add_compile_options(${COMPONENT_LIB} -O2)
   FetchContent_Declare(CNL
       GIT_REPOSITORY "https://github.com/johnmcfarlane/cnl"
       GIT_TAG        "v2.0.2-development"
       GIT_SHALLOW    ON
   )
   FetchContent_MakeAvailable(CNL)
endif()

idf_component_register(SRCS "main.cpp" "utils.c"
                       INCLUDE_DIRS ".")

target_link_libraries(${COMPONENT_LIB} Cnl)
