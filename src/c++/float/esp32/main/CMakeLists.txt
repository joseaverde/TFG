include(FetchContent)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(NOT CMAKE_BUILD_EARLY_EXPANSION)
   FetchContent_Declare(GSL
       GIT_REPOSITORY "https://github.com/microsoft/GSL"
       GIT_TAG        "v4.0.0"
       GIT_SHALLOW    ON
   )
   FetchContent_MakeAvailable(GSL)
 
   FetchContent_Declare(range_v3
      GIT_REPOSITORY "https://github.com/ericniebler/range-v3"
      GIT_TAG        "0.12.0"
      GIT_SHALLOW    ON
   )
   FetchContent_MakeAvailable(range_v3)
endif()

idf_component_register(SRCS "main.cpp"
                       INCLUDE_DIRS ".")

target_link_libraries(${COMPONENT_LIB} Microsoft.GSL::GSL
                                       range-v3::range-v3)
