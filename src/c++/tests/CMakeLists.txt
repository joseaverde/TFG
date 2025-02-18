find_package(nlohmann_json REQUIRED)

add_executable(test-validation test-validation.cpp utils.hpp)
target_link_libraries(test-validation
   PRIVATE Seizure
      pybind11_all_do_not_use nlohmann_json::nlohmann_json rapidcsv::rapidcsv)

add_executable(energy-benchmark energy-benchmark.cpp random_walk.hpp)
target_link_libraries(energy-benchmark PRIVATE Seizure)

if (NOT ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang"))
   find_package(Boost REQUIRED)
   add_executable(esp32 esp32.cpp utils.hpp)
   target_link_libraries(esp32
      PRIVATE Seizure
         pybind11_all_do_not_use nlohmann_json::nlohmann_json rapidcsv::rapidcsv
         boost::boost)
endif()
