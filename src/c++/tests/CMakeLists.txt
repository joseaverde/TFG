# find_package(nlohmann_json REQUIRED)

set(COMMON common.hpp seizure_detector_config.hpp)

add_executable(seizure_detector seizure_detector.cpp ${COMMON})
target_link_libraries(seizure_detector PRIVATE Seizure)

add_executable(seizure_detector_benchmark seizure_detector_benchmark.cpp ${COMMON})
target_link_libraries(seizure_detector_benchmark PRIVATE Seizure)

add_executable(seizure_detector_ftests seizure_detector_ftests.cpp ${COMMON})
target_link_libraries(seizure_detector_ftests PRIVATE Seizure)

add_executable(seizure_detector_utests seizure_detector_utests.cpp ${COMMON})
target_link_libraries(seizure_detector_utests PRIVATE Seizure)

add_executable(seizure_detector_simulation seizure_detector_simulation.cpp ${COMMON})
target_link_libraries(seizure_detector_simulation PRIVATE Seizure)


