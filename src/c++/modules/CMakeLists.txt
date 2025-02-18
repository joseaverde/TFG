pybind11_add_module(signals signals.cpp)
target_link_libraries(signals PRIVATE Seizure)
pybind11_add_module(EEGLIB eeglib.cpp)
target_link_libraries(EEGLIB PRIVATE Seizure)
