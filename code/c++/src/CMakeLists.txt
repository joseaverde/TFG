# +-------------------------------------------------------------------------+ #
# | CMakeLists.txt                                                          | #
# |                                                                         | #
# +-------------------------------------------------------------------------+ #

add_library(Seizure INTERFACE)
target_include_directories(Seizure
   INTERFACE
      "${CMAKE_CURRENT_LIST_DIR}"
      "${CMAKE_CURRENT_LIST_DIR}/${SEIZURE_PARALLEL_MODE}")


target_sources(Seizure
   PUBLIC
      seizure
   PRIVATE
      algorithms.hpp
      batch.hpp
      concepts.hpp
      detector.hpp
      dtw.hpp
      fftw++.hpp
      metadata.hpp
      serial.hpp
      signals.hpp
      types.hpp
      validator.hpp
      views.hpp
      welch.hpp
)

target_link_libraries(Seizure INTERFACE ${SEIZURE_LIBRARIES})
