// @file signals.cpp
// @brief Binding to use DTW functions from Python
// @author José Antonio Verde Jiménez

#include "pyutils.hpp"

#include <seizure>

// NOLINTBEGIN (bugprone-easily-swappable-parameters)
// The old Python interface requieres the types to be of the same type and in
// that order.

namespace {

  // Types

  using Real         = double;
  using Channel      = std::vector<Real>;
  using Py_channel   = pybind11::array_t<Real>;
  using Sample_count = Seizure::Sample_count;

  // Wrappers

  Py_channel GetDistMtx(Py_channel const & S, Sample_count /* nS */, Py_channel const & Q,
                        Sample_count /* nQ */, Sample_count sEpoPat, Sample_count stride,
                        Sample_count warping_window, bool /* verbose */) {
    return {pymove(Seizure::get_distance_matrix<Seizure::DTW_method::Partial_euclidean_distance>(
        pyspan(S), pyspan(Q),
        {.sEpoPat = sEpoPat, .stride = stride, .warping_window = warping_window}))};
  }

  Py_channel GetDistMtxU(Py_channel const & S, Sample_count /* nS */, Py_channel const & Q,
                         Sample_count /* nQ */, Sample_count sEpoPat, Sample_count stride,
                         Sample_count warping_window, bool /* verbose */) {
    return {pymove(Seizure::get_distance_matrix<Seizure::DTW_method::Full_euclidean_distance_symmetric_2>(
        pyspan(S), pyspan(Q),
        {.sEpoPat = sEpoPat, .stride = stride, .warping_window = warping_window}))};
  }

  Py_channel GetDistMtxUNS(Py_channel const & S, Sample_count /* nS */, Py_channel const & Q,
                           Sample_count /* nQ */, Sample_count sEpoPat, Sample_count stride,
                           Sample_count warping_window, bool /* verbose */) {
    return {pymove(Seizure::get_distance_matrix<Seizure::DTW_method::Partial_euclidean_distance_symmetric_2>(
        pyspan(S), pyspan(Q),
        {.sEpoPat = sEpoPat, .stride = stride, .warping_window = warping_window}))};
  }

  // Module

  PYBIND11_MODULE(EEGLIB, m) {
    m.doc() = "Signals library for Seizure Algorithm";
    m.def("GetDistMtx", GetDistMtx);
    m.def("GetDistMtxU", GetDistMtxU);
    m.def("GetDistMtxUNS", GetDistMtxUNS);
  }
}  // namespace

// NOLINTEND (bugprone-easily-swappable-parameters)
