// @file signals.cpp
// @brief Binding to use signal-related functions from Python
// @author José Antonio Verde Jiménez

#include "pyutils.hpp"

#include <seizure>

namespace {

  // Types

  using Real         = double;
  using Channel      = std::vector<Real>;
  using Py_channel   = pybind11::array_t<Real>;
  using Py_pair      = std::tuple<Real, Real>;
  using Py_ranges    = std::vector<Py_pair>;
  using Sample_count = Seizure::Sample_count;

  // Wrappers

  void call_checks(Py_channel const & signal, Sample_count mw, Sample_count stride,
                   Sample_count ops) {
    if (ops == 0) { throw pybind11::value_error{"Operations must be positive"}; }
    if (stride * (ops - 1) + mw > std::ssize(signal)) {
      throw pybind11::value_error{"Too many operations"};
    }
  }

  Real simpson(Py_channel const & y, Real dx) {
    if (std::ssize(y) <= 2) { throw pybind11::value_error{"Lenght must be greater than 2"}; }
    return Seizure::simpson(pyspan(y), dx);
  }

  std::tuple<Py_channel, Py_channel> welch(Py_channel const & y, Real fs, Sample_count win,
                                           Sample_count overlap) {
    if (std::ssize(y) < win) {
      throw pybind11::value_error{"Length must be at least the window's size"};
    }
    if (overlap <= 0 || overlap >= win) { throw pybind11::value_error{"Invalid overlap"}; }
    Real step{};
    Real stop{};
    Seizure::Welch<Real, Seizure::Windows::hann> welch_func{win};
    welch_func.freq_range(fs, step, stop);
    auto result = welch_func(pyspan(y), fs, overlap);
    Channel freqs(std::ssize(result));
    for (gsl::index i = 0; i < std::ssize(result); ++i) { freqs[i] = step * static_cast<double>(i); }
    return {pymove(std::move(freqs)), pymove(std::move(result))};
  }

  // NOLINTNEXTLINE (readablity-function-size)
  std::vector<Py_channel> call_psd(Py_channel const & signal, Sample_count mw,
                                   Sample_count stride, Sample_count ops,
                                   Py_ranges const & ranges) {
    call_checks(signal, mw, stride, ops);
    Seizure::PSD_params<Real, std::vector<std::pair<Real, Real>>> params;
    params.window_size = Seizure::psd_default_ranges<Real>().window_size;
    params.overlap = Seizure::psd_default_ranges<Real>().overlap;
    params.sampling_freq = Seizure::psd_default_ranges<Real>().sampling_freq;
    if (mw < params.window_size) {
      throw pybind11::value_error{"Window must be at least " +
                                  std::to_string(params.window_size)};
    }
    for (auto const & range : ranges) {
      params.ranges.emplace_back(std::get<0>(range), std::get<1>(range));
    }
    auto result{Seizure::call_psd(pyspan(signal), mw, stride, params)};
    std::vector<Py_channel> py_result(std::ssize(result));
    for (gsl::index i = 0; i < std::ssize(result); ++i) { py_result[i] = pymove(std::move(result[i])); }
    return py_result;
  }

  Py_channel call_max_dist(Py_channel const & signal, Sample_count mw, Sample_count stride,
                           Sample_count ops) {
    call_checks(signal, mw, stride, ops);
    return {pymove(Seizure::call_max_dist(pyspan(signal), mw, stride))};
  }

  Py_channel call_energy(Py_channel const & signal, Sample_count mw, Sample_count stride,
                         Sample_count ops) {
    call_checks(signal, mw, stride, ops);
    return {pymove(Seizure::call_energy(pyspan(signal), mw, stride))};
  }

  // NOLINTBEGIN(readability-function-size)
  using Calls = std::tuple<Py_channel, Py_channel, Py_channel, Py_channel, Py_channel>;
  // NOLINTNEXTLINE (clang-diagnostic-ignored-optimization-argument)
  std::tuple<Calls, Calls> optimization_step_begin(Py_channel const & data_all, Sample_count stride,
                                                   Sample_count ops, Py_channel const & query_all,
                                                   Sample_count qstride, Sample_count qops,
                                                   Sample_count mw) {
    call_checks(data_all, mw, stride, ops);
    call_checks(query_all, mw, qstride, qops);
    auto data_span  = pyspan(data_all);
    auto query_span = pyspan(query_all);
    auto data_psd   = Seizure::call_psd(data_span, mw, stride, Seizure::psd_default_ranges<Real>());
    auto query_psd  = Seizure::call_psd(query_span, mw, qstride, Seizure::psd_default_ranges<Real>());

    return {
        { pymove(std::move(data_psd[0])),  pymove(std::move(data_psd[1])),
         pymove(std::move(data_psd[2])),  pymove(Seizure::call_energy(data_span,  mw, stride)),
         pymove(Seizure::call_max_dist(data_span, mw, stride)) },
        {pymove(std::move(query_psd[0])), pymove(std::move(query_psd[1])),
         pymove(std::move(query_psd[2])), pymove(Seizure::call_energy(query_span, mw, qstride)),
         pymove(Seizure::call_max_dist(query_span, mw, qstride))}
    };
  }
  // NOLINTEND(readability-function-size)

  // Module

  PYBIND11_MODULE(signals, m) {
    m.doc() = "Signals library for Seizure Algorithm";
    m.def("simpson", simpson, pybind11::arg("y"), pybind11::arg("dx"));
    m.def("welch", welch);
    m.def("call_psd", call_psd);
    m.def("call_max_dist", call_max_dist);
    m.def("call_energy", call_energy);
    m.def("begin", optimization_step_begin, pybind11::arg("data_all"), pybind11::arg("stride"),
          pybind11::arg("ops"), pybind11::arg("query_all"), pybind11::arg("qstride"),
          pybind11::arg("qops"), pybind11::arg("mw"));
  }

}  // namespace
