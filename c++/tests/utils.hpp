#include <pybind11/embed.h>
#include <pybind11/numpy.h>
#include <nlohmann/json.hpp>
#include <iostream>

constexpr int samplerate = 256;

template <typename Real>
auto read_scv(bool clean_signal, auto const & chbmit, auto const & patient,
              auto const & json_path) {
  std::vector<Real> result{};
  {
    std::ifstream file(json_path);
    if (not file) { return result; }
    nlohmann::json json = nlohmann::json::parse(file);
    pybind11::scoped_interpreter python{};
    pybind11::array_t<Real> channel;
    auto sys = pybind11::module::import("sys");
    sys.attr("path").attr("append")("tests/");
    sys.attr("path").attr("append")("../tests/");
    auto helper = pybind11::module::import("real_data_test_helper");
    std::string const name{json[patient]["channel"]};
    channel = helper.attr("get_Scv")(std::string{chbmit}, patient, name, samplerate, clean_signal)
                  .template cast<pybind11::array_t<Real>>();
    for (auto element : channel) { result.push_back(element.template cast<Real>()); }
  }
  return result;
}
