#define VALIDATION_METADATA_CSV_SUPPORT
#define PYBIND11_DETAILED_ERROR_MESSAGES

#include <iostream>
#include <fstream>
#include <string>
#include <filesystem>
#include <vector>
#include <span>
#include <tuple>

#include <nlohmann/json.hpp>
#include <rapidcsv.h>
#include <pybind11/embed.h>
#include <pybind11/numpy.h>

#include "validation/validation.hpp"
#include "validation/metadata.hpp"
#include "validation/batch.hpp"
#include "validation/validator.hpp"

using json = nlohmann::json;
using Real = float;

constexpr int samplerate{256};
constexpr bool filtered{true};
constexpr auto sEpoPat = Validation::epoch;
constexpr auto stride = Validation::stride;


// NOLINTBEGIN
int main (int argc, char * argv[]) {
  if (argc != 5) {
    std::cerr << "USAGE: `" << argv[0] << " patient csv-file json-file chbmit'"
              << "\n";
    return -1;
  }
  std::string const patient{argv[1]};
  std::string const csv_path{argv[2]};
  std::string const json_path{argv[3]};
  std::filesystem::path const chbmit{argv[4]};
  // Open CSV and JSON
  rapidcsv::Document const csv_data{csv_path, rapidcsv::LabelParams(0, 0)};
  std::ifstream f(json_path);
  if (not f) {
    std::cerr << "Couldn't open json file `" << json_path << "'" << "\n";
    return -2;
  }
  json json_data = json::parse(f);
  // Use the python to generate the patient data
  pybind11::scoped_interpreter python{};
  pybind11::array_t<Real> foreign_channel;
  {
    std::cout << "Fetching patient data..." << "\n";
    auto sys = pybind11::module::import("sys");
    sys.attr("path").attr("append")("tests/");
    auto helper = pybind11::module::import("real_data_test_helper");
    std::string const channel{json_data[patient]["channel"]};
    foreign_channel = helper.attr("get_Scv")(std::string{chbmit}, patient, channel, samplerate, filtered).cast<pybind11::array_t<Real>>();
  }
  std::span<const Real> channel{foreign_channel.data(),
                                static_cast<std::size_t>(foreign_channel.size())};

  std::cout << "   Channel Size: " << channel.size() << "\n";
  size_t const n_epochs = (channel.size() - sEpoPat) / stride - 1;
  std::cout << "   Epoch Count: " << n_epochs << "\n";

  // Obtain the patterns
  std::span<const Real> Pj;
  {
    int const Pj_index = csv_data.GetCell<int>("batch", patient);
    Pj = channel.subspan(Pj_index * stride, Validation::query_size);
    // std::vector<Real> Pj;
    // for (auto item : json_data[patient]["patterns"][0]) {
    //   Pj.push_back(item);
    // }
  }

  // Read the metadata
  Validation::Metadata<Real> const
    metadata{chbmit/patient/(patient+std::string("_seizure_table.csv"))};

  // Validate each region
  std::vector<std::pair<std::size_t, std::size_t>> pairs;
  {
    auto helper = pybind11::module::import("real_data_test_helper");
    auto img = csv_data.GetCell<std::string>("validation", patient);
    auto list = helper.attr("my_eval")(img);
    int length = list.attr("__len__")().cast<int>();
    pairs.reserve(length);
    for (int i = 0; i < length; ++i) {
      int first = list.attr("__getitem__")(i).attr("__getitem__")(0).cast<int>();
      int second = list.attr("__getitem__")(i).attr("__getitem__")(1).cast<int>();
      pairs.emplace_back(first, second);
    }
  }
  // Validation::Validator validator{Scv, DNCs, metadata, batch};
  Validation::patient_information_t const info{patient, csv_path, json_path};
  Validation::Validator validator{channel, Validation::make_DNCs(channel, false), metadata, info};

  for (auto [from, to] : validator.ranges(info)) {
    std::cout << from << " " << to << std::endl;
    // auto Scv = channel.subspan(from, to - from);
    // size_t const n_epochs = (Scv.size() - sEpoPat) / stride - 1;
    // std::vector<bool> const DNCs(n_epochs, false);
    // std::cout << "   Epochs: " << n_epochs << "\n";


    // Results
    // auto result = Validation::validation<Real>(Scv, DNCs, metadata, batch, from / stride);
    // auto result = Validation::validation<Real>(Scv, DNCs, metadata, batch, 0);
    // std::cout << "      Results:"         << "\n"
    //           << "         Precision:   " << result.precision << "\n"
    //           << "         Sensitivity: " << result.sensitivity << "\n"
    //           << "         F1 Score:    " << result.f_1_score << "\n";
  }
  return 0;
}
//NOLINTEND
