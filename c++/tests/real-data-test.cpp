#define VALIDATION_METADATA_CSV_SUPPORT
#define PYBIND11_DETAILED_ERROR_MESSAGES

#include <iostream>
#include <fstream>
#include <string>
#include <filesystem>
#include <vector>
#include <span>
#include <tuple>
#include <chrono>

#include <nlohmann/json.hpp>
#include <rapidcsv.h>
#include <pybind11/embed.h>
#include <pybind11/numpy.h>

#include "validation/validation.hpp"
#include "validation/metadata.hpp"
#include "validation/batch.hpp"

using json = nlohmann::json;
using Real = double;

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
  std::vector<Real> signal_channel;
  std::vector<std::pair<std::size_t, std::size_t>> pairs;
  {
    pybind11::scoped_interpreter python{};
    pybind11::array_t<Real> foreign_channel;
    std::cout << "Fetching patient data..." << "\n";
    auto sys = pybind11::module::import("sys");
    sys.attr("path").attr("append")("tests/");
    auto helper = pybind11::module::import("real_data_test_helper");
    std::string const channel{json_data[patient]["channel"]};
    foreign_channel = helper.attr("get_Scv")(std::string{chbmit}, patient, channel, samplerate, filtered).cast<pybind11::array_t<Real>>();
    signal_channel.reserve(foreign_channel.size());
    for (auto it : foreign_channel) {
      signal_channel.push_back(it.cast<Real>());
    }
    // Validate each region
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
  std::span<const Real> channel{signal_channel.data(), signal_channel.size()};

  std::cout << "   Channel Size: " << channel.size() << "\n";
  size_t const n_epochs = (channel.size() - sEpoPat) / stride - 1;
  std::cout << "   Epoch Count: " << n_epochs << "\n";

  // Obtain the patterns
  std::span<const Real> Pj;
  {
    int const Pj_index = csv_data.GetCell<int>("batch", patient);
    Pj = channel.subspan(Pj_index * stride, 1280);
    // std::vector<Real> Pj;
    // for (auto item : json_data[patient]["patterns"][0]) {
    //   Pj.push_back(item);
    // }
  }

  // Generate the batch data
  [[maybe_unused]] Validation::Batch<Real, std::span<const Real>> batch {
    {csv_data.GetCell<Real>("p1_min", patient), csv_data.GetCell<Real>("p1_max", patient)},
    {csv_data.GetCell<Real>("p2_min", patient), csv_data.GetCell<Real>("p2_max", patient)},
    {csv_data.GetCell<Real>("p3_min", patient), csv_data.GetCell<Real>("p3_max", patient)},
    {csv_data.GetCell<Real>("d_min", patient), csv_data.GetCell<Real>("d_max", patient)},
    {csv_data.GetCell<Real>("e_min", patient), csv_data.GetCell<Real>("e_max", patient)},
    json_data[patient]["dmax"], Pj};
  std::cout << batch << "\n";

  // Read the metadata
  Validation::Metadata<Real> const
    metadata{chbmit/patient/(patient+std::string("_seizure_table.csv"))};

  // for (auto [from, to] : pairs) {
  {
    std::size_t from = 0;
    std::size_t to = channel.size();
    std::cout << "   Validating " << from << " .. " << to << "\n";
    // from *= stride;
    // to *= stride;
    std::cout << "   [" << from << ", " << to << "]" << "\n";
    auto Scv = channel.subspan(from, to - from);
    size_t const n_epochs = (Scv.size() - sEpoPat) / stride - 1;
    std::vector<bool> const DNCs(n_epochs, false);
    std::cout << "   Epochs: " << n_epochs << "\n";
    // Results
    // auto result = Validation::validation<Real>(Scv, DNCs, metadata, batch, from / stride);
    [[maybe_unused]] constexpr Real inf{std::numeric_limits<Real>::infinity()};
    Validation::Batch<Real, std::span<const Real>> const batch {
      {-inf, inf},  {-inf, inf},  {-inf, inf},  {-inf, inf},  {-inf, inf},  json_data[patient]["dmax"], Pj
    };
    std::cout << batch << "\n";
    auto t1 = std::chrono::high_resolution_clock::now();
    auto result = Validation::validation<Real>(Scv, DNCs, metadata, batch, 0);
    auto t2 = std::chrono::high_resolution_clock::now();
    std::cout << "      Results:"         << "\n"
              << "         Precision:   " << result.precision << "\n"
              << "         Sensitivity: " << result.sensitivity << "\n"
              << "         F1 Score:    " << result.f_1_score << "\n"
              << "         Elapsed:     "
                << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
                << "ms\n";
  }
  return 0;
}
//NOLINTEND
