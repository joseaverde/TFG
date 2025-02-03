#include <filesystem>
#include <gsl/gsl>
#include <iostream>
#include <seizure>
#include <string>
#include "utils.hpp"

using Real                    = double;
constexpr int main_argc_count = 6;
constexpr int fifth           = 5;

template <typename Func, typename... Args>
[[nodiscard]] auto benchmark(Func func, Args... args) {
  auto start  = std::chrono::high_resolution_clock::now();
  auto result = func(args...);
  auto stop   = std::chrono::high_resolution_clock::now();
  std::cout << "   Elapsed "
            << std::chrono::duration_cast<std::chrono::milliseconds>(stop - start).count() << "ms\n";
  return result;
}

void best_case(auto const & Scv, auto const & metadata, auto const & patient_information) {
  Seizure::Validator const validator(Scv, Seizure::make_DNCs(Scv, false), metadata,
                                     patient_information);
  for (auto && [from, to] : validator.ranges(patient_information)) {
    int from_copy = from;
    int to_copy = to;
    std::cout << "   Range " << from << " .. " << to << "\n";
    auto const result = benchmark([&]() { return validator.validate(from_copy, to_copy); });
    Seizure::Quality_metrics<Real> const metrics{result};
    std::cout << result << "\n" << metrics << "\n";
  }
  std::cout << "   Full signal\n";
  auto const result = benchmark([&]() { return validator.validate(); });
  Seizure::Quality_metrics<Real> const metrics{result};
  std::cout << result << "\n" << metrics << "\n";
}

void worst_case (auto const & Scv, auto const & metadata) {
  Seizure::Validator const validator(Scv, Seizure::make_DNCs(Scv, false), metadata);
  auto const result = benchmark([&]() { return validator.validate(); });
  Seizure::Quality_metrics<Real> const metrics{result};
  std::cout << result << "\n" << metrics << "\n";
}

// NOLINTBEGIN(bugprone-exception-escape)
int main(int argc, char const * argv[]) {
  using namespace std::literals::string_literals;
  gsl::span<char const *> const args{argv, static_cast<std::size_t>(argc)};
  if (argc != main_argc_count) {
    std::cerr << "USAGE: `" << args[0] << " patient csv-file json-file chbmit-path worst-case?'\n";
    return -1;
  }
  std::cout << "Loading signal...\n";
  std::string const patient{args[1]};
  std::string const csv_path{args[2]};
  std::string const json_path{args[3]};
  std::filesystem::path const chbmit_path{args[4]};
  auto const Scv{read_scv<Real>(true, chbmit_path, patient, json_path)};
  Seizure::Patient_information const patient_information{patient, csv_path, json_path};
  Seizure::Metadata const metadata{chbmit_path / patient /
                                   (patient + std::string("_seizure_table.csv"))};
  if (args[fifth] == "true"s) {
    worst_case(Scv, metadata);
  } else if (args[fifth] == "false"s) {
    best_case(Scv, metadata, patient_information);
  } else {
      std::cerr << "Invalid worst case\n";
      return -1;
  }
  return 0;
}
// NOLINTEND(bugprone-exception-escape)
