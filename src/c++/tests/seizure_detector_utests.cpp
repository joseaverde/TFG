#include "common.hpp"
#include <iostream>
#include <string>
#include <vector>

std::vector<Real> read_vector () {
  std::vector<Real> data;
  std::size_t size;
  std::cin >> size;
  data.reserve(size);
  for (std::size_t i = 0; i < size; ++i) {
    Real temp;
    std::cin >> temp;
    data.push_back(temp);
  }
  return data;
}

void put (std::vector<Real> const & signal) {
  for (auto item : signal) { std::cout << " " << item; }
}

constexpr std::size_t welch_window_size = 512;
constexpr std::size_t welch_window_overlap = welch_window_size / 2;

int main () {
  std::cout << "C++ utests\n";
//std::cout << "stride_size          = " << Stride_Size << "\n";
//std::cout << "epoch_size           = " << Epoch_Size << "\n";
  std::cout << "welch_window_size    = " << welch_window_size << "\n";
  std::cout << "welch_window_overlap = " << welch_window_overlap << "\n";
  std::cout << std::endl;

  std::string command;
  while (true) {
    std::getline(std::cin, command);
    if (command == "Stop") { break; }
    if (command == "Simpson") {
      Real dx;
      auto signal = read_vector();
      std::cin >> dx;
      std::getline(std::cin, command);
      std::cout << Seizure::simpson(signal, dx) << std::endl;
    } else if (command == "Welch") {
      auto Pxx = read_vector();
      Real freq;
      std::cin >> freq;
      std::getline(std::cin, command);
      Seizure::Welch<Real, Seizure::Windows::hann> welch(welch_window_size);
      put(welch(Pxx, freq, welch_window_overlap));
      std::cout << std::endl;
    } else {
      std::cerr << "Unknown command " << command << std::endl;
      break;
    }
  }
  return 0;
}
