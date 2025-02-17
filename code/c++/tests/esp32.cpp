#include <filesystem>
#include <seizure>
#include <gsl/gsl>
#include <iostream>
#include <string>
#include <boost/asio.hpp>
#include <chrono>
#include <thread>
#include "utils.hpp"

using Real                                   = float;
constexpr int main_argc_count                = 5;
constexpr Seizure::Sample_count stride_size  = 256;
constexpr Seizure::Stride_count epoch_size   = 4;
constexpr Seizure::Stride_count stride_count = 5;

class ESP32 {
  public:
    ESP32 (std::string const & port, std::size_t baud_rate)
      : io_{}, serial_{io_, port} {
      serial_.set_option(boost::asio::serial_port_base::baud_rate(baud_rate));
    }

    bool write (const char * data, std::size_t size) {
      using namespace std::chrono_literals;
      if (written_ + size > 128) {
        std::this_thread::sleep_for(1100ms);
        written_ = 0;
      }
      boost::asio::write(serial_, boost::asio::buffer(data, size));
      written_ += size;
      return true;
    }

    std::string read_line () {
      std::string result{};
      char buffer;
      for (;;) {
        boost::asio::read(serial_, boost::asio::buffer(&buffer, 1));
        switch (buffer) {
          case '\r': break;
          case '\n': return result;
          default:   result += buffer;
        }
      }
    }

  private:
    boost::asio::io_service  io_;
    boost::asio::serial_port serial_;
    std::size_t written_{0};
};

template <typename ... Args>
void try_write (ESP32 & stream, Args const & ... args) {
  if (not (Seizure::write(stream, args) and ...)) {
    std::cerr << "Couldn't write to ESP32\n";
    std::terminate();
  }
}

void communicate ([[maybe_unused]] auto const & validator) {
  using namespace std::chrono_literals;
  std::cout << "Opening port\n";
  ESP32 esp32{"/dev/ttyUSB0", 115200};
  std::cout << "ESP32: " << esp32.read_line() << "\n";
  std::cout << "Batch = " << validator.get_batch() << "\n";
  try_write(esp32, stride_size, epoch_size, stride_count, validator.get_batch());
  std::string line;
  while ((line = esp32.read_line()) != "") {
    std::cout << "ESP32: " << line << "\n";
  }
  int index = 0;
  for (auto const & frame : validator.get_scv() | Seizure::sliding_window_view(stride_size, stride_size)) {
    std::cout << "Sending second " << index++ << std::endl;
    auto const next = std::chrono::high_resolution_clock::now() + 4s;
    try_write(esp32, true, frame);
    std::this_thread::sleep_for(next - std::chrono::high_resolution_clock::now());
    std::cout << "ESP32: " << esp32.read_line() << std::endl;
  }
  try_write(esp32, false);
  std::cout << esp32.read_line() << std::endl;
}

// NOLINTBEGIN(bugprone-exception-escape)
int main (int argc, char const * argv[]) {
  gsl::span<char const *> const args{argv, static_cast<std::size_t>(argc)};
  if (argc != main_argc_count) {
    std::cerr << "USAGE: `" << args[0] << " patient csv-file json-file chbmit-path'\n";
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
  // Seizure::Validator const validator(Scv, Seizure::make_DNCs(Scv, false), metadata,
  //                                    patient_information);
  Seizure::Validator const validator(Scv, Seizure::make_DNCs(Scv, false), metadata);
  communicate(validator);
  return 0;
}
// NOLINTEND(bugprone-exception-escape)
