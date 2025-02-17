// sudo perf stat -r 5 -e power/energy-cores/,power/energy-ram. ./executable
#include <iostream>
#include <seizure>
#include <chrono>
#include <vector>
#include "random_walk.hpp"

constexpr Seizure::Sample_count stride_size = 256;
constexpr Seizure::Stride_count epoch_size = 5;
constexpr Seizure::Stride_count input_signal_size = 8192;
constexpr Seizure::Stride_count detector_stride_count = 5;
using Real = double;

int main () {
  auto start = std::chrono::high_resolution_clock::now();
  auto const Scv {random_walk<Real>(input_signal_size * stride_size)};
  constexpr Seizure::Bounds<Real> all_reals{-std::numeric_limits<Real>::infinity(),
                                            std::numeric_limits<Real>::infinity()};
  Seizure::Batch const batch{all_reals, all_reals, all_reals, all_reals, all_reals,
    std::numeric_limits<Real>::infinity(), random_walk<Real>(epoch_size * stride_size)};
  Seizure::Detector detector{stride_size, epoch_size, detector_stride_count, batch};
  auto stop = std::chrono::high_resolution_clock::now();
  auto time = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start).count();
  std::cout << "Preparation took " << time << "ms\n";
  std::cout << "Starting detection\n";
  int detected = 0;
  start = std::chrono::high_resolution_clock::now();
  for (Seizure::Sample_count index = 0; index <= std::ssize(Scv) - stride_size; index += stride_size) {
    detector.write([&Scv,index] (Seizure::Sample_index sample) {
          return Scv[index + sample];
        });
    if (detector.detect()) { detected++; }
  }
  stop = std::chrono::high_resolution_clock::now();
  time = std::chrono::duration_cast<std::chrono::microseconds>(stop - start).count();
  std::cout << "Detected " << detected << " out of " << input_signal_size << " strides!\n";
  std::cout << "Elapsed " << time << "μs (" << time/input_signal_size << " μs/stride)\n";
  return 0;
}
