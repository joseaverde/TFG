#include "seizure_detector_config.hpp"
#include "seizure"
#include <iostream>
#include <vector>
#include <chrono>

constexpr std::size_t stride_count = 100;
constexpr std::size_t pattern_count = 3;

template <typename Output_it>
void read_stride (Output_it out, std::size_t part) {
  for (std::size_t i = 0; i < stride_size; ++i) {
    *out = static_cast<Real>(part % 256 + part);
    ++out;
  }
}

constexpr Real inf = std::numeric_limits<Real>::infinity();

auto make_batch () {
  Seizure::Batch<Real, std::vector<std::vector<Real>>> batch;
  batch.psd_1    = {-inf, inf};
  batch.psd_2    = {-inf, inf};
  batch.psd_3    = {-inf, inf};
  batch.energy   = {-inf, inf};
  batch.max_dist = {-inf, inf};
  batch.d_max_c  = 0;
  batch.Pj.reserve(pattern_count);
  for (std::size_t i = 0; i < pattern_count; ++i) {
    batch.Pj.push_back({});
    batch.Pj.reserve(epoch_size);
    for (std::size_t j = 0; j < epoch_size; ++j) {
      batch.Pj[i].push_back(static_cast<Real>(i * epoch_size + j));
    }
  }
  return batch;
}

static auto const psd_params = Seizure::psd_default_ranges<Real>(512);
static Seizure::DTW_params const dtw{epoch_size, stride_size, warping_window};
constexpr Real d_th = 1.05;

template <typename Container, typename Batch>
bool is_seizure (Container const & epoch, Batch const & batch) {
  auto const energy = Seizure::call_energy(epoch, epoch_size, stride_size);
  if (not Seizure::within(energy[0], batch.energy)) { return false; }
  auto const max_dist = Seizure::call_max_dist(epoch, epoch_size, stride_size);
  if (not Seizure::within(max_dist[0], batch.max_dist)) { return false; }
  auto const psds = Seizure::call_psd(epoch, epoch_size, stride_size, psd_params);
  if (not Seizure::within(psds[0][0], batch.psd_1)
      or not Seizure::within(psds[1][0], batch.psd_2)
      or not Seizure::within(psds[2][0], batch.psd_3)) { return false; }
  for (std::size_t i = 0; i < batch.Pj.size(); ++i) {
    auto const dist = Seizure::get_distance_matrix<Seizure::DTW_method::Partial_euclidean_distance>(epoch, batch.Pj[i], dtw)[0];
    if (dist <= batch.d_max_c * d_th) { return true; }
  }
  return false;
}

extern "C" void app_main () {
  std::vector<Real> signal(epoch_size, Real{});
  std::size_t detections = 0;
  auto const batch = make_batch();

  // Algorithm
  const auto start{std::chrono::high_resolution_clock::now()};
  for (std::size_t i = 1; i < strides_per_epoch; ++i) {
    read_stride(signal.begin() + (i - 1) * stride_size, i);
  }
  for (std::size_t i = strides_per_epoch; i <= stride_count; ++i) {
    std::cout << "Second " << i << std::endl;
    std::copy(signal.begin(), signal.begin() + (epoch_size - stride_size),
              signal.begin() + stride_size);
    read_stride(signal.begin(), i);
    if (is_seizure(signal, batch)) { detections++; }
  }
  const auto stop{std::chrono::high_resolution_clock::now()};

  // Result
  const std::chrono::duration<double> elapsed{stop - start};
  std::cout << "Elapsed " << elapsed << " s\n";
  const std::size_t processed = stride_count - strides_per_epoch + 1;
  std::cout << static_cast<double>(processed) / elapsed.count()
            << " epochs/second\n";
  std::cout << static_cast<double>(elapsed.count()) / processed
            << " seconds per epoch\n";
  std::cout << detections << "\n";
}
