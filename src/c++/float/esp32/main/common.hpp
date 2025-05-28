#pragma once
#include "seizure_detector_config.hpp"
#include <iostream>
#include <vector>
#include <seizure>

auto read_batch () {
  Seizure::Batch<Real, std::vector<std::vector<Real>>> batch;
  std::size_t size;
  std::cin >> size
           >> batch.psd_1.first    >> batch.psd_1.second
           >> batch.psd_2.first    >> batch.psd_2.second
           >> batch.psd_3.first    >> batch.psd_3.second
           >> batch.energy.first   >> batch.energy.second
           >> batch.max_dist.first >> batch.max_dist.second
           >> batch.d_max_c;
  batch.Pj.reserve(size);
  for (std::size_t i = 0; i < size; ++i) {
    batch.Pj.push_back(std::vector<Real>{});
    batch.Pj[i].reserve(epoch_size);
    for (std::size_t j = 0; j < epoch_size; ++j) {
      Real temp;
      std::cin >> temp;
      batch.Pj[i].push_back(temp);
    }
  }
  return batch;
}

auto read_signal () {
  std::vector<Real> signal;
  std::size_t size;
  std::cin >> size;
  signal.reserve(size);
  for (std::size_t i = 0; i < size; ++i) {
    Real temp;
    std::cin >> temp;
    signal.push_back(temp);
  }
  return signal;
}
