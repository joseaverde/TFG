#pragma once

#include "random_walk.hpp"
#include "validation/validation.hpp"

#include <benchmark/benchmark.h>
#include <concepts>
#include <gsl/gsl>
#include <iostream>
#include <span>
#include <vector>

namespace BenchmarkValidation_ {

  // constexpr size_t warp_win = 16;     //warping window size of the Sakoe-Chiba band
  constexpr ptrdiff_t sEpoPat = 1024;  // size of Epochs
  constexpr ptrdiff_t stride  = 256;   // samples between consecutive patterns and
                                       // epochs (sliding window stride)
  template <std::floating_point Real>
  constexpr Validation::bounds_t<Real> all{std::numeric_limits<Real>::min(),
                                           std::numeric_limits<Real>::max()};

  template <class T>
  using my_span = std::span<T, std::dynamic_extent>;

  template <std::floating_point Real>
  void benchmark_prevalidation(benchmark::State & state) {
    gsl::index const count          = sEpoPat + stride * (state.range(0) - 1);
    gsl::index const signal_samples = state.range(1);
    gsl::index const perc           = state.range(2);
    gsl::index const query_samples  = signal_samples * perc / 100;
    gsl::index const n_epochs       = (signal_samples - sEpoPat) / stride + 1;
    auto const signal{random_walk<Real>(signal_samples)};
    auto const query{random_walk<Real>(query_samples)};
    std::vector<bool> const DNCs(n_epochs, true);
    my_span<const Real> P_j{signal.cbegin(), static_cast<size_t>(count)};
    Validation::Batch<Real, my_span<const Real>> const B_c{
        all<Real>, all<Real>, all<Real>, all<Real>, all<Real>, std::numeric_limits<Real>::max(),
        P_j};
    Validation::Metadata<Real> const meta{};
    for (auto foo : state) {
      auto result = Validation::validation<Real>(signal, DNCs, meta, B_c);
      benchmark::DoNotOptimize(result);
    }
    state.counters["Epochs"] = benchmark::Counter(gsl::narrow<double>(n_epochs),
                                                  benchmark::Counter::kIsIterationInvariantRate);
  }

  template <std::floating_point Real>
  void benchmark_validation(benchmark::State & state) {
    gsl::index const count          = sEpoPat + stride * (state.range(0) - 1);
    gsl::index const signal_samples = state.range(1);
    gsl::index const perc           = state.range(2);
    gsl::index const query_samples  = signal_samples * perc / 100;
    gsl::index const n_epochs       = (signal_samples - sEpoPat) / stride + 1;
    auto const signal{random_walk<Real>(signal_samples)};
    auto const query{random_walk<Real>(query_samples)};
    std::vector<bool> const DNCs(n_epochs, false);
    my_span<const Real> P_j{signal.cbegin(), static_cast<size_t>(count)};
    Validation::Batch<Real, my_span<Real const>> const B_c{
        all<Real>, all<Real>, all<Real>, all<Real>, all<Real>, std::numeric_limits<Real>::max(),
        P_j};
    Validation::Metadata<Real> const meta{};
    for (auto foo : state) {
      auto result = Validation::validation<Real>(signal, DNCs, meta, B_c);
      benchmark::DoNotOptimize(result);
    }
    state.counters["Epochs"] = benchmark::Counter(gsl::narrow<double>(n_epochs),
                                                  benchmark::Counter::kIsIterationInvariantRate);
  }
}  // namespace BenchmarkValidation_
