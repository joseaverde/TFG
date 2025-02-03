#pragma once

#include "random_walk.hpp"

#include <benchmark/benchmark.h>
#include <concepts>
#include <signals/signals.hpp>
#include <signals/welch.hpp>

namespace BenchmarkSignals_ {

  constexpr ptrdiff_t mov_win       = 1280;
  constexpr ptrdiff_t stride        = 256;
  constexpr ptrdiff_t element_count = 1'000'000;
  constexpr ptrdiff_t n_epochs      = (element_count - mov_win) / stride + 1;
  constexpr Signals::params_t params{mov_win, stride};
  constexpr ptrdiff_t welch_win     = 512;
  constexpr ptrdiff_t welch_overlap = welch_win / 2;
  template <std::floating_point Real>
  constexpr Real welch_freq = 256.0;
  template <std::floating_point Real>
  constexpr Real simpson_dx = 0.5;

  inline void set_epochs(benchmark::State & state) {
    state.counters["Epochs"] = benchmark::Counter(gsl::narrow<double>(n_epochs),
                                                  benchmark::Counter::kIsIterationInvariantRate);
  }

  template <std::floating_point Real>
  void Simpson(benchmark::State & state) {
    auto const signal{random_walk<Real>(element_count)};
    for (auto foo : state) {
      auto result = Signals::simpson(signal, simpson_dx<Real>);
      benchmark::DoNotOptimize(result);
    }
    set_epochs(state);
  }

  template <std::floating_point Real>
  void Max_Dist(benchmark::State & state) {
    auto const signal{random_walk<Real>(element_count)};
    for (auto foo : state) {
      auto result = Signals::call_max_dist(signal, params);
      benchmark::DoNotOptimize(result);
    }
    set_epochs(state);
  }

  template <std::floating_point Real>
  void Energy(benchmark::State & state) {
    auto const signal{random_walk<Real>(element_count)};
    for (auto foo : state) {
      auto result = Signals::call_energy(signal, params);
      benchmark::DoNotOptimize(result);
    }
    set_epochs(state);
  }

  template <std::floating_point Real>
  void PSD(benchmark::State & state) {
    auto const signal{random_walk<Real>(element_count)};
    auto const ranges{Signals::default_ranges<Real>()};
    for (auto foo : state) {
      auto result = Signals::call_psd(signal, params, ranges);
      benchmark::DoNotOptimize(result);
    }
    set_epochs(state);
  }

  template <std::floating_point Real>
  void Welch(benchmark::State & state) {
    auto const signal{random_walk<Real>(element_count)};
    for (auto foo : state) {
      Signals::Welch<Real, Signals::Windows::hann<Real>> welch(welch_win);
      auto result = welch(signal, welch_freq<Real>, welch_overlap);
      benchmark::DoNotOptimize(result);
    }
    set_epochs(state);
  }

  template <std::floating_point Real>
  void Welch_Warmup(benchmark::State & state) {
    auto const signal{random_walk<Real>(element_count)};
    Signals::Welch<Real, Signals::Windows::hann<Real>> welch(welch_win);
    for (auto foo : state) {
      auto result = welch(signal, welch_freq<Real>, welch_overlap);
      benchmark::DoNotOptimize(result);
    }
    set_epochs(state);
  }
}  // namespace BenchmarkSignals_
