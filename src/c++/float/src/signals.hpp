/// @file signals.hpp
/// @brief A set of utility functions for pruning the search
/// @author José Antonio Verde Jiménez

#ifndef SEIZURE_SIGNALS_HPP
#define SEIZURE_SIGNALS_HPP

#include "algorithms.hpp"
#include "concepts.hpp"
#include "parallel.hpp"
#include "types.hpp"
#include "views.hpp"
#include "welch.hpp"

#include <gsl/gsl>
#include <iostream>
#include <range/v3/all.hpp>

namespace Seizure {

  /// Given an array-like object `y` and the spacing `dx`, it calculates the
  /// integral below the function `y(x)`.
  ///
  /// @param y
  /// An array y[i], i = 0, 1, 2, ... y.size() - 1, that represents the values
  /// of the function `y(x)` evaluated in `y(x0 + i*dx)`. Where `x0` is the
  /// value where `y[0]` is evaluated (for the algorithm it isn't necessary to
  /// now where the function starts).
  ///
  /// @param dx
  /// The spacing between any two values evaluated in `y(x)`.
  ///
  /// @return
  /// It returns the an approximation of the area below the function `y(x)`.
  auto simpson(Real_input_channel auto const & y, Real_type auto dx)
  // https://en.wikipedia.org/wiki/Simpson%27s_rule
  // The function is simplified given that the interval spacing is the same
  // and is constant: `dx': in which case the formula is just:
  //
  //   1     N/2-1
  //  --- dx   Σ   (f[2i] + 4f[2i+1] + f[2i+2])
  //   3      i=0
  //
  //  for (auto i = 0; i <= N/2 - 1; i++) {
  //    result += y[2*i] + 4*y[2*i+1] + y[2*i+2];
  //

  // After analyzing the function we can see that even numbers are added
  // twice except 0 and N, and odd numbers are added four times.
  //
  // Also if the number of elements in y happens to be odd, then we have to
  // apply a correction given by the formula:
  //  __                                      __
  //  |    5          4             1          |
  //  |  ---- f[N] + --- f[N-1] - ---- f[N-2]  | dx
  //  |_  12          6            12         _|
  //
  // And iterate until the second to last element
  {
    using Real = Channel_value_type<decltype(y)>;
    constexpr Real denominator{12};
    constexpr Real weight_1{5};
    constexpr Real weight_2{8};
    Real result{};
    for (gsl::index i = 2; i < std::ssize(y); i += 2) { result += y[i - 2] + 4 * y[i - 1] + y[i]; }
    result = result * dx / 3;
    if (std::ssize(y) > 2 and std::ssize(y) % 2 == 0) {
      result += dx *
                (weight_1 * y[std::ssize(y) - 1] + weight_2 * y[std::ssize(y) - 2] -
                 y[std::ssize(y) - 3]) /
                denominator;
    }
    return result;
  }

  template <Real_type Real>
  constexpr ssize_t psd_channel(Real value, Real step, Real /* unused */) {
    return static_cast<ssize_t>(std::round(value / step));
  }

  template <typename Channel, typename Element>
  concept Pair_input_channel =
      Input_channel<Channel> and
      std::same_as<Channel_value_type<Channel>, std::pair<Element, Element>>;

  template <typename Element, Pair_input_channel<Element> Channel>
  struct PSD_params {
      Channel ranges{};
      Sample_count window_size{};
      Sample_count overlap{};
      Element sampling_freq{};
  };

  template <typename E, typename T>
  PSD_params(PSD_params<E, T>) -> PSD_params<E, T>;

  template <typename T>
  concept A_PSD_parameter_type = requires(T t) {
    { PSD_params(t) } -> std::same_as<T>;
  };

  // NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)
  // The values in this funciton are supposed to be magic numbers, because it
  // returns the DEFAULT values. I'm not using a constant because I can't use
  // a template parameter for it.
  template <typename Real>
  constexpr auto psd_default_ranges(Sample_count window_size = 512) {
    using Pair  = std::pair<Real, Real>;
    using Array = std::array<Pair, 3>;
    return PSD_params<Real, Array>{
        Array{Pair{2.5, 12.0}, Pair{12.0, 18.0}, Pair{18.0, 35.0}},
        window_size, window_size / 2,
        256.0
    };
  }
  // NOLINTEND(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers)

  // NOLINTBEGIN(cppcoreguidelines-pro-bounds-constant-array-index)
  // If Real_input_channel can be any type, including an array. Obviously that
  // I'll need to index it. And it is not as though I'm using some magic number
  // to index. I'm using a for-loop. Therefore it is justified.
  auto call_psd(Real_input_channel auto const & signal, Sample_count mov_win, Sample_count stride,
                A_PSD_parameter_type auto const & params) {
    using Real        = Channel_value_type<decltype(signal)>;
    int const threads = parallel_max_threads();
    std::vector<Welch<Real, Windows::hann>> welchs;
    welchs.reserve(threads);
    std::generate_n(std::back_inserter(welchs), threads, [&]() {
      return Welch<Real, Windows::hann>{params.window_size};
    });
    auto window_view = signal | sliding_window_view(mov_win, stride);
    std::vector<std::vector<Real>> psd_vector(std::ssize(params.ranges),
                                              std::vector<Real>(window_view.size(), Real{}));
    Real freq_res{};
    Real last_freq{};
    welchs[0].freq_range(params.sampling_freq, freq_res, last_freq);
    auto indices = ranges::views::iota(0, static_cast<int>(window_view.size()));
    parallel_for_with_index(ranges::views::zip(window_view, indices), [&](int obj, auto && pars) {
      auto && [win, index]  = pars;
      std::vector<Real> Pxx = welchs[obj](win, params.sampling_freq, params.overlap);
      for (gsl::index j = 0; j < std::ssize(params.ranges); ++j) {
        ssize_t const begin  = psd_channel(params.ranges[j].first, freq_res, last_freq);
        ssize_t const end    = psd_channel(params.ranges[j].second, freq_res, last_freq);
        psd_vector[j][index] = simpson(Pxx | ranges::views::slice(begin, end), freq_res);
      }
    });
    return psd_vector;
  }
  // NOLINTEND(cppcoreguidelines-pro-bounds-constant-array-index)

  auto call_energy(Real_input_channel auto const & signal, Sample_count mov_win,
                   Sample_count stride) {
    using Real = Channel_value_type<decltype(signal)>;
    Expects(mov_win > 0 and stride > 0);
    auto window_view = signal | sliding_window_view(mov_win, stride);
    std::vector<Real> energy_vector(window_view.size());
    parallel_for(ranges::views::zip(window_view, energy_vector), [&](auto && win, auto && energy) {
      auto const mean = accumulate(win, Real{}) / mov_win;
      energy          = accumulate(win, Real{},
                                   [mean](Real l, Real r) {
                            return l + std::pow(r - mean, 2);
                          }) /
               mov_win;
    });
    return energy_vector;
  }

  auto call_max_dist(Real_input_channel auto const & signal, Sample_count mov_win,
                     Sample_count stride) {
    using Real = Channel_value_type<decltype(signal)>;
    Expects(mov_win > 0 and stride > 0);
    auto window_view = signal | sliding_window_view(mov_win, stride);
    std::vector<Real> dist_vector(window_view.size());
    parallel_for(ranges::views::zip(window_view, dist_vector), [&](auto && win, auto && dist) {
      auto const [min, max] = std::minmax_element(win.begin(), win.end());
      dist                  = *max - *min;
    });
    return dist_vector;
  }
}  // namespace Seizure

#endif  // SEIZURE_SIGNALS_HPP
