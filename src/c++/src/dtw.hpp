// @file dtw.hpp
// @brief Dynamic Time Warping algorithm implmentation
// @author José Antonio Verde Jiménez

#ifndef SEIZURE_DTW_HPP
#define SEIZURE_DTW_HPP

#include "concepts.hpp"
#include <vector>
#include <gsl/gsl>
#include <span>
#include <cmath>
#include <concepts>
#include <numeric>
#include <range/v3/all.hpp>

// TODO: Make functions take less parameters
// NOLINTBEGIN(readability-function-size)
namespace Seizure {

  /// @value Partial_euclidean_distance
  /// z-normalized DTW matrix computation with partial euclidean distance (no sqrt)
  ///
  /// @value Full_euclidean_distance_symmetric_2
  /// Un-z-normalized DTW version with full euclidean distance (with sqrt) and pattern=symmetric_2
  ///
  /// @value Partial_euclidean_distance_symmetric_2
  /// Un_z_normalized DTW version with partial euclidean distance (without sqrt) and
  /// pattern=symmetric_2
  enum class DTW_method : std::uint8_t {
    Partial_euclidean_distance,
    Full_euclidean_distance_symmetric_2,
    Partial_euclidean_distance_symmetric_2
  };

  enum class Pattern_kind : std::uint8_t { Symmetric_1 = 1, Symmetric_2 = 2 };

  struct Block_range {
      std::ptrdiff_t eID;   // Index of the first epoch to process
      std::ptrdiff_t nEpo;  // Number of epochs to process
      std::ptrdiff_t pID;   // Index of the first pattern to process
      std::ptrdiff_t nPat;  // Number of patterns to process
  };

  struct DTW_params {
      std::ptrdiff_t sEpoPat;
      std::ptrdiff_t stride;
      std::ptrdiff_t warping_window;
  };

  template <Real_type Real>
  struct Stat_params {
      Real mean, invdev;
  };

  /// Returns two versions of the statistics: one with μ (mu) and σ (sigma) interleaved and another
  /// with μ and σ separated for SIMD version.
  auto computeStatistics(Real_input_channel auto const & signal, DTW_params params) {
    using Real                     = Channel_value_type<decltype(signal)>;
    std::ptrdiff_t const numEpochs = (std::ssize(signal) - params.sEpoPat) / params.stride + 1;
    std::vector<Stat_params<Real>> statistics;
    statistics.reserve(numEpochs);
    for (gsl::index i = 0; i < numEpochs; ++i) {
      auto const win  = signal | ranges::views::slice(params.stride * i, params.stride * i + params.sEpoPat);
      Real const avg  = std::accumulate(win.begin(), win.end(), Real{}) / params.sEpoPat;
      Real const sum2 = std::accumulate(win.begin(), win.end(), Real{}, [](Real l, Real r) {
        return l + std::pow(r, 2);
      });
      // We store the inverse of σ = 1/m * Σ(x_i - μ)² = 1/m * Σ(x_i²) - μ²
      Real const invdev = Real{1} / std::sqrt((sum2 / params.sEpoPat) - std::pow(avg, 2));
      statistics.emplace_back(avg, invdev);
    }
    return statistics;
  }

  template <bool doesSqrt>
  constexpr auto dist(Real_type auto x, Real_type auto y) {
    if constexpr (doesSqrt) {
      // return std::sqrt(dist<false>(x, y));
      // That is the same as saying:
      return std::abs(x - y);
    } else {
      return std::pow(x - y, 2);
    }
  }

  /// This function computes the DTW similarly to the dtw-python library with symetric 2 pattern but
  /// partial euclidean distance without sqrt.
  /// Calculate Dynamic Time Warpping distance.
  ///
  /// @param A
  /// Z-Normalized Data.
  ///
  /// @param B
  /// Z-Normalized Query
  ///
  /// @param rows
  /// Size of Sakoe-Chiba warpping band
  ///
  /// @param workspace
  /// Prealocated space of size 4*r+2 to accomodate 2 warpping bands.
  ///
  /// @tparam diagCost
  /// Symmetric_1 or Symmetric_2 pattern
  ///
  /// @tparam doesSqrt
  /// if True then use full euclidean distance, otherwise use partial euclidean distance (without
  /// sqrt)
  // TODO: Averiguar qué es `m' de dónde sale y cuál es su dominio.
  /// Each band occupies 2 * r + 1 elements
  template <Pattern_kind diagCost, bool doesSqrt>
  constexpr auto single_dtw(Real_input_channel auto const & A, Real_input_channel auto const & B,
                            ssize_t m, ssize_t rows,
                            Common_channel_value_type<decltype(A), decltype(B)> max) {
    using Real         = decltype(max);
    ssize_t k_idx      = 0;
    constexpr Real inf = std::numeric_limits<Real>::infinity();
    std::vector<Real> cost(2 * rows + 1, inf);
    std::vector<Real> prev(2 * rows + 1, inf);
    for (ssize_t row = 0; row < m; ++row) {
      k_idx = std::max(ssize_t{0}, rows - row);
      ssize_t const last_j{std::min(m - 1, row + rows)};
      ssize_t const first_j{std::max(ssize_t{0}, row - rows)};
      for (ssize_t j = first_j; j <= last_j; ++j, ++k_idx) {
        Real const current_dist = dist<doesSqrt>(A[row], B[j]);
        if (row == 0 and j == 0) {
          cost[k_idx] = current_dist;
          continue;
        }
        Real const y_val = ((j < 1) or (k_idx < 1)) ? inf : cost[k_idx - 1] + current_dist;
        Real const x_val = ((row < 1) or (k_idx + 1 > 2 * rows)) ? inf : prev[k_idx + 1] + current_dist;
        Real const z_val = ((row < 1) or (j < 1)) ? inf : prev[k_idx] + static_cast<int>(diagCost) * current_dist;
        cost[k_idx] = std::min({x_val, y_val, z_val});
        // if (cost[k_idx] > max) { return inf; }
      }
      std::swap(cost, prev);
    }
    return prev[--k_idx];
  }

  auto DTWCPU(Real_input_channel auto const & S, Real_input_channel auto const & Q,
              Block_range const & blk, DTW_params params,
              Common_channel_value_type<decltype(S), decltype(Q)> max) {
    using Real = decltype(max);
    auto sttS{computeStatistics(S, params)};
    auto sttQ{computeStatistics(Q, params)};
    std::vector<Real> regPat(blk.nPat * params.sEpoPat);

    for (gsl::index patID = blk.pID; patID < (blk.pID + blk.nPat); ++patID) {
      Real meanPat     = sttQ[patID].mean;
      Real invSigmaPat = sttQ[patID].invdev;
      for (gsl::index j = 0; j < params.sEpoPat; ++j) {
        regPat[(patID - blk.pID) * params.sEpoPat + j] =
            (Q[patID * params.stride + j] - meanPat) * invSigmaPat;
      }
    }

    std::vector<Real> result(blk.nPat * blk.nEpo);
    for (gsl::index epID = blk.eID; epID < (blk.eID + blk.nEpo); ++epID) {
      std::vector<Real> Epoch(params.sEpoPat, 0);
      Real const meanEp        = sttS[epID].mean;
      Real const invSigmaEp    = sttS[epID].invdev;
      std::ptrdiff_t const ptr = epID * params.stride;
      for (gsl::index k = 0; k < params.sEpoPat; ++k) {
        Epoch[k] = (S[ptr + k] - meanEp) * invSigmaEp;
      }
      for (gsl::index patID = 0; patID < blk.nPat; ++patID) {
        std::span<Real const> const regPat_view{&regPat[patID * params.sEpoPat],
                                                static_cast<std::size_t>(params.sEpoPat)};
        result[(epID - blk.eID) * blk.nPat + patID] = single_dtw<Pattern_kind::Symmetric_1, false>(
            Epoch, regPat_view, params.sEpoPat, params.warping_window, max);
      }
    }

    return result;
  }

  template <bool doesSqrt>
  auto DTWCPU_u(Real_input_channel auto const & S, Real_input_channel auto const & Q,
                Block_range const & blk, DTW_params params,
                Common_channel_value_type<decltype(S), decltype(Q)> max) {
    std::vector<decltype(max)> result(blk.nPat * blk.nEpo);
    for (gsl::index epID = blk.eID; epID < (blk.eID + blk.nEpo); ++epID) {
      std::ptrdiff_t const ptr = epID * params.stride;
      for (gsl::index patID = 0; patID < blk.nPat; ++patID) {
        auto const S_view = S | ranges::views::drop(ptr) | ranges::views::take(params.sEpoPat);
        auto const Q_view =
            Q | ranges::views::drop(patID * params.stride) | ranges::views::take(params.sEpoPat);
        result[(epID - blk.eID) * blk.nPat + patID] = single_dtw<Pattern_kind::Symmetric_2, doesSqrt>(
            S_view, Q_view, params.sEpoPat, params.warping_window, max);
      }
    }
    return result;
  }

  template <DTW_method method>
  auto get_distance_matrix(
      Real_input_channel auto const & S, Real_input_channel auto const & Q, DTW_params params,
      Common_channel_value_type<decltype(S), decltype(Q)> max =
          std::numeric_limits<Common_channel_value_type<decltype(S), decltype(Q)>>::infinity()) {
    std::ptrdiff_t const nEpo = (std::ssize(S) - params.sEpoPat) / params.stride + 1;
    std::ptrdiff_t const nPat = (std::ssize(Q) - params.sEpoPat) / params.stride + 1;
    Block_range const blk{0, nEpo, 0, nPat};
    if constexpr (method == DTW_method::Partial_euclidean_distance) {
      return DTWCPU(S, Q, blk, params, max);
    } else {
      constexpr bool with_sqrt = method == DTW_method::Full_euclidean_distance_symmetric_2;
      return DTWCPU_u<with_sqrt>(S, Q, blk, params, max);
    }
  }
}  // namespace Seizure
// NOLINTEND(readability-function-size)

#endif  // SEIZURE_DTW_HPP
