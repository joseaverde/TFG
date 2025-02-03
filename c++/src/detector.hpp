#ifndef SEIZURE_DETECTOR_HPP
#define SEIZURE_DETECTOR_HPP

#include "batch.hpp"
#include "dtw.hpp"
#include "signals.hpp"
#include "validator.hpp"
#include "views.hpp"

#include <concepts>
#include <gsl/gsl>
#include <range/v3/all.hpp>
#include <type_traits>
#include <vector>

namespace Seizure {

  template <Real_type Real, Input_channel_of<Real> Query_channel>
  class Detector {
    public:
      Detector(Sample_count stride_size, Stride_count epoch_size, Stride_count stride_count,
               Batch<Real, Query_channel> batch)
        : stride_{stride_size}, epoch_{epoch_size * stride_size}, stride_count_{stride_count},
          samples_(stride_size * stride_count, 0.0), batch_{std::move(batch)} { }

      template <typename Func>
        requires(std::invocable<Func, Sample_index> and
                 std::same_as<Real, std::invoke_result_t<Func, Sample_index>>)
      void write(Func func) {
        auto const offset = stride_ * ((stride_index_ + size_) % stride_count_);
        for (Sample_index index = 0; index < stride_; index++) {
          samples_[offset + index] = func(index);
        }
        if (size_ == stride_count_) { stride_index_ = (stride_index_ + 1) % stride_; }
        size_ = std::min(stride_count_, size_ + 1);
      }

      [[nodiscard]] bool detect() const {
        auto views = samples_ | ranges::views::cycle |
                     ranges::views::drop_exactly(stride_ * stride_index_) |
                     ranges::views::take_exactly(stride_ * size_) | sliding_window_view(epoch_, stride_);
        Real const max = batch_.d_max_c * d_th<Real>;
        for (auto && view : views) {
          if (within(call_energy(view, epoch_, stride_)[0], batch_.energy) and
              within(call_max_dist(view, epoch_, stride_)[0], batch_.max_dist)) {
            auto const psds{call_psd(view, epoch_, stride_, psd_default_ranges<Real>())};
            if (within(psds[0][0], batch_.psd_1) and within(psds[1][0], batch_.psd_2) and
                within(psds[2][0], batch_.psd_3)) {
              std::vector<Real> dists = get_distance_matrix<DTW_method::Partial_euclidean_distance>(
                  view, batch_.Pj, DTW_params{epoch_, stride_, warping_window},
                  std::numeric_limits<Real>::max());
              Real const dist = std::min(std::numeric_limits<Real>::max(),
                  *std::min_element(dists.begin(), dists.end()));
              if (dist < max) { return true; }
            }
          }
        }
        return false;
      }

    private:
      // Stride and Epoch sizes
      Sample_count stride_;
      Sample_count epoch_;
      Stride_count size_{0};
      Stride_count stride_count_;
      // Circular buffer
      Stride_index stride_index_{0};
      std::vector<Real> samples_;
      Batch<Real, Query_channel> batch_;
  };

}  // namespace Seizure

#endif  // SEIZURE_DETECTOR_HPP
