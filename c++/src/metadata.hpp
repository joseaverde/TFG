#ifndef SEIZURE_METADATA_HPP
#define SEIZURE_METADATA_HPP

#include <concepts>
#include <vector>
#include <tuple>
#include <algorithm>

#ifdef SEIZURE_CSV_SUPPORT
  #include <rapidcsv.h>
#endif

namespace Seizure {

  template <std::integral Count_type>
  struct Counters {
      Count_type true_positives{0};
      Count_type false_positives{0};
      Count_type true_negatives{0};
      Count_type false_negatives{0};
  };

  class Metadata {
      std::vector<bool> regions_;
      std::vector<Epoch_index> start_;
      std::vector<Epoch_index> end_;
      Stride_count fns_{};
      Stride_count tns_{};
    public:
#ifdef SEIZURE_CSV_SUPPORT
      Metadata (auto const & path) {
        rapidcsv::Document const document{path};
        start_ = document.GetColumn<Epoch_index>("total_start");
        end_ = document.GetColumn<Epoch_index>("total_end");
        start_.pop_back();
        end_.pop_back();
        regions_ = std::vector<bool>(start_.size(), false);
      }
#endif

      Metadata() = default;

      Metadata(Input_channel_of<std::pair<Epoch_index, Epoch_index>> auto const & pairs)
        : regions_(pairs.size(), false) {
        start_.reserve(pairs.size());
        end_.reserve(pairs.size());
        for (auto const & [start, end] : pairs) {
          start_.push_back(start);
          end_.push_back(end);
        }
      }

      [[nodiscard]] bool is_seizure (Epoch_index epoch_i) const {
        return find_region(epoch_i) != not_a_seizure;
      }

      void reset () {
        std::fill(regions_.begin(), regions_.end(), false);
        fns_ = 0;
        tns_ = 0;
      }

      void check_seizure (Epoch_index index) {
        auto const region = find_region(index);
        if (region != not_a_seizure) { regions_[region] = true; }
      }

      void check_not_seizure (Epoch_index index) {
        auto const region = find_region(index);
        if (region == not_a_seizure) {
          tns_++;
        } else {
          fns_++;
        }
      }

      template <std::integral Count>
      [[nodiscard]] auto get_counters () const {
        Counters<Count> counters{};
        for (auto item : regions_) {
          if (item) {
            counters.true_positives++;
          } else {
            counters.false_positives++;
          }
        }
        counters.true_negatives = tns_;
        counters.false_negatives = fns_;
        return counters;
      }

    private:
      static constexpr Epoch_index not_a_seizure{-1};
      [[nodiscard]] Epoch_index find_region (Epoch_index epoch_i) const {
        for (Epoch_index idx = 0; idx < std::ssize(start_); ++idx) {
          if (epoch_i >= start_[idx] and epoch_i <= end_[idx]) {
            return idx;
          }
        }
        return not_a_seizure;
      }
  };

}

#endif//SEIZURE_METADATA_HPP
