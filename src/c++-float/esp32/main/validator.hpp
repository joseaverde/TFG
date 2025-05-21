#ifndef SEIZURE_VALIDATOR_HPP
#define SEIZURE_VALIDATOR_HPP

#include "batch.hpp"
#include "concepts.hpp"
#include "dtw.hpp"
#include "parallel.hpp"
#include "signals.hpp"
#include "types.hpp"

#include <concepts>
#include <fstream>
#include <gsl/gsl>
#include <limits>
#include <ostream>
#include <range/v3/all.hpp>
#include <ranges>
#include <span>
#include <tuple>
#include <utility>
#include <vector>

#ifdef SEIZURE_JSON_SUPPORT
  #include <nlohmann/json.hpp>
#endif
#ifdef SEIZURE_CSV_SUPPORT
  #include <rapidcsv.h>
#endif

template <typename T>
concept PathName = true;

namespace Seizure {

  template <typename StringType, typename PathType>
  struct Patient_information {
      StringType name;
      PathType csv_path;
      PathType json_path;
  };

  // TODO: Somehow make them class members that can be changed
  constexpr Sample_count query_size     = 1280;
  constexpr Sample_count epoch          = 1280;
  constexpr Sample_count stride         = 256;
  constexpr Sample_count warping_window = 16;

  auto make_DNCs(Input_channel auto const & channel, bool flag) {
    auto const size = (channel.size() - epoch) / stride - 1;
    std::vector<bool> DNCs(size, flag);
    return DNCs;
  }

  template <Real_type Real>
  struct Quality_metrics {
      Real precision;
      Real sensitivity;
      Real f_1_score;

      template <std::integral Count>
      Quality_metrics(Counters<Count> const & counters)
        : precision{gsl::narrow<Real>(counters.true_positives + counters.false_positives) == 0.0
                        ? 1
                        : gsl::narrow<Real>(counters.true_positives) /
                              gsl::narrow<Real>(counters.true_positives +
                                                counters.false_positives)},
          sensitivity{
              gsl::narrow<Real>(counters.true_positives + counters.false_negatives) == 0.0
                  ? 1
                  : gsl::narrow<Real>(counters.true_positives) /
                        gsl::narrow<Real>(counters.true_positives + counters.false_negatives)},
          f_1_score{(precision * sensitivity) * 2 / (precision + sensitivity)} { }
  };

  template <std::integral T>
  Counters<T> operator+(Counters<T> const & lhs, Counters<T> const & rhs) {
    return {lhs.true_positives + rhs.true_positives, lhs.false_positives + rhs.false_positives,
            lhs.true_negatives + rhs.true_negatives, lhs.false_negatives + rhs.false_negatives};
  }

  template <std::integral Count_type>
  std::ostream & operator<<(std::ostream & out, Counters<Count_type> const & counters) {
    return out << "True Positives:  " << counters.true_positives << "\n"
               << "False Positives: " << counters.false_positives << "\n"
               << "True Negatives:  " << counters.true_negatives << "\n"
               << "False Negatives: " << counters.false_negatives << "\n";
  }

  template <Real_type Real>
  std::ostream & operator<<(std::ostream & out, Quality_metrics<Real> const & metrics) {
    return out << "Precision:   " << metrics.precision << "\n"
               << "Sensitivity: " << metrics.sensitivity << "\n"
               << "F1 Score:    " << metrics.f_1_score << "\n";
  }

  template <Real_type Real>
  constexpr Real d_th = 1.05;  // Tolerance factor

  template <Real_type Real, Input_channel_of<Real> Data_channel,
            Input_channel_of<Real> Query_channel, Input_channel_of<bool> Bool_channel>
  class Validator {
    private:
      // TODO: USE ranges::view::cycle. Add funtion to add signal.
      //
      // Validator validator{1280, 256, batch};
      //
      // Make use of a cyclic vector
      // validator.add_stride({1, 2, ..., 256});
      // validator.back_pusher...
      // Add preconditions
      //
      // https://stackoverflow.com/questions/2616643/is-there-a-standard-cyclic-iterator-in-c

      [[nodiscard]] auto filter_S(auto const & view) const {
        auto const psds{call_psd(view, epoch, stride, psd_default_ranges<Real>())};
        auto const energy{call_energy(view, epoch, stride)};
        auto const max_dist{call_max_dist(view, epoch, stride)};
        std::vector<bool> result;
        result.reserve(energy.size());
        for (gsl::index ops = 0; ops < std::ssize(energy); ++ops) {
          result.push_back(
              within(psds[0][ops], batch_.psd_1) and within(psds[1][ops], batch_.psd_2) and
              within(psds[2][ops], batch_.psd_3) and within(energy[ops], batch_.energy) and
              within(max_dist[ops], batch_.max_dist));
        }
        return result;
      }

      [[nodiscard]] auto distance(auto const & signal, Real /* max */) const {
        // FIXME: max doesn't work, only infinity
        std::vector<Real> dists = get_distance_matrix<DTW_method::Partial_euclidean_distance>(
            signal, batch_.Pj, DTW_params{epoch, stride, warping_window},
            std::numeric_limits<Real>::infinity());
        return *std::min_element(dists.begin(), dists.end());
      }

      [[nodiscard]] Real min_distance(auto const & signal, Real max) const {
        Real min = std::numeric_limits<Real>::infinity();
        return std::min(min, distance(signal, max));
      }

    public:
      Validator(Data_channel Scv, Bool_channel DNCs, Metadata md, Batch<Real, Query_channel> batch)
        : Scv_{std::move(Scv)}, DNCs_{std::move(DNCs)}, md_{std::move(md)},
          batch_{std::move(batch)} { }

      [[nodiscard]] auto const & get_batch () const { return batch_; }
      [[nodiscard]] auto const & get_metadata () const { return md_; }
      [[nodiscard]] auto const & get_scv () const { return Scv_; }

#if defined(SEIZURE_CSV_SUPPORT) and defined(SEIZURE_JSON_SUPPORT)
      template <typename StringType, typename PathType>
      Validator(Data_channel Scv, Bool_channel DNCs, Metadata md,
                Patient_information<StringType, PathType> const & info)
        : Scv_{std::move(Scv)}, DNCs_{std::move(DNCs)}, md_{std::move(md)} {
        rapidcsv::Document const csv{info.csv_path, rapidcsv::LabelParams(0, 0)};
        std::ifstream json_file(info.json_path);
        if (not json_file) { std::terminate(); }
        nlohmann::json json = nlohmann::json::parse(json_file);
        std::span<Real const> const view{Scv_.begin(), Scv_.end()};
        int const Pj_index = csv.GetCell<int>("batch", info.name);
        batch_             = {
            {csv.GetCell<Real>("p1_min", info.name), csv.GetCell<Real>("p1_max", info.name)},
            {csv.GetCell<Real>("p2_min", info.name), csv.GetCell<Real>("p2_max", info.name)},
            {csv.GetCell<Real>("p3_min", info.name), csv.GetCell<Real>("p3_max", info.name)},
            { csv.GetCell<Real>("d_min", info.name),  csv.GetCell<Real>("d_max", info.name)},
            { csv.GetCell<Real>("e_min", info.name),  csv.GetCell<Real>("e_max", info.name)},
            json[info.name]["dmax"],
            view.subspan(Pj_index * stride, query_size)
        };
      }
#endif

#if defined(SEIZURE_JSON_SUPPORT)
      template <typename StringType, typename PathType>
      [[nodiscard]] auto ranges(Patient_information<StringType, PathType> const & info) const {
        std::vector<std::pair<Epoch_index, Epoch_index>> pairs{};
        std::ifstream json_file(info.json_path);
        if (not json_file) { return pairs; }
        nlohmann::json json = nlohmann::json::parse(json_file);
        std::string list{json[info.name]["validation"]};
        for (auto & chr : list) {
          if (chr == '(') {
            chr = '[';
          } else if (chr == ')') {
            chr = ']';
          }
        }
        nlohmann::json pairs_json = nlohmann::json::parse(list);
        for (auto const & pair : pairs_json) { pairs.emplace_back(pair[0], pair[1]); }
        return pairs;
      }
#endif

      Validator(Data_channel Scv, Bool_channel DNCs, Metadata md)
        : Scv_{std::move(Scv)}, DNCs_{std::move(DNCs)}, md_{std::move(md)} {
        std::span<Real const> const view{Scv_.begin(), Scv_.end()};
        constexpr Real inf = std::numeric_limits<Real>::infinity();
        batch_             = {
            {-inf, inf},
            {-inf, inf},
            {-inf, inf},
            {-inf, inf},
            {-inf, inf},
            inf,
            view.subspan(0 * stride, query_size)
        };
      }

      template <std::integral Count = long>
      [[nodiscard]] auto validate() const {
        return validate(0, (Scv_.size() - epoch) / stride + 1);
      }

      // FIXME: ¿Qué se hace si hay un ataque epiléptico que ha sido filtrado
      //        por los bounds?
      template <std::integral Count = long>
      [[nodiscard]] auto validate(Epoch_index from, Epoch_index to) const {
        std::span<Real const> const full_scv_view{Scv_.begin(), Scv_.end()};
        auto const scv_view{full_scv_view.subspan(from * stride, (to - from) * stride)};
        auto const in_bounds = filter_S(scv_view);
        auto window_view     = scv_view | sliding_window_view(epoch, stride);
        auto indices         = std::views::iota(from, to);
        Real const max       = batch_.d_max_c * d_th<Real>;
        auto metadata        = md_;
        for (auto && [epoch, idx] : ranges::views::zip(window_view, indices)) {
          if (not DNCs_[idx] and in_bounds[idx - from]) {
            Real const dist = min_distance(epoch, max);
            if (dist < max) {
              metadata.check_seizure(idx);
            } else {
              metadata.check_not_seizure(idx);
            }
          }
        }
        return metadata.get_counters<Count>();
      }

        /*
        auto counters        = parallel_for_sum<Counters<Count>>(
            ranges::views::zip(window_view, indices), [&](auto && counters, auto && params) {
              auto && [epoch, idx] = params;
              if (not DNCs_[idx] and in_bounds[idx - from]) {
                Real const dist = min_distance(epoch, max);
                if (dist < max) {
                  metadata.check_seizure(idx);
                } else {
                  metadata.check_not_seizure(idx);
                }
              }
            });
        return counters;
        */

    private:
      Data_channel Scv_;
      Bool_channel DNCs_;
      Metadata md_;
      Batch<Real, Query_channel> batch_;
  };

  template <class Data_channel, class Query_channel, class Bool_channel>
  Validator(Data_channel, Bool_channel, Metadata,
            Batch<Channel_value_type<Data_channel>, Query_channel>)
      -> Validator<Channel_value_type<Data_channel>, Data_channel, Query_channel, Bool_channel>;

  template <class Data_channel, class Bool_channel, typename StringType, typename PathType>
  Validator(Data_channel, Bool_channel, Metadata,
            Patient_information<StringType, PathType> const & info)
      -> Validator<Channel_value_type<Data_channel>, Data_channel,
                   std::span<std::add_const_t<Channel_value_type<Data_channel>>>, Bool_channel>;

  template <class Data_channel, class Bool_channel>
  Validator(Data_channel, Bool_channel, Metadata)
      -> Validator<Channel_value_type<Data_channel>, Data_channel,
                   std::span<std::add_const_t<Channel_value_type<Data_channel>>>, Bool_channel>;

}  // namespace Seizure

#endif  // SEIZURE_VALIDATOR_HPP
