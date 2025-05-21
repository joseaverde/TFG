#ifndef SEIZURE_PARALLEL_HPP
#define SEIZURE_PARALLEL_HPP

#include <oneapi/tbb.h>
#include <tuple>

namespace Seizure {

  int parallel_max_threads() {
    return tbb::this_task_arena::max_concurrency();
  }

  template <class Range, class Func>
  void parallel_for(Range && rng, Func && func) {
    auto const range = std::move(std::forward<decltype(rng)>(rng));
    oneapi::tbb::parallel_for_each(range.begin(), range.end(), [&](auto && item) {
      std::apply(func, item);
    });
  }

  template <class Range, class Func>
  void parallel_for_with_index(Range && rng, Func && func) {
    auto const range = std::move(std::forward<decltype(rng)>(rng));
    oneapi::tbb::parallel_for_each(range.begin(), range.end(), [&](auto && item) {
      std::apply(func, std::make_tuple(tbb::this_task_arena::current_thread_index(), item));
    });
  }

  template <class Accumulator, class Range, class Func>
  Accumulator parallel_for_sum(Range && rng, Func && func) {
    auto const range = std::move(std::forward<decltype(rng)>(rng));
    return tbb::parallel_reduce(
        tbb::blocked_range(range.begin(), range.end()),
        Accumulator{},
        [&](auto const & indices, Accumulator acc) {
          Accumulator result = acc;
          for (auto && item : indices) {
            std::apply(std::forward<decltype(func)>(func),
                       std::make_tuple(&result, item));
          }
          return result;
        },
        [](Accumulator const & lhs, Accumulator const & rhs) {
          return lhs + rhs;
        });
  }

}  // namespace Seizure

#endif  // SEIZURE_PARALLEL_HPP
