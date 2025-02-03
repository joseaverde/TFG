#ifndef SEIZURE_PARALLEL_HPP
#define SEIZURE_PARALLEL_HPP

#include <tuple>

namespace Seizure {

  int parallel_max_threads() {
    return 1;
  }

  template <class Range, class Func>
  void parallel_for(Range && rng, Func && func) {
    for (auto && item : std::forward<decltype(rng)>(rng)) {
      std::apply(std::forward<decltype(func)>(func), item);
    }
  }

  template <class Range, class Func>
  void parallel_for_with_index(Range && rng, Func && func) {
    for (auto && item : std::forward<decltype(rng)>(rng)) {
      std::apply(std::forward<decltype(func)>(func), std::make_tuple(0, item));
    }
  }

  template <class Accumulator, class Range, class Func>
  Accumulator parallel_for_sum(Range && rng, Func && func) {
    Accumulator result{};
    for (auto && item : std::forward<decltype(rng)>(rng)) {
      std::apply(std::forward<decltype(func)>(func), std::make_tuple(&result, item));
    }
    return result;
  }

}  // namespace Seizure

#endif//SEIZURE_PARALLEL_HPP
