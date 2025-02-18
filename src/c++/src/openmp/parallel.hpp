#ifndef SEIZURE_PARALLEL_HPP
#define SEIZURE_PARALLEL_HPP

#include <omp.h>
#include <vector>

namespace Seizure {

  int parallel_max_threads() {
    return omp_get_max_threads();
  }

  template <class Range, class Func>
  void parallel_for(Range && rng, Func && func) {
    #pragma omp parallel for
    for (auto && item : std::forward<decltype(rng)>(rng)) {
      std::apply(std::forward<decltype(func)>(func), item);
    }
  }

  template <class Range, class Func>
  void parallel_for_with_index(Range && rng, Func && func) {
    #pragma omp parallel for
    for (auto && item : std::forward<decltype(rng)>(rng)) {
      std::apply(std::forward<decltype(func)>(func), std::make_tuple(omp_get_thread_num(), item));
    }
  }

  template <class Accumulator, class Range, class Func>
  Accumulator parallel_for_sum(Range && rng, Func && func) {
    std::vector<Accumulator> pool(omp_get_max_threads(), Accumulator{});
    auto my_rng = std::move(std::forward<decltype(rng)>(rng));
    #pragma omp parallel for
    for (std::size_t index = 0; index < std::size(my_rng); index++) {
      std::apply(std::forward<decltype(func)>(func),
                 std::make_tuple(&pool[omp_get_thread_num()], *(my_rng.begin() + index)));
    }
    Accumulator result{};
    for (auto const & item : pool) { result = result + item; }
    return result;
  }

}  // namespace Seizure

#endif  // SEIZURE_PARALLEL_HPP
