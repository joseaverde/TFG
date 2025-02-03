#ifndef SEIZURE_PARALLEL_HPP
#define SEIZURE_PARALLEL_HPP

#include "views.hpp"

#include <thread>
#include <tuple>
#include <range/v3/all.hpp>

#ifndef SEIZURE_MAX_THREADS
  #define SEIZURE_MAX_THREADS 1
#endif

namespace Seizure {

  int parallel_max_threads() {
    return SEIZURE_MAX_THREADS;
  }

  template <class Range, class Func>
  void parallel_for(Range && rng, Func && func) {
    std::vector<std::thread> pool;
    for (auto && chunk :
         chunk_split_view(std::forward<decltype(rng)>(rng), parallel_max_threads())) {
      pool.emplace_back(
          [&func](auto && chunk) {
            for (auto && item : chunk) { std::apply(std::forward<decltype(func)>(func), item); }
          },
          chunk);
    }
    for (auto & thread : pool) { thread.join(); }
  }

  template <class Range, class Func>
  void parallel_for_with_index(Range && rng, Func && func) {
    std::vector<std::thread> pool;
    for (auto && [chunk, index] : ranges::views::zip(
          chunk_split_view(std::forward<decltype(rng)>(rng), parallel_max_threads()),
          ranges::views::iota(0))) {
      pool.emplace_back(
          [&func] (auto && chunk, int index) {
          for (auto && item : chunk) {
            std::apply(std::forward<decltype(func)>(func), std::make_tuple(index, item));
          }},
          chunk, index);
    }
    for (auto & thread : pool) { thread.join(); }
  }

  template <class Accumulator, class Range, class Func>
  Accumulator parallel_for_sum(Range && rng, Func && func) {
    std::vector<Accumulator> accs(parallel_max_threads(), Accumulator{});
    std::vector<std::thread> pool;
    for (auto && [chunk, index] : ranges::views::zip(
          chunk_split_view(std::forward<decltype(rng)>(rng), parallel_max_threads()),
          ranges::views::iota(0))) {
      pool.emplace_back(
          [&func,&accs] (auto && chunk, int index) {
          for (auto && item : chunk) {
            std::apply(std::forward<decltype(func)>(func), std::make_tuple(&(accs[index]), item));
          }},
          chunk, index);
    }
    for (auto & thread : pool) { thread.join(); }
    Accumulator result{};
    for (auto const & acc : accs) { result = result + acc; }
    return result;
  }

}  // namespace Seizure

#endif  // SEIZURE_PARALLEL_HPP
