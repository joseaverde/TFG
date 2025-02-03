#ifndef SEIZURE_ALGORITHMS_HPP
#define SEIZURE_ALGORITHMS_HPP

#include "concepts.hpp"

#include <numeric>

namespace Seizure {

  constexpr auto accumulate(Input_channel auto const & cont,
                            Channel_value_type<decltype(cont)> acc) {
    return std::accumulate(cont.begin(), cont.end(), acc);
  }

  constexpr auto accumulate(Input_channel auto const & cont, Channel_value_type<decltype(cont)> acc,
                            auto func) {
    return std::accumulate(cont.begin(), cont.end(), acc, func);
  }
}  // namespace Seizure

#endif  // SEIZURE_ALGORITHMS_HPP
