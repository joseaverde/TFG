#ifndef SEIZURE_VIEWS_HPP
#define SEIZURE_VIEWS_HPP

#include <range/v3/all.hpp>

namespace Seizure {

  constexpr auto sliding_window_view(auto size, auto stride) {
    return ranges::views::sliding(size) | ranges::views::stride(stride);
  }

  constexpr auto chunk_split_view(auto && view, auto size) {
    return view | ranges::views::chunk(view.size() / size + 1);
  }
}  // namespace Seizure

#endif  // SEIZURE_VIEWS_HPP
