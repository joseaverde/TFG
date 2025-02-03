#ifndef SEIZURE_CONCEPTS_HPP
#define SEIZURE_CONCEPTS_HPP

#include <concepts>
#include <ranges>

namespace Seizure {

  template <typename T>
  concept Real_type = std::floating_point<T>;

  template <typename Container>
  concept Input_channel = std::ranges::input_range<Container>;

  template <Input_channel Channel>
  using Channel_value_type = std::remove_cvref_t<std::ranges::range_value_t<Channel>>;

  template <typename Channel>
  concept Real_input_channel =
      Input_channel<Channel> and Real_type<Channel_value_type<Channel>>;

  template <Input_channel ic1, Input_channel ic2>
    requires std::same_as<std::ranges::range_value_t<ic1>, std::ranges::range_value_t<ic2>>
  using Common_channel_value_type = std::ranges::range_value_t<ic1>;

  template <typename Container, typename Of>
  concept Input_channel_of =
      Input_channel<Container> and std::same_as<Channel_value_type<Container>, Of>;

}  // namespace Seizure

#endif  // SEIZURE_CONCEPTS_HPP
