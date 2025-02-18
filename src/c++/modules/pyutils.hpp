#ifndef PYUTILS_HPP
#define PYUTILS_HPP

#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <span>

template <typename T>
constexpr std::span<T const> pyspan(pybind11::array_t<T> const & y) {
  return std::span{y.data(), static_cast<std::size_t>(y.size())};
}

template <class C>
auto pymove(C && y) -> pybind11::array_t<typename C::value_type> {
  using result_t = typename C::value_type;
  // NOLINTNEXTLINE (cppcoreguidelines-owning-memory)
  auto vector  = new C(std::forward<decltype(y)>(y));
  auto capsule = pybind11::capsule{vector, [](void * item) {
                                     // NOLINTNEXTLINE (cppcoreguidelines-owning-memory)
                                     delete static_cast<C *>(item);
                                   }};
  return pybind11::array_t<result_t>(vector->size(), vector->data(), capsule);
}

#endif//PYUTILS_HPP
