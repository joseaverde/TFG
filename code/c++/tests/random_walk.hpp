#ifndef RANDOM_WALK_HPP
#define RANDOM_WALK_HPP

#include <concepts>
#include <random>
#include <gsl/gsl>
#include <vector>

template <std::floating_point Real>
std::vector<Real> random_walk(ptrdiff_t size) {
  std::random_device seed;
  std::mt19937_64 mte{seed()};
  // NOLINTNEXTLINE (cppcoreguidelines-avlid-magic-numbers)
  std::uniform_real_distribution<Real> uniform{-0.5, 0.5};
  std::vector<Real> result(size, Real{});
  result[0] = uniform(mte);
  for (gsl::index i = 1; i < size; ++i) { result[i] = uniform(mte) + result[i - 1]; }
  return result;
}

#endif//RANDOM_WALK_HPP
