#include <iostream>
#include <vector>
#include <cstdint>
#include <cnl/all.h>
#include <chrono>
#include <concepts>

// ==== Fixed type creation ====

template <int bits, int exp, bool elastic>
struct Fixed_type_helper;

template <int exp>
struct Fixed_type_helper<32, exp, false> {
  using type = cnl::scaled_integer<int32_t, cnl::power<-exp>>;
};

template <int exp>
struct Fixed_type_helper<64, exp, false> {
  using type = cnl::scaled_integer<int64_t, cnl::power<-exp>>;
};

template <int exp>
struct Fixed_type_helper<32, exp, true> {
  using type = cnl::elastic_scaled_integer<31, cnl::power<-exp>, int32_t>;
};

template <int exp>
struct Fixed_type_helper<64, exp, true> {
  using type = cnl::scaled_integer<cnl::wide_integer<128, int32_t>, cnl::power<-exp>>;
};

template <typename Result, typename Left, typename Right>
Result multiply (Left left, Right right) {
  return Result{0.0};
}

template <int bits, int exp, bool elastic = true>
using Fixed_type = Fixed_type_helper<bits, exp, elastic>::type;

// Patata

using Raw_sample = Fixed_type<32, 20, true>;
using Feature = Fixed_type<64, 20, true>;
using Sample = Fixed_type<32, 31, true>;
using Big_sample = Fixed_type<64, 63, true>;

auto acc (std::vector<Sample> const & iterable) {
  using Internal = cnl::scaled_integer<int64_t, cnl::power<-31>>;
  cnl::convert<cnl::undefined_overflow_tag, Internal> to_internal{};
  Internal result{0.0};
  for (auto it : iterable) { result += to_internal(it); }
  return result;
}

Sample mean (std::vector<Sample> const & iterable) {
  return acc(iterable) / iterable.size();
}

Sample max_distance (std::vector<Sample> const & iterable) {
  Sample min = *iterable.begin();
  Sample max = min;
  for (auto it : iterable) {
    min = std::min(min, it);
    max = std::max(max, it);
  }
  return max - min;
}

template <typename T> struct PrintName;

auto quarter_variance (std::vector<Sample> const & iterable) {
  using Internal = cnl::scaled_integer<int64_t, cnl::power<-40>>;
  Sample const m = mean(iterable);
  Internal result{0.0};
  for (auto it : iterable) {
    Sample x = m / 2 - it / 2;
    result += Internal{x * x};
  }
  result = result / iterable.size();
  return result;
}

template <typename Result>
Result energy (std::vector<Sample> const & iterable, auto normalisation_factor) {
  auto var = quarter_variance(iterable);
  // return Result{var} * Result{Result{4 * normalisation_factor} * normalisation_factor};
  multiply<Result>(var, normalisation_factor);
  return Result{Result{Result{var} * 4} * normalisation_factor} * normalisation_factor;
}

extern "C" void black_void (void *);
extern "C" void eeg_read_sample(int *value);

void read_sample (std::vector<Sample> & values) {
  int num;
  for (auto & it : values) {
    eeg_read_sample(&num);
    it = cnl::quotient(num, 10000);
  }
}

template <typename Func, typename ... Args>
void benchmark (std::string const & name, Func func, Args ... args) {
  std::cout << "==== " << name << " ====\n";

  const auto start{std::chrono::high_resolution_clock::now()};
  auto dummy = func(args ...);
  black_void(&dummy);
  const auto stop{std::chrono::high_resolution_clock::now()};

  const std::chrono::duration<double> elapsed{stop - start};
  std::cout << "Result: " << dummy << "\n";
  std::cout << "Elapsed: " << elapsed << "\n";
  std::cout << "Rate: " << 1 / elapsed.count()
            << " epochs/second\n"<< std::endl;
}

extern "C" void app_main () {
  constexpr int epoch_size = 1280;

  std::vector<Sample> values;
  values.reserve(epoch_size);
  for (int i = 0; i < epoch_size; ++i) { values.push_back(0); }
  read_sample(values);

  benchmark("Max_Distance", max_distance, values);
  benchmark("Mean", mean, values);
  benchmark("Energy", [] (auto x) { return energy<Feature>(x, Raw_sample{16384.0}); }, values);
}
