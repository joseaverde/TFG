/// @file   fftw++.hpp
/// @brief  Helper header for interacting with FFTW3
/// @author José Antonio Verde Jiménez
///
/// Depending on the data type, FFTW3 uses a different function name or type
/// name. For instance for `float' the complex type is `fftwf_complex'; for
/// `double', is `fftw_complex'; and for `long double', is `fftwl_complex'. And
/// the same is true for every single function.
///
/// This header declares a templatised struct named FFTW that is specialised
/// for `float', `double' and `long double' for the functions used in `Welch'.
///
/// This file doesn't attempt to cover the whole FFTW3 library. Just the parts
/// that are being used internally in code.

#ifndef SEIZURE_FFTWPP_HPP
#define SEIZURE_FFTWPP_HPP

#include <concepts>
#include <type_traits>
#include <complex>

namespace Seizure {

  constexpr int FFTW_ESTIMATE = 0;

  template <typename T>
  struct FFTW {
    using Complex = std::complex<T>;
    class Plan {
      public:
        Plan (int n, T * in, Complex * out) : n_{static_cast<std::size_t>(n)}, in_{in}, out_{out} { }
        void execute () {
          my_fft(std::span<const T>{in_, n_}, std::span<Complex>{out_, n_}, 1);
        }
      private:

        std::size_t n_;
        T * in_;
        Complex * out_;

        // TODO: Don't use subspan, thank you
        static void my_fft (std::span<const T> const & input,
                            std::span<Complex> const & output, int s) {
          using namespace std::numbers;
          auto const N = std::ssize(input);
          if (N == 1) {
            *output.begin() = {*input.begin(), 0};
          } else if (N % 2 == 0) {
            my_fft(input.subspan(0, N / 2), output.subspan(0, N / 2), 2 * s);
            my_fft(input.subspan(s, N / 2), output.subspan(N / 2, N / 2), 2 * s);
            for (int k = 0; k < N / 2; ++k) {
              Complex p = output[k];
              Complex q = std::exp(Complex{0, -2 * pi_v<T> * k / N}) * output[k + N/2];
              output[k] = p + q;
              output[k + N/2] = p - q;
            }
          } else {
            for (int k = 0; k < N; ++k) {
              output[k] = {0, 0};
              for (int n = 0; n < N; ++n) {
                output[k] += input[n * s] * std::exp(Complex{0, -2 * pi_v<T> / N * k * n});
              }
            }
          }
        }


    };

    static Plan dft_r2c_1d (int n, T * in, Complex * out, unsigned) { return Plan{n, in, out}; }
    static void execute (Plan plan) { plan.execute(); }
    struct destroy { void operator() (Plan) {} };
  };

  /// @brief Renames a std::unique_ptr for a plan type, it has the needed
  /// destructor.
  template <typename T>
  using FFTW_plan = FFTW<T>::Plan;
}  // namespace Seizure

#endif  // SEIZURE_FFTWPP_HPP
