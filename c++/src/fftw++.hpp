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
#include <fftw3.h>
#include <memory>
#include <type_traits>

namespace Seizure {

  /// @struct FFTW
  /// @brief Specifies static functions and types for a given typename T.
  ///
  /// It declares the following types:
  ///  * `plan_t':    fftw?_plan
  ///  * `complex_t': fftw?_complex
  ///
  /// The functions: dft_r2c_1d and execute
  ///
  /// And the functor `destroy' for using with std::unique_ptr.
  template <typename T>
  struct FFTW;

  template <>
  struct FFTW<float> {
      using Plan    = fftwf_plan;
      using Complex = fftwf_complex;

      static Plan dft_r2c_1d(int n, float * in, fftwf_complex * out, unsigned flags) {
        return fftwf_plan_dft_r2c_1d(n, in, out, flags);
      }

      static void execute(Plan const plan) { fftwf_execute(plan); }

      struct destroy {
          void operator()(Plan p) { fftwf_destroy_plan(p); }
      };
  };

  template <>
  struct FFTW<double> {
      using Plan    = fftw_plan;
      using Complex = fftw_complex;

      static Plan dft_r2c_1d(int n, double * in, fftw_complex * out, unsigned flags) {
        return fftw_plan_dft_r2c_1d(n, in, out, flags);
      }

      static void execute(Plan const plan) { fftw_execute(plan); }

      struct destroy {
          void operator()(Plan p) { fftw_destroy_plan(p); }
      };
  };

  template <>
  struct FFTW<long double> {
      using Plan    = fftwl_plan;
      using Complex = fftwl_complex;

      static Plan dft_r2c_1d(int n, long double * in, fftwl_complex * out, unsigned flags) {
        return fftwl_plan_dft_r2c_1d(n, in, out, flags);
      }

      static void execute(Plan const plan) { fftwl_execute(plan); }

      struct destroy {
          void operator()(Plan p) { fftwl_destroy_plan(p); }
      };
  };

  /// @brief Renames a std::unique_ptr for a plan type, it has the needed
  /// destructor.
  template <typename T>
    requires std::floating_point<T>
  using FFTW_plan = std::unique_ptr<typename std::remove_pointer_t<typename FFTW<T>::Plan>,
                                    typename FFTW<T>::destroy>;
}  // namespace Seizure

#endif  // SEIZURE_FFTWPP_HPP
