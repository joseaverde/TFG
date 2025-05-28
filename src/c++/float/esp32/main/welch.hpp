/// @file   welch.hpp
/// @brief  Implementation of Welch's method for Power Spectral Density Computation
/// @author José Antonio Verde Jiménez
///
/// This header contains the class Welch that implements the Welch's method for
/// calculating the Power Spectral Density of a signal.
///
/// The method is kept in a class instead of a function because:
///  * FFTW3 needs to keep an expensive `plan' object that must be precalculated
///  * The Window function is expensive and should be precalculated.
///
/// @warning This class is NOT THREAD SAFE, it keeps an internal buffer to
/// compute the Fast Fourier Transform which is associated with the FFTW3 `plan'
/// object. And thus, it is not possible to call concurrently the same object.
///
/// @see https://ccrma.stanford.edu/~jos/sasp/Welch_s_Method.html
/// @see https://ieeexplore.ieee.org/abstract/document/6525419
/// @see https://github.com/scipy/scipy/blob/f990b1d2471748c79bc4260baf8923db0a5248af/scipy/signal/_spectral_py.py#L1857
/// @see https://en.wikipedia.org/wiki/Welch%27s_method

#ifndef SEIZURE_WELCH_HPP
#define SEIZURE_WELCH_HPP

#include "fftw++.hpp"
#include "types.hpp"
#include "views.hpp"

#include <complex>
#include <concepts>
#include <gsl/gsl>

namespace Seizure {

  using Welch_window_size = std::ptrdiff_t;

  namespace Windows {
    /// @namespace Windows
    /// @brief This namespace contains several Window function which can be used
    /// for calculating the Power Spectral Density in Welch class.
    template <Real_type Real>
    constexpr Real hann(Welch_window_size window_size, int i) {
      // NOLINTNEXTLINE (cppcoreguidelines-avoid-magic-numbers)
      return 0.5 - 0.5 * std::cos(2.0 * std::numbers::pi_v<Real> * i /
                                  static_cast<Real>(window_size - 1));
    }
  }  // namespace Windows

  template <Real_type Real, Real (*Window)(Welch_window_size, int)>
  class Welch {
      /// @class  Welch
      /// @brief  This class implements the Power Spectral Density using Welch method.
      /// @tparam Real
      /// A floating-point type supported by FFTW3 (float, double, long double)
      /// Keep in mind that you must link with the correct library in order to
      /// to use it

      using Fftw           = FFTW<Real>;
      using Plan           = FFTW_plan<Real>;
      using Complex        = std::complex<Real>;
      using Complex_buffer = std::vector<Complex>;
      using Channel        = std::vector<Real>;

      static constexpr Real squared(Real x) { return x * x; }

      static constexpr Real norm_squared(Complex c) {
        return squared(c.real()) + squared(c.imag());
      }

      static constexpr Real correction_factor(Welch_window_size size) {
        return accumulate(ranges::views::ints(0, static_cast<int>(size)) |
                              ranges::views::transform([size](int i) -> Real {
                                return squared(Window(size, i));
                              }),
                          Real{});
      }

    public:
      /// @tparam WindowSize
      /// A positive integer with the size of the Window used for calculating the
      /// Power Spectral Density. This number must be greater than 1.
      /// @tparam Window
      /// A Window function.
      constexpr Welch(Welch_window_size window_size) noexcept(noexcept(Window))
        : window_size_{window_size}, values_(window_size_), result_(window_size_, {0.0, 0.0}),
          window_{ranges::views::iota(0, window_size_) |
                  ranges::views::transform([this](auto const index) {
                    return Window(window_size_, index);
                  }) |
                  ranges::to<Channel>()},
          plan_{Fftw::dft_r2c_1d(window_size_, values_.data(),
                                 // NOLINTNEXTLINE (cppcoreguidelines-pro-type-reinterpret-cast)
                                 reinterpret_cast<Fftw::Complex *>(result_.data()), FFTW_ESTIMATE)},
          normalisation_factor_{Welch::correction_factor(window_size_)} { }

      /// @brief Calculate the Power Spectral Density
      /// @param x
      /// A constant view over a signal.
      /// @param freq
      /// Sample frequency of the `x' time series.
      /// @param overlap
      /// How much does the window overlap. For Welch's method using the `hann'
      /// Window, the half of the Window's size is commonly used.
      constexpr Channel operator()(Input_channel_of<Real> auto const & x, Real freq, Sample_count overlap) {
        // Precondition((overlap > 0 && overlap < window_size_) && (std::ssize(x) >= window_size_));
        Channel Pxx(window_size_ / 2 + 1, Real{});
        Sample_count const steps  = (std::ssize(x) - window_size_) / overlap + 1;
        freq                     /= 2;
        auto const windows        = x | sliding_window_view(window_size_, window_size_ - overlap);
        for (auto && win : windows) {
          auto win_view = ranges::views::zip_with(std::multiplies<Real>{}, win, window_);
          std::ranges::copy(win_view.begin(), win_view.end(), values_.begin());
          Fftw::execute(plan_);
          for (auto && [pxx_i, res_i] : ranges::views::zip(Pxx, result_)) {
            pxx_i += norm_squared(res_i) / (normalisation_factor_ * freq);
          }
        }
        Pxx |= ranges::actions::transform([steps](Real val) {
          return val / steps;
        });
        return Pxx;
      }

      constexpr void freq_range(Real freq, Real & step, Real & stop) {
        stop = freq / 2;
        step = freq / window_size_;
      }

    private:
      Welch_window_size window_size_;
      Channel values_;
      Complex_buffer result_;
      Channel window_;
      Plan plan_;
      Real normalisation_factor_{};
  };

}  // namespace Seizure

#endif  // SEIZURE_WELCH_HPP
