#include "benchmark-signals_.hpp"
#include "benchmark-validation_.hpp"

#include <benchmark/benchmark.h>

namespace {
  using namespace BenchmarkSignals_;
  using namespace BenchmarkValidation_;
// NOLINTBEGIN
  BENCHMARK(Simpson<float>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Simpson<double>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Simpson<long double>)->Unit(benchmark::kMillisecond);

  BENCHMARK(Welch<float>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Welch<double>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Welch<long double>)->Unit(benchmark::kMillisecond);

  BENCHMARK(Welch_Warmup<float>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Welch_Warmup<double>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Welch_Warmup<long double>)->Unit(benchmark::kMillisecond);

  BENCHMARK(Max_Dist<float>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Max_Dist<double>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Max_Dist<long double>)->Unit(benchmark::kMillisecond);

  BENCHMARK(Energy<float>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Energy<double>)->Unit(benchmark::kMillisecond);
  BENCHMARK(Energy<long double>)->Unit(benchmark::kMillisecond);

  BENCHMARK(PSD<float>)->Unit(benchmark::kMillisecond);
  BENCHMARK(PSD<double>)->Unit(benchmark::kMillisecond);
  BENCHMARK(PSD<long double>)->Unit(benchmark::kMillisecond);

  BENCHMARK(benchmark_validation<float>)
      ->Unit(benchmark::kMillisecond)
      ->Args({3, BenchmarkValidation_::sEpoPat * 8, 3})
      ->Args({3, BenchmarkValidation_::sEpoPat * 16, 3});
  BENCHMARK(benchmark_validation<double>)
      ->Unit(benchmark::kMillisecond)
      ->Args({3, BenchmarkValidation_::sEpoPat * 8, 3})
      ->Args({3, BenchmarkValidation_::sEpoPat * 16, 3});
  BENCHMARK(benchmark_validation<long double>)
      ->Unit(benchmark::kMillisecond)
      ->Args({3, BenchmarkValidation_::sEpoPat * 8, 3})
      ->Args({3, BenchmarkValidation_::sEpoPat * 16, 3});

  BENCHMARK(benchmark_prevalidation<float>)
      ->Unit(benchmark::kMillisecond)
      ->Args({3, BenchmarkValidation_::sEpoPat * 8, 3})
      ->Args({3, BenchmarkValidation_::sEpoPat * 16, 3});
  BENCHMARK(benchmark_prevalidation<double>)
      ->Unit(benchmark::kMillisecond)
      ->Args({3, BenchmarkValidation_::sEpoPat * 8, 3})
      ->Args({3, BenchmarkValidation_::sEpoPat * 16, 3});
  BENCHMARK(benchmark_prevalidation<long double>)
      ->Unit(benchmark::kMillisecond)
      ->Args({3, BenchmarkValidation_::sEpoPat * 8, 3})
      ->Args({3, BenchmarkValidation_::sEpoPat * 16, 3});
// NOLINTEND
}  // namespace

namespace Benchmarks {
  int benchmark(int argc = 0, char ** argv = nullptr) {
    // NOLINTBEGIN
    char arg0_default[] = "benchmark";
    char * args_default = arg0_default;
    // NOLINTEND
    if (argv == nullptr or argc == 0) {
      argc = 1;
      argv = &args_default;
    }
    ::benchmark::Initialize(&argc, argv);
    if (::benchmark::ReportUnrecognizedArguments(argc, argv)) { return 1; }
    ::benchmark::RunSpecifiedBenchmarks();
    ::benchmark::Shutdown();
    return 0;
  }
}  // namespace Benchmarks
