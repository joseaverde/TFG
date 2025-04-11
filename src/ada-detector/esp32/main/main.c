#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>

#define DENOMINATOR (1 << 16)
#define NUMERATOR (DENOMINATOR >> 3)
#define SAMPLE_LAST 10000
#define STRIDE 256
#define TIMED_STRIDES 10

// Constructor
extern void SeizureDetectorFixedinit(void);
extern void detector_bindinginit(void);

// Benchmarking functions
extern void eeg_max_distance(int32_t *result);
extern void eeg_energy(int32_t *result);
extern void eeg_batch_normalise(int32_t *result);
extern void eeg_dtw(int32_t *result);
extern void eeg_fft(int32_t *result);
extern void _Noreturn seizure_detector(void);

// Reader
#define SCALE 32
void eeg_read_sample(int *value) {
  static int curr_num = -SAMPLE_LAST;
  *value = curr_num;
  if (++curr_num > SAMPLE_LAST) { curr_num = -SAMPLE_LAST; }
}

void __gnat_stop (void) {
  puts("Something went wrong!");
  abort();
}

static double get_time () {
  struct timeval data;
  gettimeofday(&data, NULL);
  return ((double)data.tv_sec) + ((double)data.tv_usec) / 1000000.0;
}

#define MAX_BENCHMARK_TIME 1.0
static void benchmark (void) {
  int32_t x;
  int count;
  double start, stop;
  printf("Benchmarking\n");

#define BENCHMARK(func)                                                       \
  start = get_time();                                                         \
  count = 0;                                                                  \
  do {                                                                        \
    func(&x);                                                                 \
    count++;                                                                  \
    stop = get_time();                                                        \
  } while (stop - start < MAX_BENCHMARK_TIME)

  BENCHMARK(eeg_max_distance);
  printf("Max_distance: %.0lf epochs/second\n", count / (stop - start));
  BENCHMARK(eeg_energy);
  printf("Energy:       %.0lf epochs/second\n", count / (stop - start));
  BENCHMARK(eeg_batch_normalise);
  printf("Batch Normal: %.0lf epochs/second\n", count / (stop - start));
  BENCHMARK(eeg_dtw);
  printf("Single DTW:   %.0lf epochs/second\n", count / (stop - start));
  BENCHMARK(eeg_fft);
  printf("FFT:          %.0lf epochs/second\n", count / (stop - start));
}

void app_main (void) {
  SeizureDetectorFixedinit();
  detector_bindinginit();
  benchmark();
  seizure_detector();
}
