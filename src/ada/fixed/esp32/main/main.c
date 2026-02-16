#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>

#define DENOMINATOR (1 << 16)
#define NUMERATOR (DENOMINATOR >> 3)
#define SAMPLE_LAST 10000
#define STRIDE 256
#define TIMED_STRIDES 10
#define EPOCH_SIZE 1280

// Constructor
extern void detector_binding_init(void);
extern void detector_init(void);

typedef int32_t sample_t;
typedef int64_t feature_t;
typedef sample_t epoch_t[EPOCH_SIZE];

// Benchmarking functions
extern void eeg_max_distance (void*, feature_t*);
extern void eeg_mean         (void*, feature_t*);
extern void eeg_energy       (void*, feature_t*);
extern void old_eeg_batch_normalise(feature_t *result);
extern void old_eeg_dtw(feature_t *result);
extern void old_eeg_fft(feature_t *result);
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
  feature_t x;
  epoch_t * epoch = malloc(sizeof(sample_t) * EPOCH_SIZE);
  int count;
  double start, stop;
  printf("Benchmarking\n");

#define OLD_BENCHMARK(func)                                                   \
  start = get_time();                                                         \
  count = 0;                                                                  \
  do {                                                                        \
    func(&x);                                                                 \
    count++;                                                                  \
    stop = get_time();                                                        \
  } while (0)

#define BENCHMARK(func)                                                       \
  count = 1;                                                                  \
  start = get_time();                                                         \
  func(epoch, &x);                                                            \
  stop  = get_time();

  BENCHMARK(eeg_max_distance);
  printf("Max_distance: %.0lf epochs/second\n", count / (stop - start));
  BENCHMARK(eeg_mean);
  printf("Mean:         %.0lf epochs/second\n", count / (stop - start));
  BENCHMARK(eeg_energy);
  printf("Energy:       %.0lf epochs/second\n", count / (stop - start));
  OLD_BENCHMARK(old_eeg_batch_normalise);
  printf("Batch Normal: %.0lf epochs/second\n", count / (stop - start));
  OLD_BENCHMARK(old_eeg_dtw);
  printf("Single DTW:   %.0lf epochs/second\n", count / (stop - start));
  OLD_BENCHMARK(old_eeg_fft);
  printf("FFT:          %.0lf epochs/second\n", count / (stop - start));

  free(epoch);
}

void app_main (void) {
  detector_init();
  detector_binding_init();
  benchmark();
  seizure_detector();
}
