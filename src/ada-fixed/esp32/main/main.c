#include <stdio.h>
#include <sys/time.h>

#define DENOMINATOR (1 << 16)
#define NUMERATOR (DENOMINATOR >> 3)
#define SAMPLE_LAST 10000
#define STRIDE 256
#define TIMED_STRIDES 10

extern void SeizureDetectorFixedinit(void);
extern void _Noreturn seizure_detector(void);

void __gnat_stop (void) {
  puts("Something went wrong!");
  abort();
}

void eeg_putchar (char item) {
  putchar(item);
}

static double get_time () {
  struct timeval data;
  gettimeofday(&data, NULL);
  return ((double)data.tv_sec) + ((double)data.tv_usec) / 1000000.0;
}

static double start = 0.0;

void eeg_read_sample (int * num, int * den) {
  static int prev = 0;
  static unsigned int sample_count = 0;
  static unsigned int stride_count = 0;

  *num = prev * NUMERATOR;
  *den = DENOMINATOR;
  prev = (prev + 1) % (SAMPLE_LAST * (DENOMINATOR / NUMERATOR));
  sample_count++;

  if (sample_count % STRIDE == 0) {
    stride_count++;
    if (stride_count % TIMED_STRIDES == 0) {
      double stop = get_time();
      printf("%lf epochs/second\n",  ((double)TIMED_STRIDES) / (stop - start));
      start = stop;
      stride_count = 0;
    }
  }

}

void app_main (void) {
  start = get_time();
  SeizureDetectorFixedinit();
  seizure_detector();
}
