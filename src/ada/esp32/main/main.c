#include <stdio.h>
#include <time.h>
extern void adainit (void);
extern void adafinal (void);
extern void seizure_detector_benchmark (void);
void __gnat_stop (void) {
  puts("ERROR");
  abort();
}

volatile float patata;

#include <math.h>

void app_main (void) {
  adainit();
  printf("Starting\n");
  seizure_detector_benchmark();
  printf("Stopping\n");
  adainit();
}
