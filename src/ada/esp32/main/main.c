#include <stdio.h>
#include <time.h>
extern void adainit (void);
extern void adafinal (void);
extern void seizure_detector_benchmark (void);
void __gnat_stop (void) {
  puts("ERROR");
  abort();
}

void app_main (void) {
  adainit();
  printf("Starting\n");
  seizure_detector_benchmark();
  printf("%d\n", sizeof(time_t));
  adainit();
}
