/*---------------------------------------------------------------------------*\
 *  Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved  *
 *---------------------------------------------------------------------------*
 * File:    benchmark.c                                                      *
 * Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>          *
 * License: European Union Public License 1.2                                *
\*---------------------------------------------------------------------------*/

#include "detector.h"

#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>   // TODO: Use <time.h>

static void json_int_field (const char *name, int value);
static void json_str_field (const char *name, const char *value);
static void json_list_field (const char *name);
static void json_list_append_double (double value);
static void json_end_list_field (void);
static int json_list_length = 0;

static void dump_configuration (void);
static void configure_batch (void);
static void benchmark_simple (double max_time);
static void benchmark_all (int iterations);

void __gnat_exit (void) {
  puts("}\nSomething went wrong!");
  fflush(stdout);
  for (;;) { }
}

/* *** ==== Main function ============================================== *** */

#if defined(IDF_PROJECT)
void app_main (void)
#else
int main ()
#endif
{
  detectorinit();
  dump_configuration();
  configure_batch();
  benchmark_simple(1.0);
  benchmark_all(10);
  printf("}\n");
  fflush(stdout);
}

/* *** ==== Implementation ============================================= *** */

/* *** JSON *** */

static int json_index = 0;

static void json_next_field_ (void) {
  putchar(json_index++ == 0 ? '{' : ',');
}

static void json_int_field (const char *name, int value) {
  json_next_field_();
  printf("\"%s\":%d", name, value);
}

static void json_str_field (const char *name, const char *value) {
  json_next_field_();
  printf("\"%s\":\"%s\"", name, value);
}

static void json_list_field (const char *name) {
  json_next_field_();
  json_list_length = 0;
  printf("\"%s\":[", name);
}

static void json_list_append_double (double value) {
  if (json_list_length++ != 0) { printf(","); }
  printf("%.9lf", value);
}

static void json_end_list_field (void) {
  printf("]");
}

/* *** Utility *** */

static void dump_configuration (void) {
  json_int_field("Sample_Size", DETECTOR_SAMPLE_SIZE);
  json_int_field("Feature_Size", DETECTOR_FEATURE_SIZE);
  json_int_field("Samples_Per_Stride", SAMPLES_PER_STRIDE);
  json_int_field("Strides_Per_Epoch", STRIDES_PER_EPOCH);
  json_str_field("Language", detector_language);
  json_str_field("Compiler", detector_compiler);
  json_str_field("Profile", detector_profile);
  json_str_field("Real_Type", detector_real_type);
  json_str_field("Target", detector_target);
}

static void configure_batch (void) {
  detector_batch_set_max_distance(detector_min_feature, detector_max_feature);
  detector_batch_set_energy      (detector_min_feature, detector_max_feature);
  detector_batch_set_psd_1       (detector_min_feature, detector_max_feature);
  detector_batch_set_psd_2       (detector_min_feature, detector_max_feature);
  detector_batch_set_psd_3       (detector_min_feature, detector_max_feature);
  detector_batch_set_d_max_c     (detector_zero_feature);
  epoch_type dummy_pattern;
  for (int i = 0; i < SAMPLES_PER_EPOCH; ++i) {
    dummy_pattern[i] = detector_convert_int_to_sample(i);
  }
  for (int i = 0; i < PATTERN_COUNT; ++i) {
    detector_batch_set_pattern(&dummy_pattern, i);
  }
}

static double get_time () {
  struct timeval data;
  gettimeofday(&data, NULL);
  return ((double)data.tv_sec) + ((double)data.tv_usec) / 1000000.0;
}

static void time_it (const char *name, double max_time, void (* func) ()) {
  double elapsed = 0;
  long count = 0;
  json_list_field(name);
  do {
    double start = get_time();
    func();
    double stop = get_time();
    count++;
    json_list_append_double(stop - start);
    elapsed += stop - start;
  } while (elapsed < max_time);
  json_end_list_field();
}

static void benchmark_max_distance (void) {
  epoch_type epoch = {0};
  detector_max_distance(&epoch);
}

static void benchmark_energy (void) {
  epoch_type epoch = {0};
  detector_energy(&epoch);
}

static void benchmark_dtw (void) {
  epoch_type epoch = {0};
  detector_dtw(&epoch, 0);
}

static void benchmark_psds (void) {
  epoch_type epoch = {0};
  feature_type psd1, psd2, psd3;
  detector_psds(&epoch, &psd1, &psd2, &psd3);
}

static void benchmark_simple (double max_time) {
  time_it("max_distance", max_time, benchmark_max_distance);
  time_it("energy", max_time, benchmark_energy);
  time_it("dtw", max_time, benchmark_dtw);
  time_it("psds", max_time, benchmark_psds);
}

static void benchmark_all (int iterations) {
  bool result;
  double start, stop;
  epoch_type epoch;

  json_list_field("detector");
  for (int i = 0; i < iterations; i++) {
    start = get_time();
    detector_is_seizure(&epoch, &result);
    stop = get_time();
    json_list_append_double(stop - start);
  }
  json_end_list_field();
}
