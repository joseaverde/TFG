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
#include <time.h>

static void json_int_field (const char *name, int value);
static void json_str_field (const char *name, const char *value);

static void dump_configuration (void);
static void configure_batch (void);

/* *** ==== Main function ============================================== *** */

_Noreturn void detector_benchmark (void) {
  dump_configuration();
  configure_batch();
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
  detector_batch_set_d_max_c     (detector_min_feature, detector_min_feature);
  epoch_type dummy_pattern;
  for (int i = 0; i < SAMPLES_PER_EPOCH; ++i) {
    dummy_pattern[i] = detector_convert_int_to_sample(i);
  }
  for (int i = 0; i < PATTERN_COUNT; ++i) {
    detector_batch_set_pattern(&dummy_pattern, i);
  }
}
