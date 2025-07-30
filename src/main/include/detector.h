/*---------------------------------------------------------------------------*\
 *  Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved  *
 *---------------------------------------------------------------------------*
 * File:    detector.h                                                       *
 * Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>          *
 * License: European Union Public License 1.2                                *
\*---------------------------------------------------------------------------*/

#ifndef DETECTOR_H
#define DETECTOR_H

#include <stdint.h>
#include <stdbool.h>

/* *** ==== DETECTOR_SAMPLE_SIZE ======================================= *** */

#if not defined (DETECTOR_SAMPLE_SIZE)
# error "DETECTOR_SAMPLE_SIZE is undefined"
#elif DETECTOR_SAMPLE_SIZE == 32
  typedef int32_t sample_type;
#elif DETECTOR_SAMPLE_SIZE == 64
  typedef int64_t sample_type;
#else
# error "DETECTOR_SAMPLE_SIZE must be either 32 or 64 bits wide"
#endif

/* *** ==== DETECTOR_FEATURE_SZIE ====================================== *** */

#if not defined (DETECTOR_FEATURE_SIZE)
# error "DETECTOR_FEATURE_SIZE is undefined"
#elif DETECTOR_FEATURE_SIZE == 32
  typedef int32_t feature_type;
#elif DETECTOR_FEATURE_SIZE == 64
  typedef int64_t feature_type;
#else
# error "DETECTOR_FEATURE_SIZE must be either 32 or 64 bits wide"
#endif

/* *** ==== SAMPLES_PER_STRIDE ========================================= *** */

#define MAX_SAMPLES_PER_STRIDE 1024
#ifndef SAMPLES_PER_STRIDE
# error "SAMPLES_PER_STRIDE is undefined"
#elif SAMPLES_PER_STRIDE <= 0
# error "SAMPLES_PER_STRIDE must be a positive integer"
#elif SAMPLES_PER_STRIDE > MAX_SAMPLES_PER_STRIDE
# error "SAMPLES_PER_STRIDE must be between 1 and " #MAX_SAMPLES_PER_STRIDE
#endif

typedef sample_type stride_type[SAMPLES_PER_STRIDE];

/* *** ==== STRIDES_PER_EPOCH ========================================== *** */

#define MAX_STRIDES_PER_EPOCH 16
#ifndef STRIDES_PER_EPOCH
# error "STRIDES_PER_EPOCH is undefined"
#elif SAMPLES_PER_STRIDE <= 0
# error "STRIDES_PER_EPOCH must be a positive integer"
#elif STRIDES_PER_EPOCH > MAX_STRIDES_PER_EPOCH
# error "STRIDES_PER_EPOCH must be between 1 and " #MAX_STRIDES_PER_EPOCH
#endif

#define SAMPLES_PER_EPOCH (STRIDES_PER_EPOCH * SAMPLES_PER_STRIDE)

typedef sample_type epoch_type[SAMPLES_PER_EPOCH];

/* *** ==== PATTERN_COUNT ============================================== *** */

#define MAX_PATTERN_COUNT
#ifndef PATTERN_COUNT
# error "PATTERN_COUNT is undefined"
#elif PATTERN_COUNT <= 0
#error "PATTERN_COUNT must be a positive integer"
#elif PATTERN_COUNT > MAX_PATTERN_COUNT
# error "PATTERN_COUNT must be between 1 and " #MAX_PATTERN_COUNT
#endif

/* *** ==== API ======================================================== *** */

#ifdef __cplusplus
extern "C" {
#endif

  /* Configuration */

  extern const char *detector_language;
  extern const char *detector_compiler;
  extern const char *detector_profile;
  extern const char *detector_real_type;
  extern const char *detector_target;

  /* Sample conversion functions */

  sample_type detector_convert_double_to_sample (double item);
  sample_type detector_convert_int_to_sample    (int item);
  double      detector_convert_sample_to_double (sample_type item);
  extern const sample_t detector_max_sample;
  extern const sample_t detector_min_sample;

  /* Feature conversion functions */

  feature_type detector_convert_double_to_feature (double item);
  double       detector_convert_feature_to_double (feature_type item);
  extern const sample_t detector_max_feature;
  extern const sample_t detector_min_feature;

  /* Batch configuration functions */

  void detector_batch_set_max_distance (feature_type min, feature_type max);
  void detector_batch_set_energy       (feature_type min, feature_type max);
  void detector_batch_set_psd_1        (feature_type min, feature_type max);
  void detector_batch_set_psd_2        (feature_type min, feature_type max);
  void detector_batch_set_psd_3        (feature_type min, feature_type max);
  void detector_batch_set_d_max_c      (feature_type d_th);
  void detector_batch_set_pattern      (epoch_type *pattern, int index);
  void detector_batch_reset            (void);

  /* Detection functions */

  void detector_is_seizure (epoch_type *epoch, bool *result);

  /* Feature functions */

  feature_type detector_max_distance (epoch_type *epoch);
  feature_type detector_energy       (epoch_type *epoch);
  feature_type detector_dtw          (epoch_type *epoch, int index);
  void         detector_psds         (epoch_type *epoch,  feature_type *psd1,
                                      feature_type *psd2, feature_type *psd3);

  /* Benchmarking functions */

#ifdef __cplusplus
}
#endif

#endif//DETECTOR_H
