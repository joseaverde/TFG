#pragma once

#include <cstddef>

using Real = float;
#define REAL_NAME "float"

constexpr std::size_t stride_size = 256;
constexpr std::size_t warping_window = 16;
constexpr std::size_t strides_per_epoch = 5;
constexpr std::size_t epoch_size = stride_size * strides_per_epoch;
