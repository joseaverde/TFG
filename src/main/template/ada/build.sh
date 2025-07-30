export DETECTOR_LANGUAGE=Ada
export DETECTOR_EXECUTABLE=benchmark
export DETECTOR_SAMPLE_SIZE=32
export DETECTOR_FEATURE_SIZE=64
export SAMPLES_PER_STRIDE=256
export STRIDES_PER_EPOCH=5
export PATTERN_COUNT=3
export DETECTOR_REAL="fixed"
export DETECTOR_TARGET="esp32c6"
export DETECTOR_PROFILE="ReleaseWithoutChecks"

TARGET="esp32c6"

# [ -d build ] && rm -r build
cmake -B build -GNinja \
   -DCMAKE_TOOLCHAIN_FILE="$IDF_PATH/tools/cmake/toolchain-${TARGET}.cmake"  \
   -DTARGET=esp32c3 || exit -1
cmake --build build
